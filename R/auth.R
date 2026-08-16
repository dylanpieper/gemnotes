box::use(
  jsonlite[fromJSON],
  jose[jwt_split, jwt_decode_sig, read_jwk],
)

GOOGLE_CERTS_URL <- "https://www.googleapis.com/oauth2/v3/certs"
GOOGLE_ISSUERS <- c("accounts.google.com", "https://accounts.google.com")

# In-memory cache for Google's signing keys (module-level env, shared across
# sessions in this R process) -- avoids a network round trip on every login.
# Google rotates these infrequently; re-fetching on a cache miss (e.g. a kid
# we haven't seen) is enough to pick up rotation without a fixed TTL.
jwks_cache <- new.env(parent = emptyenv())

fetch_jwks <- function() {
  fromJSON(GOOGLE_CERTS_URL, simplifyVector = FALSE)$keys
}

find_key <- function(kid) {
  keys <- jwks_cache$keys
  matched <- if (!is.null(keys)) Find(function(k) identical(k$kid, kid), keys) else NULL
  if (is.null(matched)) {
    keys <- fetch_jwks()
    jwks_cache$keys <- keys
    matched <- Find(function(k) identical(k$kid, kid), keys)
  }
  matched
}

#' Verify a Google Identity Services ID token and return the signed-in
#' identity, or NULL if the token is missing, expired, mis-signed, or not
#' issued for this app.
#' @param id_token The JWT credential from the GSI button's callback
#' @param client_id This app's Google OAuth Client ID (config.yml google_client_id)
#' @return Named list (sub, email, email_verified, name, picture) or NULL
#' @export
verify_google_id_token <- function(id_token, client_id) {
  tryCatch(
    {
      header <- jwt_split(id_token)$header
      jwk <- find_key(header$kid)
      if (is.null(jwk)) {
        return(NULL)
      }

      claims <- jwt_decode_sig(id_token, read_jwk(jwk))

      if (!identical(claims$aud, client_id)) {
        return(NULL)
      }
      if (!(claims$iss %in% GOOGLE_ISSUERS)) {
        return(NULL)
      }

      list(
        sub = claims$sub,
        email = claims$email,
        email_verified = isTRUE(claims$email_verified),
        name = claims$name,
        picture = claims$picture
      )
    },
    error = function(e) NULL
  )
}
