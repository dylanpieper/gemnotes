# gemnotes

A multi-user Shiny app for tracking clinical licensure hours with Google SSO. Each therapist gets a private account.

## Story

I built this app for my wife, Julia ❤️, so she could track her hours toward licensure. She has to log thousands of hours before she can practice independently across categories like individual and relational sessions, supervision, and administrative work. This is the most important app I've ever built, because it's for the most important person to me. Building her tools is one of my love languages.

## Dear Therapists

If you want to use this app to track your own hours toward licensure, go to [gemnotes](https://dylanpieper-gemnotes.share.connect.posit.cloud/) and sign in with your Google account. Your data is private to you, and you can delete your account and data at any time. Questions? Email me at dylanpieper@gmail.com.

## Requirements

- Use R version 4.3 or a later version.
- Use [renv](https://rstudio.github.io/renv/). Run `renv::restore()` to install the R packages.
- Create a Supabase (Postgres) project.

## Setup

### 1. Supabase

Create a Supabase account and a project at [supabase.com](https://supabase.com/). Go to the SQL editor. Run this SQL command:

```sql
create extension if not exists pgcrypto;

create table public.users (
  id uuid primary key default gen_random_uuid(),
  google_sub text not null unique,
  email text not null,
  name text,
  total_hours_goal integer not null default 4000,
  therapy_hours_goal integer not null default 1000,
  relational_hours_goal integer not null default 500,
  supervision_individual_goal integer not null default 100,
  supervision_group_goal integer not null default 100,
  admin_hours_goal integer not null default 2800,
  policy_accepted_at timestamptz,
  created_at timestamptz not null default now()
);

alter table public.users enable row level security;

create policy users_select_own on public.users
  for select using (
    id = nullif(current_setting('app.user_id', true), '')::uuid
    or google_sub = nullif(current_setting('app.pending_sub', true), '')
  );

create policy users_insert_self on public.users
  for insert with check (
    google_sub = nullif(current_setting('app.pending_sub', true), '')
  );

create policy users_update_own on public.users
  for update using (
    id = nullif(current_setting('app.user_id', true), '')::uuid
  )
  with check (
    id = nullif(current_setting('app.user_id', true), '')::uuid
  );

create policy users_delete_own on public.users
  for delete using (
    id = nullif(current_setting('app.user_id', true), '')::uuid
  );

create table public.hours (
  id bigserial primary key,
  user_id uuid not null references public.users(id) on delete cascade
    default current_setting('app.user_id', true)::uuid,
  start_date date not null,
  individual numeric(4, 1) not null default 0,
  relational_couple numeric(4, 1) not null default 0,
  relational_family numeric(4, 1) not null default 0,
  supervision_individual numeric(4, 1) not null default 0,
  supervision_group numeric(4, 1) not null default 0,
  consultation numeric(4, 1) not null default 0,
  case_notes numeric(4, 1) not null default 0,
  session_plan numeric(4, 1) not null default 0,
  emails numeric(4, 1) not null default 0,
  letters numeric(4, 1) not null default 0,
  staff_meetings numeric(4, 1) not null default 0,
  cont_ed numeric(4, 1) not null default 0,
  exam_prep numeric(4, 1) not null default 0,
  travel numeric(4, 1) not null default 0,
  shopping numeric(4, 1) not null default 0,
  other numeric(4, 1) not null default 0,
  constraint weekly_therapy_hours_case_notes_check check ((case_notes >= (0)::numeric)),
  constraint weekly_therapy_hours_consultation_check check ((consultation >= (0)::numeric)),
  constraint weekly_therapy_hours_cont_ed_check check ((cont_ed >= (0)::numeric)),
  constraint weekly_therapy_hours_emails_check check ((emails >= (0)::numeric)),
  constraint weekly_therapy_hours_exam_prep_check check ((exam_prep >= (0)::numeric)),
  constraint weekly_therapy_hours_individual_check check ((individual >= (0)::numeric)),
  constraint weekly_therapy_hours_letters_check check ((letters >= (0)::numeric)),
  constraint weekly_therapy_hours_other_check check ((other >= (0)::numeric)),
  constraint weekly_therapy_hours_relational_couple_check check ((relational_couple >= (0)::numeric)),
  constraint weekly_therapy_hours_relational_family_check check ((relational_family >= (0)::numeric)),
  constraint weekly_therapy_hours_session_plan_check check ((session_plan >= (0)::numeric)),
  constraint weekly_therapy_hours_shopping_check check ((shopping >= (0)::numeric)),
  constraint weekly_therapy_hours_staff_meetings_check check ((staff_meetings >= (0)::numeric)),
  constraint weekly_therapy_hours_supervision_group_check check ((supervision_group >= (0)::numeric)),
  constraint weekly_therapy_hours_supervision_individual_check check ((supervision_individual >= (0)::numeric)),
  constraint weekly_therapy_hours_travel_check check ((travel >= (0)::numeric))
) TABLESPACE pg_default;

alter table public.hours enable row level security;

create policy hours_user_scoped on public.hours
  using (user_id = nullif(current_setting('app.user_id', true), '')::uuid)
  with check (user_id = nullif(current_setting('app.user_id', true), '')::uuid);
```

If you already created the `hours` table before this app had `travel`, `shopping`, and `other` categories, run this instead of recreating the table:

```sql
alter table public.hours
  add column travel numeric(4, 1) not null default 0,
  add column shopping numeric(4, 1) not null default 0,
  add column other numeric(4, 1) not null default 0,
  add constraint weekly_therapy_hours_travel_check check ((travel >= (0)::numeric)),
  add constraint weekly_therapy_hours_shopping_check check ((shopping >= (0)::numeric)),
  add constraint weekly_therapy_hours_other_check check ((other >= (0)::numeric));
```

The `nullif(..., '')` function is important here. Some connection poolers send an empty string for a session variable that is not set. They do not send a null value. A direct conversion of an empty string to a `uuid` value causes an error. A conversion of a null value to a `uuid` value does not cause an error. This method makes sure that a connection before sign-in gets zero rows, and not an error.

All accounts use the same `hours` table and the same `users` table. Postgres row-level security (RLS) keeps the rows of each user private. The app checks the ID token. Immediately after this check, the app sets a session variable named `app.pending_sub` on the database connection. This variable limits `users` table lookups and new-account inserts to one Google account, before a `user_id` value exists. After sign-in or account creation, the app sets a variable named `app.user_id`. This variable limits every other query, including queries on the `hours` table, to the rows of that user. Refer to `R/users.R` for the parameterized queries that operate on the `users` table.

Posit Connect Cloud does not support direct connections to the database. This app uses the Supabase session pooler instead. If you use Connect, do this procedure to get the connection data:

1. Go to **Project Settings → Database → Connect**.
2. Find the **Session pooler** area.
3. Get these four values: host, port, database name, and user.

Put these four values in `config.yml`. Use the keys `db_host`, `db_port`, `db_name`, and `db_user`. Put the password in the `GEM_SUPA_PASS` secret. Do not put the password in this file:

```yaml
db_host: "aws-0-us-east-2.pooler.supabase.com"
db_port: 5432
db_name: "postgres"
db_user: "postgres.<project-ref>"
```

**Necessary step: create a database role with limited rights.** Do not use `postgres.<project-ref>` as the `db_user` value. This role is a superuser role. Postgres does not apply RLS to a superuser role, and does not apply RLS to the owner of a table. If the app connects with the `postgres.<project-ref>` role, RLS has no effect. Then, every account can read and write the hours of every other account.

Create a different role. Run this SQL command in the Supabase SQL editor:

```sql
create role gemnotes_app with login password 'pick-a-strong-password';
grant usage on schema public to gemnotes_app;
grant select, insert, update, delete on public.hours, public.users to gemnotes_app;
grant usage, select on all sequences in schema public to gemnotes_app;
```

Set the `db_user` value to `gemnotes_app.<project-ref>` in `config.yml`. Use the same `<project-ref>` value from the connection data above. Put the password for this role in `GEM_SUPA_PASS`. Do not use the superuser password. The Supavisor pooler accepts a login role with the same host and the same port. This role works with the connection settings that you already have.

### 2. Google Sign-In

Do this procedure to create a Google Cloud OAuth client ID:

1. Go to [console.cloud.google.com](https://console.cloud.google.com/apis/credentials).
2. Select **APIs & Services → Credentials → Create Credentials → OAuth client ID**.
3. Select **Web application** as the application type.

- In the **Authorized JavaScript origins** area, add `http://127.0.0.1:PORT`. Use this address for local development. Use the port number that `shiny::runApp()` selects. Also add your deployment URL, for example your Posit Connect Cloud URL. Add the deployment URL now, if you know it. If you do not know it, add it after you deploy the app.
- Do not add a redirect URI. Do not use the client secret. The app checks only the signed ID token from Google. The app does not use a server-side OAuth exchange.

The client ID is not secret data. The page contains the client ID directly. Put the client ID in `config.yml`. Use the key `google_client_id`.

### 3. Config

The `config.yml` file contains the settings for the app:

```yaml
app_title: "Gem Notes"
table_name: "hours"
google_client_id: "<client-id>.apps.googleusercontent.com"
quotes:
  - "Your real quote here.<br>- Attribution"
```

The `table_name` value is the name of the Postgres table for hour entries. This file does not set licensure hour goals. Each user sets the goals of that user in the signup wizard, at the first sign-in. The app stores these goals in the `users` table. To remove a goal category, set the value of that category to `0`. Commit `config.yml` to the repository. Do not add material with copyright to `quotes`.

### 4. Secrets

This app uses one environment variable: `GEM_SUPA_PASS`. This variable is the password for the `db_user` value in `config.yml`. The function `RPostgres::Postgres()` uses this variable. Do not store this variable in a file.

For local development, do this procedure:

1. Run `usethis::edit_r_environ("project")` to create or open the `.Renviron` file in the project root directory. Git ignores this file.
2. Add this line to the file:

```
GEM_SUPA_PASS=db-password
```

3. Restart R.

## Running locally

```r
renv::restore()
shiny::runApp()
```

## Deploying to Posit Connect Cloud

1. Install [Posit Publisher](https://github.com/posit-dev/publisher). Positron includes Publisher. You can also use the VS Code extension or the CLI application.
2. Start a new deployment. Select **Connect Cloud** as the target. Publisher writes the file `.posit/publish/*.toml`. Confirm that `config.yml` is in the list of project files.
3. Declare `GEM_SUPA_PASS` as a secret in this configuration. Set the value when Publisher shows a prompt for it. Connect Cloud encrypts this value, and adds the value as an environment variable at runtime.
4. If Publisher does not show a prompt, deploy the app and let the deployment fail.
5. Go to [connect.posit.cloud](https://connect.posit.cloud/). Open **Settings → Variables**. Set the value there.
6. Add your deployed app URL to the Authorized JavaScript origins of the Google OAuth client ID. Refer to step 2 in the Google Sign-In section above.
7. Republish the app.
