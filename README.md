# AdiChat

This workspace is split into two folders:

- `backend/`: Erlang/OTP server (rebar3 / NovaFramework)
- `frontend/`: React UI

## Dev (Docker Compose)

- `make up` / `make down`
- `make up-backend` / `make down-backend`
- `make up-frontend` / `make down-frontend`

Default ports: backend `8080`, frontend `5173`, postgres `5432`.

## Dev (Local, no Docker)

Prereqs: Erlang/OTP + `rebar3`, Node.js + `pnpm`, and a PostgreSQL instance (or run only Postgres via `make up-postgres`).

- Backend: `make dev-backend` (serves on `http://localhost:8080`)
- Frontend: `make dev-frontend` (serves on `http://localhost:5173`)
