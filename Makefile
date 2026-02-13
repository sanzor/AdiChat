COMPOSE ?= docker compose
PROJECT ?= adichat

BACKEND_COMPOSE := yml-compose/backend.yml
FRONTEND_COMPOSE := yml-compose/frontend.yml

.PHONY: up up-backend up-frontend down down-backend down-frontend ps logs-backend logs-frontend
.PHONY: dev-backend dev-frontend up-postgres down-postgres

up:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) -f $(FRONTEND_COMPOSE) up -d --build

down:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) -f $(FRONTEND_COMPOSE) down --remove-orphans

up-backend:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) up -d --build

up-frontend:
	$(COMPOSE) -p $(PROJECT) -f $(FRONTEND_COMPOSE) up -d --build

up-postgres:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) up -d postgres

down-backend:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) stop backend postgres || true
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) rm -f backend postgres || true

down-frontend:
	$(COMPOSE) -p $(PROJECT) -f $(FRONTEND_COMPOSE) stop frontend || true
	$(COMPOSE) -p $(PROJECT) -f $(FRONTEND_COMPOSE) rm -f frontend || true

down-postgres:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) stop postgres || true
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) rm -f postgres || true

dev-backend:
	cd backend && rebar3 nova serve

dev-frontend:
	cd frontend && pnpm dev

ps:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) -f $(FRONTEND_COMPOSE) ps

logs-backend:
	$(COMPOSE) -p $(PROJECT) -f $(BACKEND_COMPOSE) logs -f backend

logs-frontend:
	$(COMPOSE) -p $(PROJECT) -f $(FRONTEND_COMPOSE) logs -f frontend
