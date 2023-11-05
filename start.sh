#!/bin/bash

# Get the PostgreSQL username and password
PGUSER="admin"
PGPASSWORD="test123"

# Your SQL script path
SQL_SCRIPT_PATH="/home/sanzor/NovaWebsocketServer/resources/tables.pgsql"

# Define the Kubernetes context you want to use
KUBE_CONTEXT="docker-desktop"

# Get the current username
CURRENT_USER=$(whoami)

# Get the PostgreSQL pod name
POSTGRES_POD=$(kubectl get pods -l app=postgres -o jsonpath='{.items[0].metadata.name}' --context="$KUBE_CONTEXT")

# Change working directory to where the SQL script is located
cd "$(dirname "$SQL_SCRIPT_PATH")"

# Copy the SQL script to the PostgreSQL pod
kubectl cp "$(basename "$SQL_SCRIPT_PATH")" "$POSTGRES_POD:$(basename "$SQL_SCRIPT_PATH")" --context="$KUBE_CONTEXT"

# Change back to the original working directory
cd -

# Use echo to provide the password to sudo
echo "$PGPASSWORD" | sudo -S -u "$CURRENT_USER" kubectl exec -i "$POSTGRES_POD" -- psql -U "$PGUSER" -d postgresdb -f "$(basename "$SQL_SCRIPT_PATH")"

echo "Starting Erlang application..."
rebar3 nova serve 2>&1
start_result=$?
exit
