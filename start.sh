#!/bin/bash

# Get the PostgreSQL username and password
PGUSER="admin"
PGPASSWORD="test123"

# Your SQL script path
SQL_SCRIPT_PATH="/home/sanzor/NovaWebsocketServer/resources/tables.pgsql"

# Define the Kubernetes context you want to use
KUBE_CONTEXT="docker-desktop"

# Get the PostgreSQL pod name
POSTGRES_POD=$(kubectl get pods -l app=postgres -o jsonpath='{.items[0].metadata.name}' --context="$KUBE_CONTEXT")

# Copy the SQL script to the PostgreSQL pod
kubectl cp "$SQL_SCRIPT_PATH" "$POSTGRES_POD:/tmp/tables.pgsql" --context="$KUBE_CONTEXT"

# Execute the SQL script inside the PostgreSQL pod
kubectl exec -i "$POSTGRES_POD" --context="$KUBE_CONTEXT" -- env PGPASSWORD="$PGPASSWORD" psql -U "$PGUSER" -d postgresdb -f /tmp/tables.pgsql

# Remove the SQL script from the PostgreSQL pod (optional)
kubectl exec -i "$POSTGRES_POD" --context="$KUBE_CONTEXT" -- rm /tmp/tables.pgsql

# Start the Erlang application
echo "Starting Erlang application..."
rebar3 nova serve 2>&1
start_result=$?
exit $start_result
