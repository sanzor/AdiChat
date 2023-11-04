#!/bin/bash

# Define the PostgreSQL username and password
PGUSER="admin"
PGPASSWORD="test123"

# Define the SQL script path
SQL_SCRIPT_PATH="/home/sanzor/NovaWebsocketServer/resources/tables.pgsql"

# Define the Kubernetes context you want to use
KUBE_CONTEXT="docker-desktop"

# Change the working directory to the location of the SQL script
cd "$(dirname "$SQL_SCRIPT_PATH")"

# Get the name of the PostgreSQL pod
POSTGRES_POD=$(kubectl get pods -l app=postgres -o jsonpath='{.items[0].metadata.name}' --context="$KUBE_CONTEXT")

# Use kubectl cp to copy the SQL script to the PostgreSQL pod
kubectl cp "$(basename "$SQL_SCRIPT_PATH")" "$POSTGRES_POD:$(basename "$SQL_SCRIPT_PATH")" --context="$KUBE_CONTEXT"

# Change back to the original working directory
cd -

# Connect to the PostgreSQL pod and run the SQL script using sudo -S to provide the password
echo "$PGPASSWORD" | sudo -S kubectl exec -i "$POSTGRES_POD" -- psql -U "$PGUSER" -d postgresdb -f "$(basename "$SQL_SCRIPT_PATH")"

# Exit the script
exit
