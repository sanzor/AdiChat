#!/bin/bash

# Define the PostgreSQL username and password
PGUSER="admin"
PGPASSWORD="test123"

# Define the SQL script path
SQL_SCRIPT_PATH="/home/sanzor/NovaWebsocketServer/resources/tables.pgsql"

# Get the name of the PostgreSQL pod
POSTGRES_POD=$(kubectl get pods -l app=postgres -o jsonpath='{.items[0].metadata.name}')

# Use cat to insert the SQL script content into psql in the PostgreSQL pod
cat "$SQL_SCRIPT_PATH" | sudo -S kubectl exec -it "$POSTGRES_POD" -- psql -U "$PGUSER" -d postgresdb

# Exit the script
exit
