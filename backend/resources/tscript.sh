 QUERY=$(cat /home/sanzor/NovaWebsocketServer/resources/table.pgsql)
 kubectl exec -it postgres-7b9fb8d6c5-5s85s -- psql -U admin -d postgresdb -c "$QUERY"
