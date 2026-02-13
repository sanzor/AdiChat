#!/bin/bash

# Change to the directory containing the Kubernetes YAML files
# Change to the directory containing the Kubernetes YAML files
cd ../postgres

# Apply Kubernetes configurations using kubectl with insecure skip TLS verification
kubectl apply -f postgres-configmap.yaml --insecure-skip-tls-verify
kubectl apply -f postgres-storage.yaml --insecure-skip-tls-verify
kubectl apply -f postgres-deployment.yaml --insecure-skip-tls-verify
kubectl apply -f postgres-service.yaml --insecure-skip-tls-verify


# for pgadmin (port) 
kubectl get services