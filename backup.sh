#!/bin/bash

sudo -u postgres pg_dump ds > /app/static/sql_backup/daily.backup.sql
aws s3 --region eu-west-1 --profile s3hubnerinfo cp /app/static/ s3://resultservice-backup/ --recursive --include "*" --exclude "smHmip*
