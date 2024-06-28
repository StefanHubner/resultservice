#!/bin/bash

# change into app
cd /app/

# Start PostgreSQL
service postgresql start

# Wait for the Postgres server to become available.
echo "Waiting for postgres..."
while ! nc -z 0.0.0.0 5432; do
  sleep 0.1
done
echo "PostgreSQL started"

# Creating Database and user
sudo -u postgres psql << EOF
DROP DATABASE IF EXISTS ds;
CREATE DATABASE ds;
CREATE USER stefan WITH PASSWORD 'podersdorf';
CREATE USER ubuntu;
GRANT ALL PRIVILEGES ON DATABASE ds TO stefan;
GRANT ALL PRIVILEGES ON SCHEMA public TO stefan;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO stefan;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO stefan;
GRANT ALL ON DATABASE ds TO stefan;
GRANT USAGE, CREATE ON SCHEMA PUBLIC TO stefan;
ALTER DATABASE ds OWNER TO stefan;
ALTER ROLE stefan SET client_encoding TO 'utf8';
ALTER ROLE stefan SET default_transaction_isolation TO 'read committed';
ALTER ROLE stefan SET timezone TO 'UTC';
EOF

echo "Loading data from s3"
chmod -R 777 /app/static/
aws s3 --region eu-west-1 --profile s3hubnerinfo cp  s3://resultservice-backup/ /app/static/ --recursive --include "*"

sudo -u postgres psql -d ds -f /app/static/sql_backup/daily.backup.sql
echo "Data load complete"

echo "Setting up backup..."
echo "0 */1 * * * /usr/local/bin/backup.sh" | crontab -
cron

echo "Testing PostgreSQL connection... (using .pgpass credentials)"
psql -h localhost -U stefan -d ds -c '\q'

if [ $? -eq 0 ]; then
  echo "PostgreSQL connection successful."
else
  echo "PostgreSQL connection failed."
  exit 1
fi

# Start server
echo "Starting server..."
exec resultservice-exe
