all:
	stack build

docker:
	docker build -t resultservice-backend .

run: 
	docker run -d -p 8080:8080 resultservice-backend

auth:
	aws ecr get-login-password --region eu-west-1 --profile hubner | docker login --username AWS --password-stdin 904033610562.dkr.ecr.eu-west-1.amazonaws.com/resultservice-backend

deploy:
	docker tag resultservice-backend:latest 904033610562.dkr.ecr.eu-west-1.amazonaws.com/resultservice-backend
	docker push 904033610562.dkr.ecr.eu-west-1.amazonaws.com/resultservice-backend
