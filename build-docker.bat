@echo off

call mvn clean install
docker build -t srdc/smart-cds -f ./docker/Dockerfile .
