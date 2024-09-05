FROM openjdk:11

RUN mkdir smart-cds

COPY target/smart-cds.jar smart-cds/smart-cds.jar
COPY bundle smart-cds/bundle
COPY definitions smart-cds/definitions
COPY valueset smart-cds/valueset
COPY src/main/resources/application-docker.conf smart-cds/application.conf

ENTRYPOINT [ "java", "-Xms512m", "-Xmx4g", "-jar", "-Dconfig.file=smart-cds/application.conf", "smart-cds/smart-cds.jar" ]
