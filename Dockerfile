FROM openjdk:11
COPY ./target/uberjar/connect4-0.1.0-SNAPSHOT-standalone.jar /usr/app/
WORKDIR /usr/app
ENTRYPOINT [ "java", "--illegal-access=deny", "-jar", "connect4-0.1.0-SNAPSHOT-standalone.jar" ]
