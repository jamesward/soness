# easyracer-java

An [Easy Racer](https://github.com/jamesward/easyracer) client in Java 25: each scenario (1, 2 and 4, so far) is a method of
`Main.Scenarios`, returning the winning response's body (`right` when the scenario is done correctly).
`MainTest` runs each scenario against the scenario server (`ghcr.io/jamesward/easyracer`, started with Testcontainers;
Docker must be running).

Build and test with the Gradle wrapper: `./gradlew test`, or one scenario: `./gradlew test --tests 'MainTest.scenario2'`.
