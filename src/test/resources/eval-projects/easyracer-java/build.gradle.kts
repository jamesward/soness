import java.time.Duration

plugins {
    application
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(25)
    }
}

dependencies {
    testImplementation("org.junit.jupiter:junit-jupiter:6.1.3")
    testImplementation("org.testcontainers:testcontainers:2.0.5")
    testImplementation("org.testcontainers:testcontainers-junit-jupiter:2.0.5")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:6.1.3")
    testRuntimeOnly("org.slf4j:slf4j-nop:2.0.17")
}

application {
    mainClass.set("Main")
}

tasks.withType<JavaCompile> {
    options.compilerArgs.add("--enable-preview")
}

tasks.withType<JavaExec> {
    jvmArgs("--enable-preview")
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
    // a run of a race done wrong (threads that never end) ends anyway: each test has 15 s, the whole run 3 minutes
    timeout.set(Duration.ofMinutes(3))
    // the scenario server the tests use: EASYRACER_URL's when set (one kept running), else a container per run
    System.getenv("EASYRACER_URL")?.let { environment("EASYRACER_URL", it) }
    jvmArgs("--enable-preview")

    testLogging {
        showStandardStreams = false
        showExceptions = true
        exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
        events(org.gradle.api.tasks.testing.logging.TestLogEvent.STARTED, org.gradle.api.tasks.testing.logging.TestLogEvent.PASSED, org.gradle.api.tasks.testing.logging.TestLogEvent.SKIPPED, org.gradle.api.tasks.testing.logging.TestLogEvent.FAILED)
    }
}
