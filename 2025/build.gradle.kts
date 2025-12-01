plugins {
    kotlin("jvm") version "2.2.20"
    id("application")
}

kotlin {
    jvmToolchain(21)
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

application {
    mainClass.set("org.example.MainKt")
}

tasks.test {
    useJUnitPlatform()
}