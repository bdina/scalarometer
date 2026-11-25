# CI and optional local use: GraalVM native-image + Gradle (matches ./scripts/docker-build + `gradle` workflow).
# Build: docker build -t graalvm-dev:latest -f docker/ci-toolchain.Dockerfile .
FROM ghcr.io/graalvm/native-image-community:25
RUN microdnf install -y curl-minimal findutils gzip tar unzip which \
  && microdnf clean all
ENV JAVA_TOOL_OPTIONS="--sun-misc-unsafe-memory-access=allow --enable-native-access=ALL-UNNAMED"
ARG GRADLE_VERSION=9.9.2
RUN curl -fsSL "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" -o /tmp/gradle.zip \
  && unzip -q /tmp/gradle.zip -d /opt \
  && rm /tmp/gradle.zip \
  && ln -sf "/opt/gradle-${GRADLE_VERSION}/bin/gradle" /usr/local/bin/gradle
WORKDIR /workspace
# Base image sets ENTRYPOINT to native-image; clear it so `docker run ... bash -lc 'gradle ...'` matches ./scripts/docker-build.
ENTRYPOINT []
