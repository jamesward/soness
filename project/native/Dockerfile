# The image `nativeImage` builds soness's native executable in (see project/NativeImage.scala): GraalVM Community's
# native-image, and UPX to compress what it builds. Built for the host's architecture (amd64 or arm64).
FROM oraclelinux:10-slim

ARG TARGETARCH
# GraalVM Community's release and the JDK it is built on (github.com/graalvm/graalvm-ce-builds/releases); the
# ghcr.io/graalvm/native-image-community image lags the releases by months
ARG GRAALVM_TAG=graal-25.4.4.1.1
ARG GRAALVM_JDK=25.0.4.1.1
ARG UPX_VERSION=5.3.0

# what native-image links with: gcc and the static libraries (the latter are in CodeReady Builder, off by default)
RUN microdnf install -y --enablerepo=ol10_codeready_builder gcc glibc-static zlib-ng-compat-static libstdc++-static \
      tar gzip xz findutils \
 && microdnf clean all

# GraalVM, checked against the SHA-256 its release publishes
RUN set -eu; \
    case "$TARGETARCH" in amd64) arch=x64 ;; arm64) arch=aarch64 ;; *) echo "unsupported $TARGETARCH" >&2; exit 1 ;; esac; \
    file="graalvm-community-jdk-25i4-${GRAALVM_JDK}_linux-${arch}_bin.tar.gz"; \
    base="https://github.com/graalvm/graalvm-ce-builds/releases/download/${GRAALVM_TAG}"; \
    curl -fsSL -o "/tmp/$file" "$base/$file"; \
    echo "$(curl -fsSL "$base/$file.sha256")  /tmp/$file" | sha256sum -c -; \
    mkdir /opt/graalvm && tar -xzf "/tmp/$file" -C /opt/graalvm --strip-components=1 && rm "/tmp/$file"
ENV JAVA_HOME=/opt/graalvm
ENV PATH=/opt/graalvm/bin:$PATH

RUN set -eu; \
    curl -fsSL "https://github.com/upx/upx/releases/download/v${UPX_VERSION}/upx-${UPX_VERSION}-${TARGETARCH}_linux.tar.xz" \
  | tar -xJ -C /usr/local/bin --strip-components=1 "upx-${UPX_VERSION}-${TARGETARCH}_linux/upx"; \
    upx --version | head -1; native-image --version | head -1

ENTRYPOINT []
