# The image `nativeImage` builds soness's native executable in (see build.sbt): GraalVM's native-image, and UPX to
# compress what it builds. Built for the host's architecture (amd64 or arm64).
FROM ghcr.io/graalvm/native-image-community:25.0.2

ARG TARGETARCH
ARG UPX_VERSION=5.3.0

RUN microdnf install -y xz && microdnf clean all \
 && curl -fsSL "https://github.com/upx/upx/releases/download/v${UPX_VERSION}/upx-${UPX_VERSION}-${TARGETARCH}_linux.tar.xz" \
  | tar -xJ -C /usr/local/bin --strip-components=1 "upx-${UPX_VERSION}-${TARGETARCH}_linux/upx" \
 && upx --version | head -1

ENTRYPOINT []
