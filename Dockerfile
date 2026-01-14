FROM flowcommerce/play_builder_java17:latest as builder
ADD . /opt/play
WORKDIR /opt/play
RUN sbt 'project generator' clean stage

FROM flowcommerce/play_java17:latest
COPY --from=builder /opt/play /opt/play
WORKDIR /opt/play/generator/target/universal/stage
ENTRYPOINT ["java", "-jar", "/root/environment-provider.jar", "--service", "play", "apibuilder-generator", "bin/apibuilder-generator-generator"]
HEALTHCHECK --interval=5s --timeout=5s --retries=10 \
  CMD curl -f http://localhost:9000/_internal_/healthcheck || exit 1
