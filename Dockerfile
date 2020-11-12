FROM erlang:23-alpine

# src/Makefile expects parent directory to be called yanger
ARG YANGER_LOCATION=/yanger
ENV PATH="${YANGER_LOCATION}/bin:${PATH}"
ENV YANG_MODPATH="${YANGER_LOCATION}/modules"

WORKDIR ${YANGER_LOCATION}
COPY ./ ${YANGER_LOCATION}

# -virtual used to remove build deps as part of single layer, keep image small
RUN set -xe \
    && apk add --no-cache coreutils libxml2-dev \
    && apk add --no-cache --virtual build-dependencies make gcc libc-dev \
    && make clean all \
    && apk del build-dependencies

# reset workdir, user should mount their local files to this directory
WORKDIR /workdir

ENTRYPOINT ["yanger"]
CMD ["-h"]