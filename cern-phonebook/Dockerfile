ARG BASE_IMAGE=python:3
FROM ${BASE_IMAGE} as builder
WORKDIR /code/

RUN apt-get update && \
    apt-get install -y golang && \
    go install github.com/msoap/shell2http@latest && \
    cp $(go env GOPATH)/bin/shell2http .

FROM ${BASE_IMAGE}
WORKDIR /app
COPY --from=builder /code/shell2http .

COPY entrypoint.sh /entrypoint.sh
COPY pyphonebook pyphonebook/
COPY lookup.sh .

RUN apt-get update && \
    apt-get clean autoclean && \
    apt-get autoremove --yes && \
    rm -rf /var/lib/{apt,dpkg,cache,log}/ && \
    python -m venv venv && \
    ./venv/bin/python -m pip install -U pip && \
    ./venv/bin/python -m pip install ldap3 && \
    ./venv/bin/python -m pip install ./pyphonebook

EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
CMD ["-form", "-cgi", \
     "GET:/lookup", "./lookup.sh"]
