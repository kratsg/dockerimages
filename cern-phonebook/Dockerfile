ARG BASE_IMAGE=registry.access.redhat.com/ubi8/python-39
FROM ${BASE_IMAGE} as builder
USER root
WORKDIR /code/

RUN dnf clean expire-cache && \
    dnf install -y golang && \
    go install github.com/msoap/shell2http@latest && \
    cp $(go env GOPATH)/bin/shell2http .

FROM ${BASE_IMAGE}
USER root
WORKDIR /app
COPY --from=builder /code/shell2http .

COPY entrypoint.sh /entrypoint.sh
COPY pyphonebook pyphonebook/
COPY lookup.sh .

RUN dnf clean expire-cache && \
    dnf install -y cronie jq && \
    dnf clean all && \
    rm -rf /var/cache/dnf && \
    python -m venv venv && \
    ./venv/bin/python -m pip install -U pip && \
    ./venv/bin/python -m pip install ldap3 && \
    ./venv/bin/python -m pip install ./pyphonebook

EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
CMD ["-form", "-cgi", \
     "GET:/lookup", "./lookup.sh"]
