ARG BASE_IMAGE=perl
FROM ${BASE_IMAGE} as builder
WORKDIR /code/

RUN apt-get update && \
    apt-get install -y golang && \
    go install github.com/msoap/shell2http@latest && \
    cp $(go env GOPATH)/bin/shell2http .

FROM ${BASE_IMAGE}
WORKDIR /app
COPY --from=builder /code/shell2http .

RUN apt-get clean autoclean && \
    apt-get autoremove --yes && \
    rm -rf /var/lib/{apt,dpkg,cache,log}/ && \
    cpanm install Date::Manip Net::LDAP && \
    rm -rf /root/.cpanm

COPY entrypoint.sh /entrypoint.sh
COPY phonebook /bin/phonebook
COPY lookup.sh .

EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
CMD ["-form", "-cgi", \
     "GET:/lookup", "./lookup.sh"]
