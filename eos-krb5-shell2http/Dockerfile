ARG BASE_IMAGE=kratsg/eos-krb5
FROM ${BASE_IMAGE} as builder
WORKDIR /code/

RUN yum clean expire-cache && \
    yum install -y golang && \
    go install github.com/msoap/shell2http@latest && \
    cp $(go env GOPATH)/bin/shell2http .

FROM ${BASE_IMAGE}
WORKDIR /app
COPY --from=builder /code/shell2http .

COPY entrypoint.sh /entrypoint.sh
COPY generate-token.sh .

EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
CMD ["-form", "-cgi", \
     "/generate-token", "./generate-token.sh", "/404", "printf 'Status: 404\n'; echo; echo '404 page'"]
