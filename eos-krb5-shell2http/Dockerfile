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

COPY kinit-cron /etc/cron.d/kinit-cron

RUN yum clean expire-cache && \
    yum install -y cronie jq && \
    yum clean all && \
    rm -rf /var/cache/yum && \
    chmod 0644 /etc/cron.d/kinit-cron && \
    touch /var/log/cron.log

COPY entrypoint.sh /entrypoint.sh
COPY generate-token.sh .
COPY ls.sh .
COPY mkdir.sh .
COPY rmdir.sh .

EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
CMD ["-form", "-cgi", \
     "POST:/generate-token", "./generate-token.sh", \
     "/log", "cat /var/log/cron.log", \
     "/klist", "/usr/bin/klist || true", \
     "GET:/path", "./ls.sh", \
     "PUT:/path", "./mkdir.sh", \
     "DELETE:/path", "./rmdir.sh"]
