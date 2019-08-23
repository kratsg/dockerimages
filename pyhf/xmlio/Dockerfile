FROM python:3.7-alpine

RUN apk add jq && \
    pip install pyhf[xmlio]

WORKDIR /home/atlas
USER atlas
