FROM python:3.7-alpine

RUN apk add --no-cache \
        --virtual=.build-dependencies \
        g++ gfortran file binutils \
        musl-dev python3-dev cython openblas-dev && \
    apk add libstdc++ openblas && \
    apk add jq && \
    pip install pyhf[xmlio]

WORKDIR /home/root
