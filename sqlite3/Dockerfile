FROM ubuntu:xenial
MAINTAINER Giordon Stark "kratsg@gmail.com"
LABEL org.label-schema.vcs-url="https://github.com/kratsg/dockerimages/tree/master/sqlite3"
LABEL version="latest"

RUN apt-get update && apt-get -yq install make sqlite3 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
