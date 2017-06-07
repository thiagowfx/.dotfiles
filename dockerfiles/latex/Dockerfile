FROM ubuntu:latest
MAINTAINER Thiago Perrotta <tbperrotta@gmail.com>
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -q && apt-get install -q -y \
    texlive-full \
    make git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /data
VOLUME ["/data"]
