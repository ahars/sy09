FROM ubuntu:15.10
MAINTAINER Antoine Hars <antoine.hars@gmail.com>

RUN apt-get update -y && \
    apt-get install -y \
        vim \
        git \
        curl

RUN apt-get install -y r-base 

CMD R --no-save

