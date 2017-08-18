FROM centos:7
MAINTAINER Tom Lippincott <tom@cs.jhu.edu>
LABEL Description="Concrete Haskell library and utilities."
ENV PATH=/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN curl -sSL https://get.haskellstack.org/ | sh && \
    cd /tmp/concrete-haskell && \
    stack setup

RUN yum install zlib-devel -y && \
    yum clean all -y

COPY . /tmp/concrete-haskell

#RUN cd /tmp/concrete-haskell && \
#    stack build && \
#    stack install && \
#    stack clean --full
