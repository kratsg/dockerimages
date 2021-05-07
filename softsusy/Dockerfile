FROM atlasamglab/root-base

ARG SS_VERSION=4.1.9
RUN mkdir /opt/softsusy && \
    wget -O softsusy.tar.gz https://softsusy.hepforge.org/downloads/?f=softsusy-${SS_VERSION}.tar.gz && \
    tar -xzvf softsusy.tar.gz --directory /opt/softsusy/ --strip-components=1 && \
    rm softsusy.tar.gz && \
    cd /opt/softsusy && \
    ./configure && \
    make

#ENTRYPOINT [""]
#CMD [ "-d", "/opt/suspect3/examples/CompressedSuSy.in" ]
