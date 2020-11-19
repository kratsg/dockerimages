FROM cern/cc7-base

ADD files/ca.repo /etc/yum.repos.d/ca.repo
ADD files/wlcg-centos7.repo /etc/yum.repos.d/wlcg-centos7.repo
ADD files/RPM-GPG-KEY-wlcg /etc/pki/rpm-gpg/RPM-GPG-KEY-wlcg
ADD files/rucio.cfg /usr/local/etc/rucio.cfg

ENV PATH $PATH:/home/root/pandamonium
ENV X509_USER_CERT /root/.globus/usercert.pem
ENV X509_USER_KEY /root/.globus/userkey.pem
RUN yum install -y libffi-devel openssl-devel && \
    yum install -y cpp gcc && \
    yum install -y python3 python3-devel voms-clients && \
    yum install -y ca-policy-egi-core wlcg-voms-atlas && \
    yum install -y git && \
    yum clean all && \
    rm -rf /var/cache/yum && \
    pip3 install -U pip setuptools wheel && \
    mkdir /root/.globus

ENV X509_USER_PROXY /tmp/x509up_u0
ENV RUCIO_HOME /usr/local
WORKDIR /home/panda
