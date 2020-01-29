FROM cern/cc7-base

ADD files/ca.repo /etc/yum.repos.d/ca.repo
ADD files/wlcg-centos7.repo /etc/yum.repos.d/wlcg-centos7.repo
ADD files/RPM-GPG-KEY-wlcg /etc/pki/rpm-gpg/RPM-GPG-KEY-wlcg

ENV PATH $PATH:/home/root/pandamonium
ENV X509_USER_CERT /root/.globus/usercert.pem
ENV X509_USER_KEY /root/.globus/userkey.pem
RUN yum install -y libffi-devel openssl-devel && \
    yum install -y python-pip voms-clients && \
    yum install -y ca-policy-egi-core wlcg-voms-atlas && \
    yum install -y git && \
    yum clean all && \
    rm -rf /var/cache/yum && \
    pip install -U pip setuptools && \
    pip install panda-client && \
    git clone -b python2 https://github.com/dguest/pandamonium /home/root/pandamonium && \
    mkdir /root/.globus

WORKDIR /home/panda
