FROM gitlab-registry.cern.ch/dss/eos/eos-fusex:4.8.56

ENV EOS_MGM_URL=root://eosuser.cern.ch

RUN yum clean expire-cache && \
    yum install -y krb5-workstation
    yum clean all && \
    rm -rf /var/cache/yum && \
    curl -sL -o /etc/krb5.conf https://linux.web.cern.ch/docs/krb5.conf

WORKDIR /home/root
