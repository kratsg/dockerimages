# Container Registry: https://gitlab.cern.ch/dss/eos/container_registry/10186
FROM gitlab-registry.cern.ch/dss/eos/eos-fusex:4.8.56

ENV EOS_MGM_URL=root://eosuser.cern.ch

COPY files/motd /etc/motd
COPY files/nice_prompt.sh /etc/profile.d/nice_prompt.sh

RUN echo 'exclude = mirror.ilumno.com' >> /etc/yum/pluginconf.d/fastestmirror.conf && \
    yum clean expire-cache && \
    yum install -y krb5-workstation && \
    yum clean all && \
    rm -rf /var/cache/yum && \
    curl -sL -o /etc/krb5.conf https://linux.web.cern.ch/docs/krb5.conf && \
    echo '[ ! -z "$TERM" -a -r /etc/motd ] && cat /etc/motd' >> /etc/bashrc

WORKDIR /home/workdir
SHELL ["/bin/bash", "-c"]
