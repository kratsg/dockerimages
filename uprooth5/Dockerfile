FROM rootproject/root-ubuntu16:latest
USER root
WORKDIR /code

RUN curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py && \
    python /tmp/get-pip.py && \
    rm /tmp/get-pip.py && \
    apt-get update && apt-get install -y parallel jq && \
    pip install uproot numpy matplotlib h5py && \
    bash -c 'source /usr/local/bin/thisroot.sh && \
    echo "source /usr/local/bin/thisroot.sh" >> $HOME/.bashrc && \
