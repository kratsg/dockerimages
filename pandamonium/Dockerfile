FROM python:3.7-alpine

RUN apk add --no-cache git bash && \
    pip install git+https://github.com/PanDAWMS/panda-client.git && \
    git clone -b master https://github.com/dguest/pandamonium /home/root/pandamonium && \
    echo "export PATH=\$PATH:/home/root/pandamonium" >> ~/.bashrc

WORKDIR /home/panda
