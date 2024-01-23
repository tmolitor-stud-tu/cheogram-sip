FROM debian:unstable as asterisk

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /opt
RUN sed -i 's/: deb/: deb deb-src/' /etc/apt/sources.list.d/debian.sources \
 && apt-get update \
 && apt-get -y --no-install-recommends build-dep asterisk \
 && apt-get -y --no-install-recommends install build-essential git libdpkg-perl libgit-wrapper-perl liblist-compare-perl libstring-shellquote-perl libtry-tiny-perl dgit libvpb-dev

RUN git clone https://git.singpolyma.net/asterisk \
 && cd /opt/asterisk \
 && git checkout 3ef8fade9b1803906a9052feb380170b2a51d2f9 \
 && git config user.email "docker@example.com" \
 && git config user.name "Docker" \
 && git deborig \
 && dgit --ignore-dirty build

FROM debian:stable as cheogram-sip

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /opt
RUN apt-get update \
 && apt-get -y --no-install-recommends install ca-certificates git ghc cabal-install c2hs pkg-config libxml2-dev libgsasl-dev libgnutls28-dev

RUN git clone https://git.singpolyma.net/cheogram-sip \
 && cd cheogram-sip \
 && git checkout 290130e42955ccc2406a224b6bc05792258d2eb3 \
 && cabal update \
 && cabal build

FROM debian:unstable

ENV DEBIAN_FRONTEND=noninteractive

COPY --from=asterisk /opt/asterisk-modules_16*.deb /root/
COPY --from=asterisk /opt/asterisk-config_16*.deb /root/
COPY --from=asterisk /opt/asterisk_16*.deb /root/
COPY --from=cheogram-sip /opt/cheogram-sip/dist-newstyle/build/x86_64-linux/ghc-9.0.2/cheogram-sip-0.1.0/x/cheogram-sip/build/cheogram-sip/cheogram-sip /bin/

RUN apt-get update \
 && apt-get -y --no-install-recommends install /root/asterisk*.deb \
 && apt-get -y --no-install-recommends install libxml2 libgsasl18 libgnutls30 \
 && rm -rf /var/cache /root/asterisk*.deb

COPY --from=cheogram-sip /opt/cheogram-sip/asterisk-conf/motif.conf /etc/asterisk/motif.conf
COPY --from=cheogram-sip /opt/cheogram-sip/asterisk-conf/xmpp.conf /etc/asterisk/xmpp.conf
COPY --from=cheogram-sip /opt/cheogram-sip/asterisk-conf/extensions.lua /etc/asterisk/extensions.lua

ENV COMPONENT_DOMAIN=
ENV COMPONENT_SECRET=
ENV ASTERISK_COMPONENT_DOMAIN=asterisk
ENV ASTERISK_COMPONENT_SECRET=
ENV CONNECT_IP=127.0.0.1
ENV CONNECT_PORT=5347

# Optional SIP host registration
ENV SIP_HOST=
ENV SIP_USER=
ENV SIP_PASSWORD=
ENV SIP_JID=

ADD entrypoint.sh /bin/entrypoint.sh
ENTRYPOINT ["/bin/entrypoint.sh"]
