This is an XMPP to SIP bridge. Very pre-release and not at all done.

## Main Instance

If you just want to communicate from XMPP to SIP or vice-versa, you should header over to https://sip.cheogram.com and see the instructions there.

## Getting started

This project implements all of the gateway handling, but heavy lifting is done by a [patched asterisk](https://git.singpolyma.net/asterisk).  So, on Debian:

    sudo apt build-dep asterisk
    git clone https://git.singpolyma.net/asterisk
    cd asterisk
    dgit build
    cd ..
    sudo dpkg -i asterisk*.deb

    git clone https://git.singpolyma.net/cheogram-sip
    sudo cp asterisk-conf/* /etc/asterisk/
    cabal build
    dist/build/gateway/gateway host.name.tld xmpp-server 5347 component-secret redis_url

## Getting Help

If you have any questions about this project, or wish to report a bug, please send email to: dev@singpolyma.net

This project is part of the [Soprani.ca](https://soprani.ca) family of projects, and you may connect with the community in the chatroom via `xmpp:discuss@conference.soprani.ca?join` or [on the web](https://anonymous.cheogram.com/discuss@conference.soprani.ca).

## Contributing

If you have code or patches you wish to contribute, the maintainer's preferred mechanism is a git pull request.  Push your changes to a git repository somewhere, for example:

    git remote rename origin upstream
    git remote add origin git@git.sr.ht:~yourname/dhall-ruby
    git push -u origin master

Then generate the pull request:

    git fetch upstream master
    git request-pull -p upstream/master origin

And copy-paste the result into a plain-text email to: dev@singpolyma.net

You may alternately use a patch-based approach as described on https://git-send-email.io

Contributions follow an inbound=outbound model -- you (or your employer) keep all copyright on your patches, but agree to license them according to this project's COPYING file.
