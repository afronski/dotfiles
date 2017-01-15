#!/bin/bash

curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.29.0/install.sh | bash

nvm install 0.8
nvm install 0.10
nvm install 0.12
nvm install 4.0
nvm install 4.1
nvm install 5.0
nvm install 6.0
nvm install 7.0

nvm aliast default 0.12

gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
\curl -sSL https://get.rvm.io | bash -s stable

rvm install 1.9.3
rvm install 2.0
rvm install 2.2
rvm install jruby

git clone https://github.com/gcuisinier/jenv.git ~/.jenv

curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
kerl build 18.1 18.1
mkdir /home/afronski/.kerl/erlangs
kerl install 18.1 /home/afronski/.kerl/erlangs

\curl -sSL https://raw.githubusercontent.com/taylor/kiex/master/install | bash -s
kiex install 1.1.1

curl -sSL https://raw.githubusercontent.com/aspnet/Home/dev/dnvminstall.sh | DNX_BRANCH=dev sh && source ~/.dnx/dnvm/dnvm.sh
dnvm upgrade -r coreclr

curl -sL https://raw.githubusercontent.com/brainsik/virtualenv-burrito/master/virtualenv-burrito.sh | $SHELL
