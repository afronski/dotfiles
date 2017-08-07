#!/usr/bin/env bash

git clone https://github.com/asdf-vm/asdf.git $HOME/.asdf --branch v0.3.0

if ! grep -q ".asdf/asdf.sh" "$HOME/.zshrc"; then
    echo -e '\n. $HOME/.asdf/asdf.sh' >> $HOME/.zshrc
    echo -e '\n. $HOME/.asdf/completions/asdf.bash' >> $HOME/.zshrc
    source $HOME/.zshrc
fi

asdf plugin-add clojure https://github.com/vic/asdf-clojure
asdf plugin-add crystal https://github.com/marciogm/asdf-crystal
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir
asdf plugin-add elm https://github.com/vic/asdf-elm
asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang
asdf plugin-add golang https://github.com/kennyp/asdf-golang
asdf plugin-add idris https://github.com/vic/asdf-idris
asdf plugin-add lfe https://github.com/vic/asdf-lfe
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs
asdf plugin-add packer https://github.com/gozer/asdf-packer
asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby
asdf plugin-add rust https://github.com/code-lever/asdf-rust
asdf plugin-add terraform https://github.com/neerfri/asdf-terraform

rm -rf $HOME/.tool-versions

asdf install nodejs 0.10.48
asdf install nodejs 0.12.18
asdf install nodejs 4.3.2
asdf install nodejs 6.10.1
asdf install nodejs 7.8.0

echo -e "nodejs 4.3.2" >> $HOME/.tool-versions

asdf install erlang R16B03-1
asdf install erlang 17.5
asdf install erlang 18.3
asdf install erlang 19.3

echo "erlang 19.3" >> $HOME/.tool-versions

asdf install elixir 1.2.6
asdf install elixir 1.3.4
asdf install elixir 1.4.2
asdf install elixir 1.5.0

echo "elixir 1.4.2" >> $HOME/.tool-versions

asdf install packer 0.12.3
asdf install packer 1.0.0

echo "packer 0.12.3" >> $HOME/.tool-versions

asdf install terraform 0.10.0

echo "terraform 0.10.0" >> $HOME/.tool-versions
