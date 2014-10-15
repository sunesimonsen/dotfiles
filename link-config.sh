#!/bin/bash
DOTFILES="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
pushd ~ > /dev/null

function linkConfig {
    if [ ! -f "$2" ] || [ -L "$2" ]; then
        mkdir -p $(dirname "$2")
        if [ -L "$2" ] ; then
            rm "$2"
            echo "override" "link" "$2" "to" "$DOTFILES/$1"
        else
            echo "link" "$2" "to" "$DOTFILES/$1"
        fi
        ln -sf "$DOTFILES/$1" "$2"
    else
        echo "file" "$2" "exists"
    fi
}

linkConfig evil-config/emacs.el .emacs
linkConfig vimrc .vimrc
linkConfig vim/snippets .vim/snippets
linkConfig bashrc .bashrc
linkConfig gitconfig .gitconfig
linkConfig i3 .i3
linkConfig Xmodmap .Xmodmap
linkConfig Xresources .Xresources
linkConfig dunst .config/dunst
linkConfig bin/hr .local/bin/hr
linkConfig bin/runtests .local/bin/runtests

chmod +x $DOTFILES/bin/*
chmod +x $DOTFILES/z/z.sh

popd > /dev/null
