#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export DOT_FILES_DIR=~/Applications/dotfiles

. $DOT_FILES_DIR/bash/env
. $DOT_FILES_DIR/bash/config
. $DOT_FILES_DIR/bash/aliases
