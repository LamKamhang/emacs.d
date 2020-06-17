#!/bin/sh

# emacsclient -c || (emacs --no-desktop --daemon && emacsclient -c)
# emacsclient -c -e '(select-frame-set-input-focus (selected-frame))' || (emacs --no-desktop --daemon && emacsclient -c -e '(select-frame-set-input-focus (selected-frame))')
emacsclient -c -e '(select-frame-set-input-focus (selected-frame))'
