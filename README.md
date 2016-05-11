dotfiles
========

Config files, mostly Mac-focused, but many things work on Linux too.

`deploy.sh` symlinks things in the `home` folder into the user's `$HOME`
folder, prepending a dot to the filename.
It asks you before clobbering anything.

`brew_installs.sh` installs most of the Mac programs I need, sets the
shell to `zsh`, and sets some useful system properties.

I still have to semi-manually set Caps Lock to the hyper key
(Ctrl+Shift+Option+Command).
Set it to keycode `80` in Seil, then remap it using Karabiner and the
`Karabiner/private.xml` file.
