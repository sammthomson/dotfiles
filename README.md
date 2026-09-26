dotfiles
========

Cross-platform shell and Git configuration for Windows, macOS, and Linux.

## Windows

Clone the repository to `~/code/sammthomson/dotfiles`, then run:

```powershell
pwsh -File .\install.ps1
```

The installer:

- Adds an idempotent block to the current user's all-hosts PowerShell profile.
  The block sources `powershell/profile.ps1`, so tracked changes take effect in
  new shells without reinstalling.
- Adds `~/.local/bin` and this repository's `bin` directory to PowerShell's
  process `PATH`.
- Includes `git/common.gitconfig` from the existing global Git configuration
  without replacing machine-specific credentials or identity.
- Uses the personal Git identity in `git/personal.gitconfig` only for
  repositories below `~/code/sammthomson`.

The tracked PowerShell profile also loads an optional `profile.local.ps1` from
the same directory as the all-hosts profile. Use that untracked file for
machine-, organization-, or account-specific commands that should not be
published with this repository.

Restart PowerShell after installation, or reload the profile:

```powershell
. $PROFILE.CurrentUserAllHosts
```

Installed PowerShell helpers include:

- `..` — move to the parent directory.
- `gti` — typo-tolerant `git`.
- `gs` — `git status`.
- `reload` — reload the all-hosts PowerShell profile.
- `bak PATH` — move a file to `PATH.bak` without overwriting an existing backup.
- `tgz ARCHIVE PATH...` — create a gzip-compressed tar archive.

Machine and employer-specific credentials, identities, paths, and service
configuration do not belong in this public repository.

## macOS and Linux

`deploy.sh` symlinks things in the `home` folder into the user's `$HOME`
folder, prepending a dot to the filename.
It asks you before clobbering anything.

`brew_installs.sh` installs most of the Mac programs I need, sets the
shell to `zsh`, and sets some useful system properties.


# Manual steps

Unbind Ctrl-arrow in Settings > Keyboard > Shortcuts > Mission Control > Move {left/right} a space

---I still have to semi-manually set Caps Lock to the hyper key
(Ctrl+Shift+Option+Command).---
---Set it to keycode `80` in Seil, then remap it using Karabiner and the
`Karabiner/private.xml` file.---
