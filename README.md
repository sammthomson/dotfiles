dotfiles
========

Cross-platform shell and Git configuration for Windows, macOS, and Linux.

## Windows

Clone the repository to `~/code/sammthomson/dotfiles`, then run:

```powershell
pwsh -File .\install.ps1
```

Install packages missing from the standard Windows development environment:

```powershell
winget configure -f .\.config\configuration.winget
```

The initial configuration installs GNU Emacs. Review the configuration and its
referenced DSC resources before applying it.

The installer:

- Adds an idempotent block to the current user's all-hosts PowerShell profile.
  The block sources `powershell/profile.ps1`, so tracked changes take effect in
  new shells without reinstalling.
- Adds `~/.local/bin` and this repository's `bin` directory to PowerShell's
  process `PATH`.
- Links the tracked `home/emacs.d` configuration to `%APPDATA%/.emacs.d`, the
  Windows Emacs configuration location, without replacing an existing
  configuration.
- Includes `git/common.gitconfig` from the existing global Git configuration
  without replacing machine-specific credentials or identity.
- Uses the personal Git identity in `git/personal.gitconfig` only for
  repositories below `~/code/sammthomson`.

The tracked PowerShell profile optionally loads
`Documents/dotfiles/powershell/profile.ps1` from the OneDrive root exposed by
`OneDriveCommercial` or `OneDrive`. Set `DOTFILES_LOCAL_ROOT` to override the
local dotfiles root. If the directory or profile is absent, loading is a silent
no-op. Use this private profile for machine-, organization-, or account-specific
commands that should not be published with this repository.

Restart PowerShell after installation, or reload the profile:

```powershell
. $PROFILE.CurrentUserAllHosts
```

Installed PowerShell helpers include:

- `..` — move to the parent directory.
- `e PATH...` — open files in Emacs.
- `et PATH...` — open files in terminal Emacs.
- `gti` — typo-tolerant `git`.
- `gs` — `git status`.
- `reload` — reload the all-hosts PowerShell profile.
- `bak PATH` — move a file to `PATH.bak` without overwriting an existing backup.
- `tgz ARCHIVE PATH...` — create a gzip-compressed tar archive.

The profile sets `EDITOR`, `VISUAL`, and `GIT_EDITOR` to `emacs -nw`.

The Emacs configuration targets Emacs 29 and later. It uses built-in project,
completion, pairing, Flymake, and Eglot support, with a small package set for
Magit and common editing formats. Missing packages are installed from MELPA on
first startup.

Machine and employer-specific credentials, identities, paths, and service
configuration do not belong in this public repository.

## macOS

The macOS setup supports Apple Silicon. Run the complete bootstrap:

```zsh
zsh ./bootstrap/macos.sh
```

The bootstrap installs Homebrew when needed, applies the declarative
`Brewfile`, deploys the tracked configuration, installs the global runtimes in
`mise.toml`, and applies the non-elevated settings in `macos/defaults.sh`.

Packages and runtimes can also be managed independently:

```zsh
brew bundle --no-upgrade --file ./Brewfile
mise install
```

`mise.toml` manages Java (Temurin 21), Scala 3.3.1, and sbt 1.13.0 alongside
Node and Python; the Brewfile does not manage these language tools. sbt uses
each project's configured Scala version rather than the global Scala version.

`deploy.sh` manages a small loader block in `~/.zshrc` that sources
`home/zshrc` directly from this checkout. The tracked profile sources its
`home/zsh.d` modules and Antidote plugin list directly from the repository, so
edits take effect in the next shell without redeployment.

Applications that cannot include tracked configuration use explicit links:

- `~/.emacs.d`
- `~/.ghc`
- `~/.inputrc`
- `~/.pylintrc`
- `~/.config/mise/config.toml`

The deployer also configures the common and personal conditional Git includes.
It never replaces a conflicting target unless explicitly asked to preserve the
old target first:

```zsh
./deploy.sh --check   # report drift without changing anything
./deploy.sh --link    # install missing configuration; refuse conflicts
./deploy.sh --backup  # make timestamped backups before resolving conflicts
```

## Private profile and other Unix environments

The tracked Zsh configuration optionally loads a private synced profile from:

- macOS: `~/Library/CloudStorage/OneDrive-*/Documents/dotfiles/zsh/profile.zsh`
- WSL: `Documents/dotfiles/zsh/profile.zsh` below the Windows
  `OneDriveCommercial` or `OneDrive` directory

Set `DOTFILES_LOCAL_ROOT` to bypass discovery. Native Linux and machines
without a matching profile continue without output or side effects, so the
same repository can be installed on a personal laptop without OneDrive.

## Manual macOS steps

- Unbind Control-Left and Control-Right in **System Settings > Keyboard >
  Keyboard Shortcuts > Mission Control** so Zsh can use them for word movement.
