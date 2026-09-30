_load_local_dotfiles_profile() {
  emulate -L zsh
  setopt null_glob
  unset COPILOT_WORKBENCH_WORK_JUDGE

  local relative_profile="zsh/profile.zsh"
  local -a candidates=()

  if [[ -n "${DOTFILES_LOCAL_ROOT:-}" ]]; then
    candidates+=("${DOTFILES_LOCAL_ROOT%/}/$relative_profile")
  else
    case "$(uname -s)" in
      Darwin)
        candidates+=(
          "$HOME"/Library/CloudStorage/OneDrive-*/Documents/dotfiles/$relative_profile
        )
        ;;
      Linux)
        local kernel_release
        if [[ -r /proc/sys/kernel/osrelease ]]; then
          kernel_release="$(</proc/sys/kernel/osrelease)"
        fi
        if [[ "${kernel_release:l}" == *microsoft* ]] &&
            (( $+commands[cmd.exe] )) &&
            (( $+commands[wslpath] )); then
          local variable windows_root unix_root
          for variable in OneDriveCommercial OneDrive; do
            windows_root="$(
              cmd.exe /d /c "echo %${variable}%" 2>/dev/null | tr -d '\r'
            )"
            if [[ -n "$windows_root" && "$windows_root" != "%${variable}%" ]]; then
              unix_root="$(wslpath -u "$windows_root" 2>/dev/null)" || continue
              candidates+=("${unix_root%/}/Documents/dotfiles/$relative_profile")
            fi
          done
        fi
        ;;
    esac
  fi

  local -a profiles=()
  local candidate
  for candidate in "${candidates[@]}"; do
    [[ -r "$candidate" ]] && profiles+=("$candidate")
  done
  typeset -U profiles

  if (( ${#profiles[@]} == 1 )); then
    source "$profiles[1]"
    export COPILOT_WORKBENCH_WORK_JUDGE=1
  elif (( ${#profiles[@]} > 1 )); then
    print -u2 "Multiple local dotfiles profiles found; set DOTFILES_LOCAL_ROOT:"
    printf '  %s\n' "${profiles[@]}" >&2
  fi
}

_load_local_dotfiles_profile
unfunction _load_local_dotfiles_profile
