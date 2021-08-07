PROMPT="%(?:%{$fg_bold[green]%}%n@%m :%{$fg_bold[red]%}%n@%m )"
PROMPT+='%{$fg[blue]%}%~%{$reset_color%} $(git_prompt_info)'
if [ "$UID" -eq 0 ]; then
    PROMPT+=$'\n'"# "
else
    PROMPT+=$'\n'"$ "
fi

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[cyan][%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[cyan]%}] %{$fg[red]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[cyan]%}] %{$fg[green]%}✓"
