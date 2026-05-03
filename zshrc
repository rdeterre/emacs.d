autoload -U select-word-style
select-word-style bash  # alphanumeric + underscore only, like Emacs

_zle-forward-to-word-start() {
  while (( CURSOR < ${#BUFFER} )) && [[ ${BUFFER:$CURSOR:1} == [[:alnum:]_] ]]; do
    (( CURSOR++ ))
  done
  while (( CURSOR < ${#BUFFER} )) && [[ ${BUFFER:$CURSOR:1} != [[:alnum:]_] ]]; do
    (( CURSOR++ ))
  done
}
zle -N _zle-forward-to-word-start
bindkey '\eF' _zle-forward-to-word-start

_zle-backward-to-word-end() {
  while (( CURSOR > 0 )) && [[ ${BUFFER:$((CURSOR-1)):1} == [[:alnum:]_] ]]; do
    (( CURSOR-- ))
  done
  while (( CURSOR > 0 )) && [[ ${BUFFER:$((CURSOR-1)):1} != [[:alnum:]_] ]]; do
    (( CURSOR-- ))
  done
}
zle -N _zle-backward-to-word-end
bindkey '\eB' _zle-backward-to-word-end
