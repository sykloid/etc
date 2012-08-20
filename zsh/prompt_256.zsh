# prompt.zsh: A custom prompt for zsh (256 color version).
# P.C. Shyamshankar <sykora@lucentbeing.com>

local p="%{$FX[reset]$FG[243]%}"

local name="%{$FX[reset]$FG[114]%}%n"
local host="%{$FX[reset]$FG[117]%}%m"
local jobs="%1(j.(%{$FX[reset]$FG[197]%}%j job%2(j.s.)${p})-.)"
local time="%{$FX[reset]$FG[215]%}%*"
local dir="%{$FX[reset]$FG[105]%}%~"

local last="%(?..%{$FX[reset]$FG[203]%}%??${p}:)"
local hist="%{$FX[reset]$FG[220]%}%!!"
local priv="%{$FX[reset]$FG[245]%}%#"

# Use zshcontrib's vcs_info to get information about any current version control systems.
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%{$FX[reset]$FG[082]%}"
zstyle ':vcs_info:*' unstagedstr "%{$FX[reset]$FG[160]%}"
zstyle ':vcs_info:*' formats ":%{$FX[reset]$FG[222]%}%c%u%b"

local vcsi="\${vcs_info_msg_0_}"

PROMPT="${p}(${name}${p}@${host}${p})-${jobs}(${time}${p})-(${dir}${p}${vcsi}${p})
(${last}${p}${hist}${p}:${priv}${p})- %{$FX[reset]%}"
