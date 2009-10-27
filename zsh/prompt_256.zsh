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

git_branch=""

PROMPT="${p}(${name}${p}@${host}${p})-${jobs}(${time}${p})-(${dir}${p}\${git_branch}${p})
(${last}${p}${hist}${p}:${priv}${p})- %{$FX[reset]%}"
