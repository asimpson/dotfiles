ht() { cd /Applications/MAMP/htdocs/$1; }
_ht() { _files -W /Applications/MAMP/htdocs -/; }
compdef _ht ht