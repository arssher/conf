#!/bin/bash

# invert all colours
xcalib -invert -alter

# invert theme
curr_theme=$(dconf read /org/mate/desktop/interface/gtk-theme)
if [ "$curr_theme" == "'Menta'" ]; then
    dconf write /org/mate/desktop/interface/gtk-theme "'BlackMATE'"
else
    dconf write /org/mate/desktop/interface/gtk-theme "'Menta'"
fi
