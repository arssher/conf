mint_global_import does three imports:
 - bash conf
 - mint shortcuts conf
 - emacs conf

Bash conf:
  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so you can create and specify
  machine-dependant global vars there. You can find example in .global_vars.example

TODO:
* Deal with terminal -- saving pans and tabs, dir paths?
