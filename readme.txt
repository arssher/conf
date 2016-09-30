"to machine" scripts install yandex disk conf onto the machine, "to disk" scripts export machine's conf to
yandex_disk

mint_global_import does three imports:
 - bash conf
 - mint shortcuts conf
 - emacs conf

Bash conf:
  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so you can create and specify
  machine-dependant global vars there. You can find example in .global_vars.example

"to disk" scripts are not updated and perhaps should be eventually removed.

TODO:
* Deal with terminal -- saving pans and tabs, dir paths?
