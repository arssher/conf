"to machine" scripts install yandex disk conf onto the machine, "to disk" scripts export machine's conf to
yandex_disk

Start with bash/.bash_scripts/bin/mint_global_import.sh. It does three imports:
 - bash conf
 - mint shortcuts conf
 - emacs conf

Bash conf:
  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so you can create and specify
  machine-dependant global vars there. You can find example in .global_vars.example.

  .bash_scripts contains bin folder, I put executable scripts there. The folder will be added to the PATH in
  .bashrc

Mint shorcuts conf:
  Place new shorctcuts to dconf-settings-template.conf, then run
  ydisk_to_machine.sh, it will install them on the system

"to disk" scripts are not updated and perhaps should be eventually removed.

TODO:
* Deal with terminal -- saving pans and tabs, dir paths?
