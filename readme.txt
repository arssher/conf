"to machine" scripts install yandex disk conf onto the machine, "to disk"
scripts export machine's conf to yandex_disk


Start with bash/.bash_scripts/bin/mint_global_import.sh. It does three imports:
 - bash conf
 - mint shortcuts conf
 - emacs conf
 TODO add others


Bash conf:

  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so
  you can create and specify machine-dependant global vars there. You can find
  example in .global_vars.example.

  .bash_scripts contains bin folder, I put executable scripts there. The
  directory will be added to the PATH in .bashrc (subdirectories will not be
  added).


Mint shorcuts conf:

  Place new shorctcuts to dconf-settings-template.conf, then run
  ydisk_to_machine.sh, it will install them on the system


Emacs conf:

  Only init.el file and two dirs -- static_packages and themes -- are backup'ed.
  The rest is the ELPA's business.


gdb conf:

  Contains only .gdbinit file.


readline conf:

  Contains only .inputrc file with keybindings like in my emacs.

TODO:
* Deal with terminal -- saving pans and tabs, dir paths?
