# do not ask confirmation on exit
define hook-quit
    set confirm off
end
set logging off
# well, without that, logging will not work
# https://sourceware.org/bugzilla/show_bug.cgi?id=14584
set trace-commands on
set logging file /home/ars/.gdb_history
set logging on

# script for postgres internals pretty-printing
source ~/.gdb/gdbpg.py
