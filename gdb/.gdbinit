# do not ask confirmation on exit
define hook-quit
    set confirm off
end

set logging off
# well, without that, logging will not work
# https://sourceware.org/bugzilla/show_bug.cgi?id=14584
set trace-commands on
set logging file /home/ars/.gdb_log
set logging on

# save history of unlimited size, always remove duplicates, write always to
# ~/.gdb_history
set history save on
set history size -1
set history filename ~/.gdb_history

# script for postgres internals pretty-printing
source ~/.gdb/gdbpg.py
