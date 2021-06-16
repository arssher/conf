# useful for scripting, less interactive
set pagination off
# don't ask if shlib with breakpoint is not yet loaded; make it 'pending'
# silently. Again for scripting.
set breakpoint pending on

# uncomment if you need many watchpoints... good luck
# set can-use-hw-watchpoints 0

# don't stop on usr signals; pg uses them for intra chatting
handle SIGUSR1 nostop
handle SIGUSR2 nostop

# do not ask confirmation on exit
define hook-quit
    set confirm off
end

# There is some problem here: gdb in emacs starts infinitely get 'info break'
# typed with this. And why I wanted logging in the first place?
# set logging off
# well, without that, logging will not work
# https://sourceware.org/bugzilla/show_bug.cgi?id=14584
# set trace-commands on
# set logging file /home/ars/.gdb_log
# set logging on

# save history of unlimited size, write always to ~/.gdb_history
set history save on
set history size unlimited
set history filename ~/.gdb_history

# for printing structs
set print pretty on

# script for postgres internals pretty-printing
source ~/.gdb/gdbpg.py

# some more my stuff
source ~/.gdb/quit_if_alone.py
source ~/.gdb/pg_memctx.py

# import duel module manually, I don't like pip.
# this will fail if
# python3 -m pip install arpeggio
# is not executed
# source ~/.gdb/import_duel.py

# unlimited printing
set print elements 0


# aliases
define fc
  focus next
end
document fc
Focus next
end

define pg_print_lsn
  printf "%X/%X", (uint32) ($arg0 >> 32), (uint32) ($arg0)
end

define offsetof
  print (int) &((($arg0 *) 0)->$arg1)
end
document offsetof
offset of field in struct, e.g. offsetof ReorderBufferTXN base_snapshot_node
end

define pg_berror
  # break elog.c:246
  break pg_re_throw
  b errfinish if errordata[errordata_stack_depth].elevel == 20
end
document pg_berror
break on postgres ERROR
end

define pg_bwarn
  b errfinish if errordata[errordata_stack_depth].elevel == 19
end
document pg_berror
break on postgres WARNING
end

define pg_bassert
  b ExceptionalCondition
end
document pg_bassert
break on postgres Assert failure
end

define show_sighandler
  call malloc(sizeof(struct sigaction))
  call sigaction($arg0, NULL, $1)
  print ((struct sigaction *)$1)->sa_sigaction
end
document show_sighandler
show given signal handler
$arg0 is signal name
end
