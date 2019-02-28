import gdb

class QuitIfAloneCommand (gdb.Command):
    "quit from gdb if there are no running processes"

    def __init__ (self):
        # False means this is non prefix cmd
        super(QuitIfAloneCommand, self).__init__("quit_if_alone",
                                                 gdb.COMMAND_SUPPORT,
                                                 gdb.COMPLETE_NONE, False)

    # the command itself
    def invoke (self, arg, from_tty):
        infs = gdb.inferiors()
        no_live_processes = True
        for i in infs:
            print("pid {}, valid {}".format(i.pid, i.is_valid()))
            # weird, but exited process is valid with pid = 0
            if i.is_valid() and i.pid != 0:
                no_live_processes = False
                break
        if no_live_processes:
            print("no live processes, exiting")
            gdb.execute('quit')

# instantiate to actually register the command
QuitIfAloneCommand()
