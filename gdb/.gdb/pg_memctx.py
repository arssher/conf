import gdb

def stop_event(event):


class TraceMemCtxViolations (gdb.Command):
    """
    Add breaks to ctx creation and destruction. When hit, add breaks on writing
    to context and first block header areas.
    """

    def __init__ (self):
        # False means this is non prefix cmd
        super(TraceMemCtxViolations, self).__init__("trace_mem_ctx_violations",
                                                    gdb.COMMAND_SUPPORT,
                                                    gdb.COMPLETE_NONE, False)

    # the command itself
    def invoke (self, arg, from_tty):
        pass

# instantiate to actually register the command
TraceMemCtxViolations()
