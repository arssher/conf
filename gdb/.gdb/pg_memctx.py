import gdb

memctx_creation_bpnum = 0
memctx_creation_recycled_bpnum = 0
wp_count = 0
act = False

def cont():
    gdb.execute('continue')

def prompt_hook(current_prompt):
    global act

    print("prompt hook, act is {}".format(act))

    if act:
        # gdb.execute('continue')
        gdb.post_event(cont)
        act = False

    return None

def w():
    gdb.execute('watch -l set->blocks->aset')

def stop_event_handler(event):
    print("STOPEVENTHANDLERR")

    global wp_count
    global act

    if not isinstance(event, gdb.BreakpointEvent):
        print("not breakpoint event")
        return

    for bp in event.breakpoints:
        if bp.number == memctx_creation_bpnum:
            ctx_name = gdb.parse_and_eval('name').string()
            parent = gdb.parse_and_eval('context->parent').string()
            print("parent is {}".format(parent))
            return
            if ctx_name == 'CachedPlanQuery' and wp_count < 10:
                # gdb.Breakpoint('-l set->blocks->aset')
                gdb.post_event(w)
                wp_count = wp_count + 1
                return

    print("going forward")
    act = True
    gdb.post_event(cont)


class TraceMemCtxViolations(gdb.Command):
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
    def invoke(self, arg, from_tty):
        # subscribe to breakpoints breaks
        gdb.events.stop.connect(stop_event_handler)

        memctx_creation_bp = gdb.Breakpoint("aset.c:555")
        global memctx_creation_bpnum
        memctx_creation_bpnum = memctx_creation_bp.number

        # memctx_creation_recycled_bp = gdb.Breakpoint("aset.c:464")
        # global memctx_creation_recycled_bpnum
        # memctx_creation_recycled_bpnum = memctx_creation_recycled_bp.number

        # gdb.prompt_hook = prompt_hook

# instantiate to actually register the command
TraceMemCtxViolations()
