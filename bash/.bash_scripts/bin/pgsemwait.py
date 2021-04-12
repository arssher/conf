#!/usr/bin/python

from __future__ import print_function
from bcc import BPF
from time import sleep

bpf_text = """
#include <uapi/linux/ptrace.h>
#include <linux/sched.h>

struct stats_key_t {
    u32 pid;
    int user_stack_id;
};

struct stats_value_t {
    u64 total_time;
};

struct start_key_t {
    u32 pid;
};

struct start_value_t {
    u64 last_start;
};

// map_type, key_type, leaf_type, table_name, num_entry
BPF_HASH(stats, struct stats_key_t, struct stats_value_t);
BPF_HASH(start, struct start_key_t, struct start_value_t);

BPF_STACK_TRACE(stack_traces, STACK_STORAGE_SIZE);

int trace_sem_entry(struct pt_regs *ctx)
{
    struct start_key_t start_key = {};
    struct start_value_t start_value = {};

    start_key.pid = bpf_get_current_pid_tgid();
    start_value.last_start = bpf_ktime_get_ns();

    start.update(&start_key, &start_value);

    return 0;
}

int trace_sem_return(struct pt_regs *ctx)
{
    struct stats_key_t stats_key = {};
    struct start_key_t start_key = {};
    struct stats_value_t zero = {};
    struct stats_value_t *stats_value;
    struct start_value_t *start_value;
    u64 delta;

    start_key.pid = bpf_get_current_pid_tgid();
    start_value = start.lookup(&start_key);

    if (!start_value)
        return 0; /* missed start */;

    delta = bpf_ktime_get_ns() - start_value->last_start;
    start.delete(&start_key);

    stats_key.pid = bpf_get_current_pid_tgid();
    stats_key.user_stack_id = stack_traces.get_stackid(ctx, BPF_F_REUSE_STACKID | BPF_F_USER_STACK);

    stats_value = stats.lookup_or_init(&stats_key, &zero);
    stats_value->total_time += delta;

    return 0;
}

"""

# set stack storage size
bpf_text = bpf_text.replace('STACK_STORAGE_SIZE', str(1000))

b = BPF(text=bpf_text)


libpath = BPF.find_exe('/home/ars/postgres/install/shardman_musl/bin/postgres')
if not libpath:
    bail("can't resolve library %s" % library)
library = libpath

b.attach_uprobe(name=library, sym_re='PGSemaphoreLock',
    fn_name="trace_sem_entry",
    pid = -1)

b.attach_uretprobe(name=library, sym_re='PGSemaphoreLock',
    fn_name="trace_sem_return",
    pid = -1)

matched = b.num_open_uprobes()
if matched == 0:
    print("error: 0 functions traced. Exiting.", file=stderr)
    exit(1)

sleep(10)

stats = b.get_table("stats")
stack_traces = b.get_table("stack_traces")
folded = True #False

for k, v in stats.items(): #, key=lambda v: v.total_time):
    #print(dir(k))
    #print(dir(v))
    user_stack = [] if k.user_stack_id < 0 else \
        stack_traces.walk(k.user_stack_id)
    name = 'postgres'
    if v.total_time == 0:
        continue
    if folded:
        # print folded stack output
        user_stack = list(user_stack)
        line = [name] + \
            [b.sym(addr, k.pid) for addr in reversed(user_stack)]
        print("%s %d" % (";".join(line), v.total_time))
    else:
        # print default multi-line stack output
        for addr in user_stack:
            print("    %s" % b.sym(addr, k.pid))
        print("    %-16s %s (%d)" % ("-", name, k.pid))
        print("        %d\n" % v.total_time)
