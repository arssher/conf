import importlib
import os
import sys

home = os.path.expanduser("~")
duel_path = os.path.join(home, ".gdb/vuvova-gdb-tools/duel/__init__.py")

# looks terrible, adviced from
# https://stackoverflow.com/questions/67631/how-to-import-a-module-given-the-full-path
spec = importlib.util.spec_from_file_location("duel", duel_path)
duel_module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = duel_module
spec.loader.exec_module(duel_module)
