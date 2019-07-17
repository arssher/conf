# Remove aborted xacts like
# ...
# 1 ITXFINISH MTM-1-600 0
# 1 ITXFINISH MTM-1-600 p
# ...
# works (match) only with gnu awk

BEGIN { purge=0; }
# $0 is the whole line
match($0, /ITXFINISH MTM-[0-9]+-[0-9]+ ([0-9])/, capt) {
  purge = capt[1] == 0
}
# print the line unless purged
!purge {print $0;}
