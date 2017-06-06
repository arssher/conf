(defun postgres-gdblive()
"Find active backend postgres process (usually corresponding to running psql)
and attach gdb to it. Path to Postgres installation directory must be in $PGIDIR"
  (interactive)
  (setq psql-pid (shell-command-to-string
"ps auxww | \
 grep postgres: | \
 grep -v -e 'grep' -e 'postgres: stats' -e 'postgres: writer' \
-e 'postgres: wal writer' -e 'postgres: checkpointer' -e 'postgres: archiver' \
-e 'postgres: logger' -e 'postgres: autovacuum' -e 'postgres: bgworker' | \
 awk '{print $2}' | head -1 | tr -d '\n'
"
                 )
  )
  (if (string= "" psql-pid)
      (message "%s" "Postgres backend process not found, gdb will not start")
      (message "%s%s%s" "Found Postgres backend process with pid " psql-pid ", running gdb...")
      (gdb (concat "gdb -i=mi --silent $PGIDIR/bin/postgres " psql-pid))
  )
)

(message "ars-gdb-stuff loaded")
(provide 'ars-gdb-stuff)
