(setq pgpath "/home/ars/postgres/install/vanilla/bin/postgres")

(defun pg-gdblive()
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
      (gdb (concat "gdb -i=mi --silent " pgpath " " psql-pid))
  )
  )

(defun pg-shardlord-gdblive()
"Attach to shardlor bgw"
  (interactive)
  (setq shardlord-pid (shell-command-to-string
"ps auxww | \
 grep 'postgres: bgworker: shardlord' | \
 awk '{print $2}' | head -1 | tr -d '\n'
"
                 )
  )
  (if (string= "" shardlord-pid)
      (message "%s" "shardlord not found gdb will not start")
      (message "%s%s%s" "Found shardlord with pid" shardlord-pid ", running gdb...")
      (gdb (concat "gdb -i=mi --silent " pgpath " " shardlord-pid))
      )
  )

(defun pg-apply-worker-gdblive()
"Attach to apply worker bgw"
  (interactive)
  (setq apply-worker-pid (shell-command-to-string
"ps aux | \
 grep -v -e 'grep' | \
 grep 'postgres: bgworker: logical replication worker for subscription' | \
 awk '{print $2}' | head -1 | tr -d '\n'
"
                 )
  )
  (if (string= "" apply-worker-pid)
      (message "%s" "apply worker not found, gdb will not start")
      (message "%s%s%s" "Found apply worker with pid" apply-worker-pid ", running gdb...")
      (gdb (concat "gdb -i=mi --silent " pgpath " " apply-worker-pid))
      )
  )

(defun pg-gdblive-manual()
  "Attach to any pg process"
  (interactive)
  (gdb (concat "gdb -i=mi --silent " pgpath " "
	       (read-string "Enter process pid:"))))

(defun gdb-core()
  "Debug core file, extracting executable path from it. Core file name is expected
  like sudo sysctl -w kernel.core_pattern=/tmp/core_{%E}_%p"
  (interactive)
  (let* (
	 (core-path (expand-file-name (read-file-name "Core path:" "/tmp/")))
	 (exec-path (subst-char-in-string ?! ?/
		     (car (split-string
			   (car (cdr (split-string core-path "{")))
			   "}"))))

	 )
    (message "cmd is %s" (concat "gdb -i=mi --silent " exec-path " " core-path))
    (gdb (concat "/usr/bin/gdb -i=mi --silent " exec-path " " core-path))
    ))

(message "ars-gdb-stuff loaded")
(provide 'ars-gdb-stuff)
