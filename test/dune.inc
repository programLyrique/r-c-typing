; filepath: test/dune.inc

; Auto-generated rules to run typing_test.exe on each C file and diff against
; the corresponding .expected file. Dune captures stdout using with-stdout-to.

(rule
 (alias   runtest)
 (action  (with-stdout-to for.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/for.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/for.expected for.output)))

(rule
 (alias   runtest)
 (action  (with-stdout-to plus.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/plus.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/plus.expected plus.output)))

(rule
 (alias   runtest)
 (action  (with-stdout-to while.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/while.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/while.expected while.output)))



(rule
 (alias   runtest)
 (action  (with-stdout-to small.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/small.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/small.expected small.output)))

(rule
 (alias   runtest)
 (action  (with-stdout-to switch.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/switch.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/switch.expected switch.output)))

(rule
 (alias   runtest)
 (action  (with-stdout-to typeof.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/typeof.c))) )

(rule
 (alias   runtest)
 (action  (diff cprogs/typeof.expected typeof.output)))



