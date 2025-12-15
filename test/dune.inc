; filepath: test/dune.inc

; Auto-generated rules to run typing_test.exe on each C file and diff against
; the corresponding .expected file. Dune captures stdout using with-stdout-to.

(rule
 (alias   runtest)
 (deps    cprogs/for.c)
 (action  (with-stdout-to for.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/for.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/for.c cprogs/for.expected)
 (action  (diff cprogs/for.expected for.output)))

(rule
 (alias   runtest)
 (deps    cprogs/plus.c)
 (action  (with-stdout-to plus.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/plus.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/plus.c cprogs/plus.expected)
 (action  (diff cprogs/plus.expected plus.output)))

(rule
 (alias   runtest)
 (deps    cprogs/while.c)
 (action  (with-stdout-to while.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/while.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/while.c cprogs/while.expected)
 (action  (diff cprogs/while.expected while.output)))



(rule
 (alias   runtest)
 (deps    cprogs/small.c)
 (action  (with-stdout-to small.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/small.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/small.c cprogs/small.expected)
 (action  (diff cprogs/small.expected small.output)))

(rule
 (alias   runtest)
 (deps    cprogs/switch.c)
 (action  (with-stdout-to switch.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/switch.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/switch.c cprogs/switch.expected)
 (action  (diff cprogs/switch.expected switch.output)))

(rule
 (alias   runtest)
 (deps    cprogs/typeof.c)
 (action  (with-stdout-to typeof.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/typeof.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/typeof.c cprogs/typeof.expected)
 (action  (diff cprogs/typeof.expected typeof.output)))

(rule
 (alias   runtest)
 (deps    cprogs/cast.c)
 (action  (with-stdout-to cast.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/cast.c))) )

(rule
 (alias   runtest)
 (deps    cprogs/cast.c cprogs/cast.expected)
 (action  (diff cprogs/cast.expected cast.output)))



(rule
 (alias   runtest)
 (deps    cprogs/narrowing_comp.c)
 (action  (with-stdout-to narrowing_comp.output
              (run %{exe:typing_test.exe} ../../../test/cprogs/narrowing_comp.c))))

(rule
 (alias   runtest)
 (deps    cprogs/narrowing_comp.c cprogs/narrowing_comp.expected)
 (action  (diff cprogs/narrowing_comp.expected narrowing_comp.output)))

