; filepath: test/dune.inc
; Auto-generated rules to run typing_test.exe on each C file and diff
; against the corresponding .expected file.

(rule
 (alias runtest)
 (deps
  cprogs/for.c)
 (action
  (with-stdout-to for.output
   (run %{exe:typing_test.exe} %{dep:cprogs/for.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/for.expected
  for.output)
 (action
  (diff cprogs/for.expected for.output)))


(rule
 (alias runtest)
 (deps
  cprogs/plus.c)
 (action
  (with-stdout-to plus.output
   (run %{exe:typing_test.exe} %{dep:cprogs/plus.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/plus.expected
  plus.output)
 (action
  (diff cprogs/plus.expected plus.output)))


(rule
 (alias runtest)
 (deps
  cprogs/while.c)
 (action
  (with-stdout-to while.output
   (run %{exe:typing_test.exe} %{dep:cprogs/while.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/while.expected
  while.output)
 (action
  (diff cprogs/while.expected while.output)))


(rule
 (alias runtest)
 (deps
  cprogs/small.c)
 (action
  (with-stdout-to small.output
   (run %{exe:typing_test.exe} %{dep:cprogs/small.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/small.expected
  small.output)
 (action
  (diff cprogs/small.expected small.output)))


(rule
 (alias runtest)
 (deps
  cprogs/switch.c)
 (action
  (with-stdout-to switch.output
   (run %{exe:typing_test.exe} %{dep:cprogs/switch.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/switch.expected
  switch.output)
 (action
  (diff cprogs/switch.expected switch.output)))


(rule
 (alias runtest)
 (deps
  cprogs/typeof.c)
 (action
  (with-stdout-to typeof.output
   (run %{exe:typing_test.exe} %{dep:cprogs/typeof.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/typeof.expected
  typeof.output)
 (action
  (diff cprogs/typeof.expected typeof.output)))


(rule
 (alias runtest)
 (deps
  cprogs/cast.c)
 (action
  (with-stdout-to cast.output
   (run %{exe:typing_test.exe} %{dep:cprogs/cast.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/cast.expected
  cast.output)
 (action
  (diff cprogs/cast.expected cast.output)))


(rule
 (alias runtest)
 (deps
  cprogs/narrowing_comp.c)
 (action
  (with-stdout-to narrowing_comp.output
   (run %{exe:typing_test.exe} %{dep:cprogs/narrowing_comp.c}))))

(rule
 (alias runtest)
 (deps
  cprogs/narrowing_comp.expected
  narrowing_comp.output)
 (action
  (diff cprogs/narrowing_comp.expected narrowing_comp.output)))

  
(rule
 (alias   runtest)
 (deps    cprogs/error-vs-fail.c)
 (action  (with-stdout-to error-vs-fail.output
              (run %{exe:typing_test.exe} %{dep:cprogs/error-vs-fail.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/error-vs-fail.c cprogs/error-vs-fail.expected)
 (action  (diff cprogs/error-vs-fail.expected error-vs-fail.output)))

(rule
 (alias   runtest)
 (deps    cprogs/list.c)
 (action  (with-stdout-to list.output
              (run %{exe:typing_test.exe} %{dep:cprogs/list.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/list.c cprogs/list.expected)
 (action  (diff cprogs/list.expected list.output)))

(rule
 (alias   runtest)
 (deps    cprogs/string.c)
 (action  (with-stdout-to string.output
              (run %{exe:typing_test.exe} %{dep:cprogs/string.c}))))

(rule
 (alias   runtest)
 (deps    cprogs/string.c cprogs/string.expected)
 (action  (diff cprogs/string.expected string.output)))

