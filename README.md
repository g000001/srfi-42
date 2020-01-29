# SRFI 42 for Common Lisp: Eager Comprehensions

* License: MIT
* https://srfi.schemers.org/srfi-42/


```cl
(list-ec (:- i 10) i)
→ (0 1 2 3 4 5 6 7 8 9)
```