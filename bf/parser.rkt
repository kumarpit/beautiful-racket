#lang brag
;; brag is a DSL for implementing parsers based on grammar specification
bf-program : (bf-op | bf-loop)*
bf-op      : ">" | "<" | "+" | "-" | "." | ","
bf-loop    : "[" (bf-op | bf-loop)* "]"