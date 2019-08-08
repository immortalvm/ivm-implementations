
# Work in progress...

For code examples and language introduction see the source files in Assembler.Tests/test_code.


## Usage

    ivm as <source> <binary> <symbols>  -  Assemble
    ivm run <binary>                    -  Run binary and print final stack
    ivm trace <binary> <symbols>        -  Trace binary
    ivm as-run <source>                 -  Assemble and run (no output files)
    ivm as-trace <source>               -  Assemble and trace (no output files)
    ivm check <source>                  -  Assemble, run, and check final stack


## Semi-formal EBNF, ignoring whitespace and comments

```ebnf
program = import* statement*;

import = "IMPORT" (identifier ".")+ identifier

statement = identifier ":"               (* label *)
          | "EXPORT" identifier          (* export declaration *)
          | identifier "=" expression    (* abbreviation *)
          | "data" "[" byte* "]"         (* data segment *)

          | "exit"
          | "push" | "push!" expression | "push!!" expression expression | ...
          | "set_sp" | "set_sp!" expression
          | "jump" | "jump!" expression
          | "jump_zero" | "jump_zero!" expression | "jump_zero!!" expression expression
          | "jump_not_zero" | "jump_not_zero!" expression | "jump_not_zero!!" expression expression
          | "call" | "call!" expression
          | "return"                     (* alias for "jump" *)

          | "load1" | "load1!" expression
          | "load2" | "load2!" expression
          | "load4" | "load4!" expression
          | "load8" | "load8!" expression
          | "sigx1" | "sigx1!" expression
          | "sigx2" | "sigx2!" expression
          | "sigx4" | "sigx4!" expression
          | "store1" | "store1!" expression | "store1!!" expression expression
          | "store2" | "store2!" expression | "store2!!" expression expression
          | "store4" | "store4!" expression | "store4!!" expression expression
          | "store8" | "store8!" expression | "store8!!" expression expression

          | "add" | "add!" expression | "add!!" expression expression
          | "sub" | "sub!" expression | "sub!!" expression expression
          | "mult" | "mult!" expression | "mult!!" expression expression
          | "neg" | "neg!" expression
          | "and" | "and!" expression | "and!!" expression expression
          | "or" | "or!" expression | "or!!" expression expression
          | "xor" | "xor!" expression | "xor!!" expression expression
          | "not" | "not!" expression
          | "pow2" | "pow2!" expression
          | "shift_l" | "shift_l!" expression | "shift_l!!" expression expression
          | "shift_ru" | "shift_ru!" expression | "shift_ru!!" expression expression
          | "shift_rs" | "shift_rs!" expression | "shift_rs!!" expression expression

          | "div_u" | "div_u!" expression | "div_u!!" expression expression
          | "div_s" | "div_s!" expression | "div_s!!" expression expression
          | "rem_u" | "rem_u!" expression | "rem_u!!" expression expression
          | "rem_s" | "rem_s!" expression | "rem_s!!" expression expression

          | "lt_u" | "lt_u!" expression | "lt_u!!" expression expression
          | "lt_s" | "lt_s!" expression | "lt_s!!" expression expression
          | "lte_u" | "lte_u!" expression | "lte_u!!" expression expression
          | "lte_s" | "lte_s!" expression | "lte_s!!" expression expression
          | "eq" | "eq!" expression | "eq!!" expression expression
          | "gte_u" | "gte_u!" expression | "gte_u!!" expression expression
          | "gte_s" | "gte_s!" expression | "gte_s!!" expression expression
          | "gt_u" | "gt_u!" expression | "gt_u!!" expression expression
          | "gt_s" | "gt_s!" expression | "gt_s!!" expression expression

          | "allocate" | "allocate!" expression
          | "deallocate" | "deallocate!" expression;

expression = positive_numeral  (* 0 to 2^64-1 *)
           | identifier        (* label or abbreviation *)

           | "-" expression    (* neg *)
           | "~" expression    (* not *)
           | "$" expression    (* stack content *)
           | "&" expression    (* stack pointer *)

           | "(" "+" expression* ")"    (* add *)
           | "(" "*" expression* ")"    (* mult *)
           | "(" "&" expression* ")"    (* and *)
           | "(" "|" expression* ")"    (* or  *)
           | "(" "^" expression* ")"    (* xor *)

           | "(" "="   expression expression ")"    (* eq *)
           | "(" "<u"  expression expression ")"    (* lt_u *)
           | "(" "<s"  expression expression ")"    (* lt_s *)
           | "(" "<=u" expression expression ")"    (* lte_u *)
           | "(" "<=s" expression expression ")"    (* lte_s *)
           | "(" ">u"  expression expression ")"    (* gt_u *)
           | "(" ">s"  expression expression ")"    (* gt_s *)
           | "(" ">=u" expression expression ")"    (* gte_u *)
           | "(" ">=s" expression expression ")"    (* gte_s *)

           | "(" "load1" expression ")"   (* load1 *)
           | "(" "load2" expression ")"   (* load2 *)
           | "(" "load4" expression ")"   (* load4 *)
           | "(" "load8" expression ")"   (* load8 *)
           | "(" "sigx1" expression ")"   (* sigx1 *)
           | "(" "sigx2" expression ")"   (* sigx2 *)
           | "(" "sigx4" expression ")"   (* sigx4 *);

identifier = (letter | "_") (letter | "_" | digit)*;
```


## Adapting Emacs asm-mode

Here are my adjustments to asm-mode in Emacs, in case you want to write
iVM assembly by hand. If you put this in your .emacs, you should replace
the path in the first line to where you can find the ivm executable.

    (setenv "PATH" (concat "/Users/ivarru/Source/NR/ivm-implementations" ":" (getenv "PATH")))

    (defun ivarru-asm-fill-paragraph (&optional justify)
      (interactive)
      (save-excursion
        (beginning-of-line)
        (when (looking-at "( \t)*#+ +")
          (let ((fill-prefix (or (and (looking-at " *#+ +") (match-string 0))
                                 "    "))
                (fill-paragraph-handle-comment t))
            (fill-paragraph justify)))))

    (defun ivarru-asm-mode-comment-hook ()
      (setq-local asm-comment-char ?\#))

    (defun ivarru-asm-mode-hook ()
      (setq-local fill-column 80)
      (setq-local fill-prefix nil)
      (setq-local fill-paragraph-function 'ivarru-asm-fill-paragraph)
      (setq-local compile-command
                  (concat "ivm check " (shell-quote-argument (file-name-nondirectory buffer-file-name))))
      (define-key asm-mode-map "\C-c\C-c" 'compile))

    (eval-after-load "asm-mode"
      '(progn
         (add-hook 'asm-mode-set-comment-hook 'ivarru-asm-mode-comment-hook)
         (add-hook 'asm-mode-hook 'ivarru-asm-mode-hook)))
