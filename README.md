# The iVM assembler and prototype VM

## Introduction

The iVM assembler was made for two reasons: To make it feasible to program the iVM by hand, and also to act as a target for the iVM compiler.  The assembly language provides some useful shorthands and it abstracts away details that does not regard the assembly programmer or the compiler.

## The assembly language

For code examples and a language introduction see the source files in [Assembler.Tests/test_code](Assembler.Tests/test_code),
in particular these files:

* [intro1_statements.s](Assembler.Tests/test_code/intro1_statements.s)
* [intro2_basics.s](Assembler.Tests/test_code/intro2_basics.s)
* [intro3_advanced.s](Assembler.Tests/test_code/intro3_advanced.s)
* [ex2_check_count_down.s](Assembler.Tests/test_code/ex2_check_count_down.s)
* [ex3_quick_sort.s](Assembler.Tests/test_code/ex3_quick_sort.s)
* [ex4_short_video.s](Assembler.Tests/test_code/ex4_short_video.s)


## Usage

See `ivm --help`.

### Getting started

When getting started with the assembly language, the most convenient
commands are `as-run`, `as-run -t` and `check` with a single source file as
argument:

    cd Assembler.Tests/test_code

    ivm as-run ex2_check_count_down.s

    ivm as-run -t test_linking1.s

    ivm check ex3_quick_sort.s

The `as-run` command assembles a source file, executes the resulting
binary and prints out the final contents of the stack. `as-run -t` is
similar, but prints out lots of debug information on the way. Finally,
`check` works as `as-run`, except that the final stack is compared to an
expected list of numbers written in the file itself.

If an "arg file" is specified, its contents is appended to the binary
before execution. Thus, this is a form of 'command line argument'. If an
"output dir" is also specified, then any output (image and sound files)
will be written to this directory. For now, it is recommended to use an
empty directory (as any contents may get overwritten). If no output dir is
provided, any output will be discarded.

### Dependencies, projects and builds

Assembly files may refer to each other. Such references must be explicit,
using EXPORT and IMPORT statements. Currently, all imports are resolved
relative to the "root" directory from where `ivm` was called. Use dots to
refer to subdirectories (cf. Java packages).

When calling `as`, `as-run`, `as-run -t` and `check`, the assembler first
builds a graph of references and attempts a topological sort. Circular
dependencies are not allowed. Next, all the source files get assembled
into one binary file. This is not ideal when we have a large tree of
dependencies. In this situation, we split the process in two:

* First, we generate a "project file" using `gen` containing the
  result of the topological sort.
* Then we build this project using the `build` command.
* If instead we use the `build -i` command, we attempt to re-build only
  what has changed "incrementally".

Observe that:

1. The goal passed to `gen` is the file where the execution should start,
   but referred to as in import statements, i.e. with . instead of / and
   without the file suffix (.s).

2. Whereas `as` only outputs the "linked" binary and symbols files (i.e.
   for all the code) and lets you choose the names of these files, `build`
   and `build -i` also produce binary and symbol files for each source
   file and puts them in the directory tree under "dest dir". The file
   suffixes are `.b` and `.sym`, respectively. These commands also produce
   "linked" binary and symbol files. They have `$` in the name before the
   suffix.


## Semi-formal EBNF, ignoring whitespace and comments

```ebnf
program = import* statement*;

import = "IMPORT" (identifier "/")+ identifier

statement = identifier ":"                                         (* label *)
          | "EXPORT" identifier                                    (* export declaration *)
          | identifier "=" expression                              (* abbreviation *)
          | "data1" "[" expression* "]" ("*" positive_numeral)?    (* data segment,  8 bits per value *)
          | "data2" "[" expression* "]" ("*" positive_numeral)?    (* data segment, 16 bits per value *)
          | "data4" "[" expression* "]" ("*" positive_numeral)?    (* data segment, 32 bits per value *)
          | "data8" "[" expression* "]" ("*" positive_numeral)?    (* data segment, 64 bits per value *)
          | "space" expression                                     (* pointer static byte array *)

          | "exit" | "exit!" expression
          | "push" | "push!" expression | "push!!" expression expression | ...
          | "set_sp" | "set_sp!" expression
          | "jump" | "jump!" expression
          | "jump_zero" | "jump_zero!" expression | "jump_zero!!" expression expression
          | "jump_not_zero" | "jump_not_zero!" expression | "jump_not_zero!!" expression expression
          | "call" | "call!" expression
          | "return"                                               (* alias for "jump" *)

          | "load1" | "load1!" expression
          | "load2" | "load2!" expression
          | "load4" | "load4!" expression
          | "load8" | "load8!" expression
          | "sigx1" | "sigx1!" expression
          | "sigx2" | "sigx2!" expression
          | "sigx4" | "sigx4!" expression
          | "sigx8" | "sigx8!" expression                          (* no-op *)
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

          | "read_frame" | "read_frame!" expression
          | ... | "read_pixel!!" expression expression

          | ... | "put_char!" expression
          | ... | "put_byte!" expression
          | ... | "new_frame!!!" expression expression expression
          | ... | "set_pixel!!!!!" expression ...
          | ... | "add_sample!!" expression expression;

expression = positive_numeral  (* 0 to 2^64-1 *)
           | identifier        (* label or abbreviation *)

           | "-" expression    (* corresponding statement: neg *)
           | "~" expression    (* not *)
           | "$" expression    (* stack content *)
           | "&" expression    (* stack pointer *)

           | "(" "+" expression* ")"                (* add *)
           | "(" "*" expression* ")"                (* mult *)
           | "(" "&" expression* ")"                (* and *)
           | "(" "|" expression* ")"                (* or  *)
           | "(" "^" expression* ")"                (* xor *)

           | "(" "="   expression expression ")"    (* eq *)
           | "(" "<u"  expression expression ")"    (* lt_u *)
           | "(" "<s"  expression expression ")"    (* lt_s *)
           | "(" "<=u" expression expression ")"    (* lte_u *)
           | "(" "<=s" expression expression ")"    (* lte_s *)
           | "(" ">u"  expression expression ")"    (* gt_u *)
           | "(" ">s"  expression expression ")"    (* gt_s *)
           | "(" ">=u" expression expression ")"    (* gte_u *)
           | "(" ">=s" expression expression ")"    (* gte_s *)

           | "(" "<<"  expression expression ")"    (* shift_l *)
           | "(" ">>u" expression expression ")"    (* shift_r unsigned, unsigned *)
           | "(" ">>s" expression expression ")"    (* shift_r signed, unsigned *)
           | "(" "/u"  expression expression ")"    (* div_u *)
           | "(" "/s"  expression expression ")"    (* div_s *)
           | "(" "%u"  expression expression ")"    (* rem_u *)
           | "(" "%s"  expression expression ")"    (* rem_s *)

           | "(" "load1" expression ")"
           | "(" "load2" expression ")"
           | "(" "load4" expression ")"
           | "(" "load8" expression ")"
           | "(" "sigx1" expression ")"
           | "(" "sigx2" expression ")"
           | "(" "sigx4" expression ")"
           | "(" "sigx8" expression ")"             (* identity function *)

identifier = (letter | "_" | "." ) (letter | "_" | "." | digit)*;
```

In v0.8 we added an alternative notation for "immediate arguments":

    push* [ <e1> <e2> ... <en> ]

is syntactic sugar for:

    push!!..! <e1> <e2> ... <en>        # with n exclamation marks

Similarly for the other statements, e.g. `set_pixel*`.

The arguments to `space` and `data<N>` must be compile time constants,
except that `data8` also accepts labels.


## Adapting Emacs asm-mode

Here are my adjustments to asm-mode in Emacs, in case you want to write
iVM assembly by hand. If you put this in your .emacs, you should replace
the path in the first line to where you can find the ivm executable.

    (setenv "PATH" (concat "~/ivm-implementations" ":" (getenv "PATH")))

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

## Install
### Ubuntu
sudo apt-get install -y libgdiplus 
