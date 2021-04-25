# Other machine implementations

The prototype machine implementation in of the `ivm` executable is very
slow. `vm.c` contains a much faster implmentation written in C. This
virtual machine was realized as part of the VirtuMa project. It has e.g.
been used for interpreting data in the Portable Document Format using a
port of the Ghostscript PDF interpreter, see
https://github.com/immortalvm/ivm-ghostscript. `vm.c` uses libpng. For
compilation instructions see the top of the file.

This directory also contains a partial implementation of the machine
written in Common Lisp.

A more advanced implementation of the machine, also written in C, is found
in the repo https://github.com/immortalvm/yet-another-ivm-emulator.

## Build instructions (brief)

Compile:
    make

# LISP VM

Independent 3rd party implementation in LISP, implemented from startup guide.

