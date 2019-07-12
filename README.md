# Work in progress...

For code examples, see the files in Assembler.Tests/test_code.


# Adapting asm-mode in Emacs

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
