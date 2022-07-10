# rgb.el - RGB lighting for Emacs!
A very simple shim script that allows interaction with devices via
[OpenRGB](https://gitlab.com/CalcProgrammer1/OpenRGB).

## Examples
### Direct RGB commands
```elisp
;; Set device 0 to be green
(rgb-set :color "00FF00" :device 0)

;; Set device 1 to be red and in "Breathing" mode
(rgb-set :device 1 :color "FF0000" :mode "Breathing")
```

### Adding mode RGB configurations
```elisp
;; Individually
(setf (alist-get 'perl-mode rgb-argmap)
       '((color . "#FF0000")
         (mode . "Strobe")))
 (setf (alist-get 'python-mode rgb-argmap)
       '((color . "#FFFF00")
         (mode . "Static")))
```

```elisp
(setq rgb-argmap
      (cl-pairlis
       '(org-mode perl-mode python-mode emacs-lisp-mode rust-mode markdown-mode)
       '(((color . "#800080") (mode . "Static")) ;; org-mode
         ((color . "#FF0000") (mode . "Strobe")) ;; perl-mode
         ((color . "#FFFF00") (mode . "Static")) ;; python-mode
         ((color . "#800080") (mode . "Static")) ;; emacs-lisp-mode
         ((color . "#FFA500") (mode . "Static")) ;; rust-mode
         ((color . "#FFC0CB") (mode . "Static")) ;; markdown-mode
         )))
```

## Running tests
You need to have these libraries installed:
- [Emacs Lisp Expectations](https://www.emacswiki.org/emacs/EmacsLispExpectations)
- [el-mock](https://github.com/rejeep/el-mock.el)

Then:

```
bash run-tests.bash
```

## Adding a new backend
To add a new backend, implement a function called `rgb-backend-<foo>`,
where `<foo>` is the name of the backend program that controls the RBG
device(s). To select that backend, set `rgb-executable` to the path to
the executable, and `rgb-backend` to `<foo>`. It's important that
`<foo>` in the `rgb-backend-<foo>` function and the `rgb-backend`
variable match exactly, since that's how `rgb` selects the correct
backend function to call. Your backend function should take arguments
parsed from an argmap as shown in the example (naturally, you can
change the exactly keyword arguments and values to match what's
expected by the backend program).
