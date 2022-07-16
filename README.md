# rgb.el - RGB lighting for Emacs!
A minor mode that allows interaction with RGB devices. Default support
is via [OpenRGB](https://gitlab.com/CalcProgrammer1/OpenRGB), but
`rgb.el` is written to allow for extension and custom backends.

`rgb.el` has support for customization via `M-x customize RET`, and
you may fint it easier to edit the mode-to-RGB-parameter mappings
(`rgb-argmap`) there as opposed to writing it directly in Elisp
(although a section below demonstrates doing just that with
`cl-pairlis`).

## Examples
### Direct RGB commands
```elisp
;; Set device 0 to be green
(rgb-set :color "00FF00" :device 0)

;; Set device 1 to be red and in "Breathing" mode
(rgb-set :device 1 :color "FF0000" :mode "Breathing")
```

### Setting device list
```elisp
(setq rgb-device-ids (list 0))
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

### Example `use-package` configuration
```elisp
(use-package rgb
  :demand
  :init
  (setq rgb-device-ids (list "0"))
  (setq rgb-argmap
        (cl-pairlis
         '(org-mode perl-mode python-mode emacs-lisp-mode rust-mode markdown-mode c-mode c++-mode)
         '(((color . "#800080") (mode . "Static")) ;; org-mode
           ((color . "#FF0000") (mode . "Strobe")) ;; perl-mode
           ((color . "#FFFF00") (mode . "Static")) ;; python-mode
           ((color . "#800080") (mode . "Static")) ;; emacs-lisp-mode
           ((color . "#FFA500") (mode . "Static")) ;; rust-mode
           ((color . "#FFC0CB") (mode . "Static")) ;; markdown-mode
           ((color . "#0000FF") (mode . "Static")) ;; c-mode
           ((color . "#0000FF") (mode . "Static")) ;; c++-mode
           )))
    :config
    (rgb-mode))
```

## Running tests
You need to have these libraries installed:
- [Emacs Lisp Expectations](https://www.emacswiki.org/emacs/EmacsLispExpectations)
- [el-mock](https://github.com/rejeep/el-mock.el)

Then:

```
bash run-tests.bash
```

## Selecting a different backend
To select a different backend, set the `rgb-mode-backend` variable to
a string that is the name of the backend. See next section for some
caveats on how you have to enter the backend name.

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
