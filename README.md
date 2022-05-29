# rgb.el - RGB lighting for Emacs!
A very simple shim script that allows interaction with devices via
[OpenRGB](https://gitlab.com/CalcProgrammer1/OpenRGB).

## Examples
```elisp
;; Set device 0 to be green
(rgb-set :color "00FF00" :device 0)

;; Set device 1 to be red and in "Breathing" mode
(rgb-set :device 1 :color "FF0000" :mode "Breathing")
```

## Running tests
You need to have these libraries installed:
- [Emacs Lisp Expectations](https://www.emacswiki.org/emacs/EmacsLispExpectations)
- [el-mock](https://github.com/rejeep/el-mock.el)

Then:

```
bash run-tests.bash
```
