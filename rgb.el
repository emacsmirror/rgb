;;; rgb.el --- RGB control from Emacs.

;;; Copyright © 2022 Christopher Pitts <cwpitts@protonmail.com>
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the “Software”), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;; This package uses the OpenRGB software to provide control of RGB devices
;;; from within Emacs! It assumes that there's a usable OpenRGB installation
;;; that can be found on the path.

;;; Code:
(require 'cl-lib)

(defconst rgb-executable (executable-find "openrgb"))

(cl-defun rgb-set (&key device color mode)
  "Execute RGB set command on DEVICE with arguments for COLOR and MODE."
  (let ((args (list)))
    (if device (set 'args (append args (list "-d" (format "%d" device)))))
    (if color (set 'args (append args (list "-c" (format "%s" color)))))
    (if mode (set 'args (append args (list "-m" (format "%s" mode)))))
    (apply 'start-process
           (append (list rgb-executable (format "*%s*" rgb-executable) rgb-executable)
                   args))))
(provide 'rgb)
;;; rgb.el ends here
