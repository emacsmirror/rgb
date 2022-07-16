;;; rgb.el --- RGB control via OpenRGB
;; Version: 2.0.0
;; URL: https://gitlab.com/cwpitts/rgb.el
;; Package-Requires: ((emacs "24.3"))

;; Copyright © 2022 Christopher Pitts <cwpitts@protonmail.com>
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the “Software”), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; This package uses RGB CLI tools to control RGB devices
;; from within Emacs.  By default it uses the OpenRGB software,
;; assuming that there's a usable OpenRGB installation that can be
;; found on the path.  Other supported backends can be added and configured
;; by changing the `rgb-mode-backend` variable.

;;; Code:
(require 'cl-lib)

(defvar rgb-executable (executable-find "openrgb"))
(defvar rgb-device-ids (list))
(defvar rgb-argmap (cl-pairlis '() '()))
(defvar rgb-mode-backend "openrgb")

(defun rgb-set-rgb-from-mode (mode)
  "Select RGB color backend and pass MODE to the backend function."
  (funcall (intern (format "rgb-backend-%s" rgb-mode-backend)) mode))

(cl-defun rgb-set-openrgb (&key device color mode)
  "Execute RGB set command on DEVICE with arguments for COLOR and MODE."
  (let ((args (list)))
    (if device (set 'args (append args (list "-d" (format "%d" device)))))
    (if color (set 'args (append args (list "-c" (format "%s" color)))))
    (if mode (set 'args (append args (list "-m" (format "%s" mode)))))
    (apply 'start-process
           (append (list rgb-executable (format "*%s*" rgb-executable) rgb-executable)
                   args))))

(defun rgb-backend-openrgb (mode)
  "Set RGB color based on current major mode MODE."
  (let ((params (assoc-default mode rgb-argmap)))
    (if params
        (mapc (lambda (device-id)
                (rgb-set-openrgb
                 :device device-id
                 :color (substring (assoc-default 'color params) 1)
                 :mode (assoc-default 'mode params)))
              rgb-device-ids))))

;;###autoload
(defun rgb-enable-mode-change-hook ()
    "Enable RGB mode lights hook."
  (add-hook 'window-buffer-change-functions
            (lambda (window)
              (rgb-set-rgb-from-mode major-mode))))

(provide 'rgb)
;;; rgb.el ends here
