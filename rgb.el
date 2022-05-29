;;; rgb.el --- RGB control from Emacs.
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
