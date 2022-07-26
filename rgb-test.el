;;; test-rgb.el --- Tests for rgb.el
;;; Version: 2.0.0
;;; URL: https://gitlab.com/cwpitts/rgb.el
;;; Package-Requires: ((emacs "24.3") (el-mock "1.25.1") (ert-expectations "0.2"))

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
;;; This package contains tests for rgb.el.

;;; Code:
(load-file "./el-mock.el")
(load-file "./ert-expectations.el")
;; File loads
(load-file "./rgb.el")

;; Argument formatting
(expectations
 (desc "Correct formatting of arguments")
 (expect (mock (start-process * * * "-d" "0" "-c" "FF0000") => nil)
         (rgb-set-openrgb :color "FF0000" :device 0))
 (expect (mock (start-process * * * "-d" "0" "-c" "FF0000" "-m" "Breathing") => nil)
         (rgb-set-openrgb :color "FF0000" :device 0 :light-mode "Breathing"))
 (expect (mock (start-process * * * "-c" "FF0000" "-m" "Breathing") => nil)
         (rgb-set-openrgb :color "FF0000" :light-mode "Breathing")))

(provide 'test-rgb)
;;; test-rgb.el ends here

