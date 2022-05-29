(load-file "./el-mock.el")
(load-file "./ert-expectations.el")
;; File loads
(load-file "./rgb.el")

;; Argument formatting
(expectations
 (desc "Correct formatting of arguments")
 (expect (mock (start-process * * * "-d" "0" "-c" "FF0000") => nil)
				 (rgb-set :color "FF0000" :device 0))
 (expect (mock (start-process * * * "-d" "0" "-c" "FF0000" "-m" "Breathing") => nil)
				 (rgb-set :color "FF0000" :device 0 :mode "Breathing"))
 (expect (mock (start-process * * * "-c" "FF0000" "-m" "Breathing") => nil)
				 (rgb-set :color "FF0000" :mode "Breathing")))
