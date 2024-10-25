;;; lisp/_color.lisp --- Color module
;;; Commentary:
;;; Code:

(defvar qob-enable-color t
  "Set to nil to disable color.")

(defun qob-ansi-code (color)
  "Return the ANSI code by COLOR."
  (ecase color
    ;; Style
    (:bold       1)
    (:dark       2)
    (:italic     3)
    (:underscore 4)
    (:blink      5)
    (:rapid      6)
    (:contrary   7)
    (:concealed  8)
    (:strike     9)
    ;; Color
    (:gray      "38;5;8")
    (:black     30)
    (:red       31)
    (:green     32)
    (:yellow    33)
    (:blue      34)
    (:magenta   35)
    (:cyan      36)
    (:white     37)))

(defun qob-ansi-it (code str)
  "Ansi the STR."
  (check-type code keyword)
  (if qob-enable-color
      (format nil "~C[~Am~A~C[0m"
              #\Esc (qob-ansi-code code) str #\Esc)
      str))

;; Style
(defun qob-ansi-bold (str) (qob-ansi-it :bold str))
(defun qob-ansi-dark (str) (qob-ansi-it :dark str))
(defun qob-ansi-italic (str) (qob-ansi-it :italic str))
(defun qob-ansi-underscore (str) (qob-ansi-it :underscore str))
(defun qob-ansi-blink (str) (qob-ansi-it :blink str))
(defun qob-ansi-rapid (str) (qob-ansi-it :rapid str))
(defun qob-ansi-contrary (str) (qob-ansi-it :contrary str))
(defun qob-ansi-concealed (str) (qob-ansi-it :concealed str))
(defun qob-ansi-strike (str) (qob-ansi-it :strike str))

;; Color
(defun qob-ansi-black (str) (qob-ansi-it :black str))
(defun qob-ansi-red (str) (qob-ansi-it :red str))
(defun qob-ansi-green (str) (qob-ansi-it :green str))
(defun qob-ansi-yellow (str) (qob-ansi-it :yellow str))
(defun qob-ansi-blue (str) (qob-ansi-it :blue str))
(defun qob-ansi-magenta (str) (qob-ansi-it :magenta str))
(defun qob-ansi-cyan (str) (qob-ansi-it :cyan str))
(defun qob-ansi-white (str) (qob-ansi-it :white str))

;;; End of lisp/_color.lisp
