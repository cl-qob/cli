;;; lisp/_color.lisp --- Color module
;;; Commentary:
;;; Code:

(defvar qob-enable-color t
  "Set to nil to disable color.")

(defun qob-color-code (color)
  "Return the ANSI color code by COLOR."
  (ecase color
    (:gray    "38;5;8")
    (:black   30)
    (:red     31)
    (:green   32)
    (:yellow  33)
    (:blue    34)
    (:magenta 35)
    (:cyan    36)
    (:white   37)))

(defun qob-color-it (color str)
  "COLOR the STR."
  (check-type color keyword)
  (if qob-enable-color
      (format nil "~C[~Am~A~C[0m"
              #\Esc (qob-color-code color) str #\Esc)
      str))

(defun qob-ansi-black (str) (qob-color-it :black str))
(defun qob-ansi-red (str) (qob-color-it :red str))
(defun qob-ansi-green (str) (qob-color-it :green str))
(defun qob-ansi-yellow (str) (qob-color-it :yellow str))
(defun qob-ansi-blue (str) (qob-color-it :blue str))
(defun qob-ansi-magenta (str) (qob-color-it :magenta str))
(defun qob-ansi-cyan (str) (qob-color-it :cyan str))
(defun qob-ansi-white (str) (qob-color-it :white str))

;;; End of lisp/_color.lisp
