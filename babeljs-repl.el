;;; babeljs-repl.el --- Run babeljs REPL

;; Copyright (C) 2015-2015 Hung Phan

;; Author: Hung Phan
;; Version: See `babel-repl-version'

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program is derived from comint-mode and provides the following features.
;;
;;  * TAB completion same as Babel.js REPL
;;  * file name completion in string
;;  * incremental history search
;;
;;
;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:
;;
;;    (require 'babeljs-repl)
;;
;; Type M-x babel-repl to run Babel.js REPL.
;; See also `comint-mode' to check key bindings.
;;

(require 'comint)

(defconst babel-repl-version "0.0.1"
  "Babel.js mode Version.")

(defvar babel-cli-program "babel-node"
  "Start babel-node repl for compile es6 syntax")

(defvar babel-cli-arguments '()
  "List of command line arguments to pass to babel shell cli programm")

(defvar babel-pop-to-buffer nil
  "Whether to pop up the babel shell buffer after sending command to execute")

(defvar babel-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the babel shell buffer")

(define-derived-mode babel-shell-mode comint-mode "Babel Shell"
  "Major mode for `babel-node'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t))

(defun babel-pop-to-buffer ()
  "Pop the babel shell buffer to the current window"
  (apply babel-pop-to-buffer-function '("*babel-shell*")))

;;; Taken from masteringemacs with some changes
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun babel-repl ()
  "Start babel shell comint mode"
  (interactive)
  (let ((buffer (comint-check-proc "*babel-shell*")))
    ;; pop to the "*babel-shell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'babel-shell-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create "*babel-shell*")
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "babel-shell" nil babel-cli-program nil
             babel-cli-arguments)
      (babel-shell-mode))))

;;; Send the query string to babel shell to execute
(defun babel-send-string (string)
  "Send the input string to babel shell process"
  (if (not (comint-check-proc "*babel-shell*"))
      (message "No babel shell process started")
    (progn
      (process-send-string "*babel-shell*" (concat string "\n"))
      (when babel-pop-to-buffer
        (babel-pop-to-buffer)))))

(defun babel-send-region (beg end)
  "Send the region from beg to end to babel process"
  (let ((string (buffer-substring-no-properties beg end)))
    (babel-send-string string)))

(defun babel-send-current-region ()
  "Send the selected region to babel shell process"
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (babel-send-region beg end)))

(defun babel-send-buffer ()
  "Send the current buffer to babel shell process"
  (interactive)
  (let* ((beg (point-min))
         (end (point-max)))
    (babel-send-region beg end)))

(defun babel-send-paragraph ()
  "Send the current paragraph to babel shell process"
  (interactive)
  (let ((beg (save-excursion
               (backward-paragraph)
               (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (babel-send-region beg end)))

(defun babel-send-region-or-buffer ()
  "Send the selected region if presented, otherwise, send the whole buffer"
  (interactive)
  (if (use-region-p)
      (babel-send-current-region)
    (babel-send-buffer)))

(defun babel-send-dwim ()
  "Send the selected region presented, otherwise, send the current paragraph"
  (interactive)
  (if (use-region-p)
      (babel-send-current-region)
    (babel-send-paragraph)))

(defun babel-switch-to-buffer ()
  "Switch to babel shell buffer"
  (interactive)
  (if (comint-check-proc "*babel-shell*")
      (switch-to-buffer "*babel-shell*")
    (babel-repl)))

(provide 'babeljs-repl)
