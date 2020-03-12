;;; evil-tex-util.el -- Functions to be used by evil-texi -*- lexical-binding: t; -*-
;;; Commentary:
;; This file is part of evil-tex, which provides license
;; information.
;;; Code:

(require 'latex)
(require 'dash)

;; Stuff from evil-latex-textobjects

(defun evil-tex-macro-beginning-begend ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \\macro{foo|bar} it returns the start and end of \"\\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                  ; backslash
        (skip-chars-forward "A-Za-z@*") ; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))               ; opening brace
        (cons beg (point))))))

(defun evil-tex-macro-end-begend ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]" (- (point) 2))
          (backward-char))              ; closing brace
        (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

;; Now ours

(defun evil-tex--replace-region (region string)
  "Replace the region between REGION with STRING.
REGION should be a (beg . end) cons."
  (save-excursion
    (delete-region (car region) (cdr region))
    (goto-char (car region))
    (insert string)))

(defun evil-tex--re-search-backwards-unless-already (str)
  "Search backward for STR unless point is already on it."
  (unless (looking-at str)
    (re-search-backward str)))

(defun evil-tex--get-macro-braces-begend ()
  "Return (beg . end) of the macro braces currently active."
  (let (beg end)
    (save-excursion
      (evil-tex--re-search-backwards-unless-already "\\\\")
      (search-forward "{")
      (setq beg (point))
      (forward-sexp)
      (setq end (point)))
    (cons beg end)))

(defun evil-tex-env-beginning-begend ()
  "Return (start . end) of the \\begin{foo} of current env.

\\begin{equation}
^               ^"
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)      ; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")        ; goto opening brace
      (forward-sexp)                   ; goto closing brace
      ;; Count the newline after \begin{foo} to the environment header
      ;; Without this, delete-inner-env would unexpectedly move the end
      ;; to the same line as the beginning
      ;; (when (looking-at "[[:blank:]]*$")
      ;;   (message "Newline")
      ;;   (forward-line 1))
      (cons beg (point)))))

(defun evil-tex-env-end-begend ()
  "Return (start . end) of the \\end{foo} of current env.

\\end{equation}
^             ^"
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)        ; we are at closing brace
      (setq end (point))
      (backward-sexp)                  ; goto opening brace
      (search-backward "\\")           ; goto backslash
      (cons (point) end))))


(defun evil-tex--begin-braces-begend-begend ()
  "Return (beg . end) of the argument given to \\begin in the current env."
  (interactive)
  (save-excursion
    (LaTeX-find-matching-begin)
    (evil-tex--get-macro-braces-begend)))

(defun evil-tex--end-braces-begend ()
  "Return (beg . end) of the argument given to \\end in the current env."
  (interactive)
  (save-excursion
    (LaTeX-find-matching-end)
    (evil-tex--get-macro-braces-begend)))

(defun evil-tex-change-env (new-env)
  "Change current env to NEW-ENV."
  (evil-tex--replace-region (evil-tex--begin-braces-begend-begend) new-env)
  (evil-tex--replace-region (evil-tex--end-braces-begend) new-env))

(provide 'evil-tex-util)
;;; evil-tex-util.el ends here
