;;; refactor-clojure.el --- refactoring Clojure code

;;; Copyright (2013) Sergey Astanin

;; Author: Sergey Astanin
;; Package-Requires: ((paredit "20130722.1324") (multiple-cursors "1.2.2"))
;; Version: 0.1
;; URL: http://github.com/astanin/refactor-clojure
;; License: MIT

;;; Commentary:

;; User-visible functions:
;;
;; (refactor-clojure-extract-variable) -- extracts an S-expression and
;; binds it to a variable in the let-form above.
;;

;;; Code goes here

(require 'multiple-cursors)
(require 'paredit)


(setq refactor-clojure--original-position nil)
(make-variable-buffer-local 'refactor-clojure--original-position)

;; (save-excursion) doesn't cut it as we want to restore the original
;; position only _after_ the user leaves multiple-cursors-mode, and
;; not before
(defun refactor-clojure--remember-original-position ()
  "Remember a position to jump back to after the refactoring."
  ;; counting positions from behind makes it invariant with respect to
  ;; insertions above (let-clause).
  (setq refactor-clojure--original-position (- (point-max) (point))))

(defun refactor-clojure--goto-original-position ()
  "Jump back to the original point where the refactoring started."
  (let ((original-point (+ (- (point-max)
                              refactor-clojure--original-position
                              1 ; an extra closing parenthesis
                              )
                           (length (car kill-ring-yank-pointer)))))
    (goto-char original-point)
    (paredit-backward)
    (remove-hook 'multiple-cursors-mode-disabled-hook
                 'refactor-clojure--goto-original-position
                 'buffer-local)))

(defun refactor-clojure--create-new-let-bindings ()
  "Wrap an S-expression under cursor with new let-bindings.
  Use the last killed string as an initialization expression.
  Enable multiple (twin) cursors to enter variable name.
  "
  ;; create a let-expression around
  (paredit-wrap-round)
  (insert "let []")
  (paredit-newline)
   ;; back into the let-bindings vector
  (paredit-backward)
  (forward-char)
  (yank)
  (paredit-backward)
  (insert " ")
  (backward-char))

(defun refactor-clojure--append-to-let-bindings ()
  "Append new bindings to an existing let-form under cursor.
  Use the last killed string as an initialization expression.
  Enable multiple (twin) cursors to enter variable name.
  "
  (paredit-forward-down) ; "let" under the cursor
  (paredit-forward 2) ; bindings's vector [] behind the cursor
  (paredit-backward-down) ; cursor after the last binding
  (paredit-newline)
  (yank)
  (paredit-backward)
  (insert " ")
  (backward-char))

(defun refactor-clojure-extract-variable (&optional levels)
  "Extract an S-expression under cursor and binds it to a
  variable in a let-form. With `C-u' prefix argument N, insert
  the bindings N levels up.  Without prefix argument, insert the
  bindings one level above.

  If there is already a let-form above, the bindings are appended
  to the existing form. Otherwise a new let-form is inserted."
  (interactive (list (if current-prefix-arg current-prefix-arg 1)))
  ;; kill the expression under cursor
  (refactor-clojure--remember-original-position)
  (mc/create-fake-cursor-at-point)
  (kill-sexp)
  ;; go `levels` levels upwards
  (paredit-backward-up levels)
  ;; add variable bindings
  (if (string= "(let "
               (buffer-substring-no-properties (point) (+ (point) 5)))
      (refactor-clojure--append-to-let-bindings)
    (refactor-clojure--create-new-let-bindings))
  ;; active a multiple-cursors mode, and jump back after it's over
  (add-hook 'multiple-cursors-mode-disabled-hook
            'refactor-clojure--goto-original-position
            nil 'buffer-local)
  (multiple-cursors-mode))

(provide 'refactor-clojure)


;;; refactor-clojure.el ends here
