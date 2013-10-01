;;; refactor-clojure.el --- refactoring Clojure code

;;; Copyright (2013) Sergey Astanin

;; Author: Sergey Astanin
;; Package-Requires: ((paredit "20130722.1324") (multiple-cursors "1.2.2"))
;; Version: 0.2
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


(setq rc/original-position nil)
(make-variable-buffer-local 'rc/original-position)

;; (save-excursion) doesn't cut it as we want to restore the original
;; position only _after_ the user leaves multiple-cursors-mode, and
;; not before
(defun rc/remember-original-position ()
  "Remember a position to jump back to after the refactoring."
  ;; counting positions from behind makes it invariant with respect to
  ;; insertions above (let-clause).
  (setq rc/original-position (- (point-max) (point))))

(defun rc/goto-original-position ()
  "Jump back to the original point where the refactoring started."
  (let ((original-point (+ (- (point-max)
                              rc/original-position
                              1 ; an extra closing parenthesis
                              )
                           (length (car kill-ring-yank-pointer)))))
    (goto-char original-point)
    (paredit-backward)
    (remove-hook 'multiple-cursors-mode-disabled-hook
                 'rc/goto-original-position
                 'buffer-local)))

(defun rc/create-new-let-bindings ()
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

(defun rc/append-to-let-bindings ()
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

;; TODO: leave the fake cursor in the new location and the main in the original
(defun refactor-clojure-extract-variable (&optional levels)
  "Extract an S-expression under cursor and binds it to a
  variable in a let-form. With `C-u' prefix argument N, insert
  the bindings N levels up.  Without prefix argument, insert the
  bindings one level above.

  If there is already a let-form above, the bindings are appended
  to the existing form. Otherwise a new let-form is inserted."
  (interactive (list (if current-prefix-arg current-prefix-arg 1)))
  ;; kill the expression under cursor
  (rc/remember-original-position)
  (mc/create-fake-cursor-at-point)
  (kill-sexp)
  ;; go `levels` levels upwards
  (paredit-backward-up levels)
  ;; add variable bindings
  (if (string= "(let "
               (buffer-substring-no-properties (point) (+ (point) 5)))
      (rc/append-to-let-bindings)
    (rc/create-new-let-bindings))
  ;; active a multiple-cursors mode, and jump back after it's over
  (add-hook 'multiple-cursors-mode-disabled-hook
            'rc/goto-original-position
            nil 'buffer-local)
  (multiple-cursors-mode))

(defmacro rc/with-move (&rest body)
  `(save-excursion
     (ignore-errors
       ,@body)))

(defun rc/form-toplevel? ()
  "True if the current form is at the top level."
  (let ((p (point))
        (up (rc/with-move (paredit-backward-up) (point))))
    (eql p up)))

(defun rc/form-type ()
  "Guess the type of the current form as a symbol. May return nil."
  (let ((c (char-after))
        (next-c (char-after (+ 1 (point)))))
    (cond
     ((eq c ?\() 'compound)
     ((eq c ?\[) 'vector)
     ((eq c ?\") 'string)
     ((eq c ?\{) 'map)
     ((eq c ?^)  'meta)
     ((eq c ?')  'quote)
     ((eq c ?:)  'keyword)
     ((eq c ?`)  'syntax-quote)
     ((eq c ?#)  (cond
                  ((eq next-c ?\") 'regexp)
                  ((eq next-c ?\{) 'set)
                  ((eq next-c ?')  'var-quote)))
     ((and (<= ?A c) (<= c ?z)) 'symbol)
     ((and (<= ?0 c) (<= c ?9)) 'number))))

(defun rc/form-fn-name ()
  "For a compound form, return an expression in the function position as a string.
  May return nil."
  (when (eq 'compound (rc/form-type))
    (buffer-substring-no-properties
     (rc/with-move
      (paredit-forward-down)
      (point))
     (rc/with-move
       (paredit-forward-down)
       (paredit-forward)
       (point)))))

(defun rc/form-as-string ()
  (buffer-substring-no-properties
     (point) (save-excursion
               (paredit-forward)
               (point))))

(defun rc/form-symbol-name ()
  "For a symbol form, return its name as a string. May return nil."
  (when (eq 'symbol (rc/form-type))
    (rc/form-as-string)))

(defun rc/beginning-of-next-expr ()
  "Move the point to the beginning of the next expression within a form."
  (paredit-forward)
  (skip-chars-forward " \t\r\n"))

(setq rc/clojure-special-symbols
      '(def if do let quote var fn loop recur
        throw try monitor-enter monitor-exit))

(defun rc/form-special? ()
  "True if the current form is a Clojure special form."
  (let ((type (rc/form-type))
        (name (rc/form-fn-name)))
    (when (and (eq type 'compound) name)
      (memq (intern name) rc/clojure-special-symbols))))

(defun rc/form-plain-symbol? ()
  "True if the current form is a Clojure symbol."
  (and (eq 'symbol (rc/form-type))
       (not (memq (intern (rc/form-symbol-name)) rc/clojure-special-symbols))))

(defun rc/form-compound? ()
  "True if the current form is of one of the compound
  types (vectors, maps and sets inclusive)."
  (memq (rc/form-type) '(compound vector map set)))

(defun rc/form-end-position ()
  (rc/with-move (paredit-forward) (- (point) 1)))

(setq rc/form-used-vars 'undefined)  ; forward declaration

(defun rc/compound-form-used-vars ()
  "Build a list of variables refered to from a compound form."
  (assert (rc/form-compound?))
  (save-excursion
    (let ((vars '())                     ; accumulator
          (end-of-form   (rc/form-end-position)))
      (paredit-forward-down)             ; step into the compound form
      (while (< (point) end-of-form)
        (let ((sub-vars (rc/form-used-vars)))
          (when sub-vars
            (setq vars (append vars sub-vars))))
        (rc/beginning-of-next-expr))
      vars)))

(defun rc/set-difference (a b)
  "Set difference A \ B using `equal`."
  (remove-if (lambda (v) (member v b)) a))

(defun rc/bindings-new-and-used-vars ()
  ;; TODO: support positional destructuring
  ;; TODO: support associative destructuring
  ;; TODO: support :as in positional and associative destructuring
  ;; TODO: support :keys in associative destructuring
  ;; TODO: support :syms in associative destructuring
  ;; TODO: support :strs in associative destructuring
  (assert (eq 'vector (rc/form-type)))
  (save-excursion
    (let ((used-vars '())
          (all-new-vars  '())
          (current-new-vars '())
          (end-of-vector  (rc/form-end-position)))
      (paredit-forward-down)             ; step-into
      (while (< (point) end-of-vector)
        ;; newly bound variables
        (let ((sub-vars (rc/form-used-vars))) ; FIXME: support destructuring
          (setq current-new-vars sub-vars))
        ;; skip metadata if any and go to the initializer
        (rc/beginning-of-next-expr)
        (while (eq 'meta (rc/form-type))
          (rc/beginning-of-next-expr))
        ;; referred variables
        (let* ((sub-vars (rc/form-used-vars))
               (actually-used-vars (rc/set-difference sub-vars all-new-vars)))
          (setq used-vars (append used-vars actually-used-vars)))
        ;; current new vars may now be referred to in the subsequent bindings
        (setq all-new-vars (append all-new-vars current-new-vars))
        (rc/beginning-of-next-expr))
      `((used-vars . ,used-vars)
        (new-vars . ,all-new-vars)))))

(defun rc/let-form-used-vars ()
  (when (equal "let" (rc/form-fn-name))
    (save-excursion
      (let ((end-of-let (rc/form-end-position)))
        (paredit-forward-down)      ; step into
        (rc/beginning-of-next-expr) ; go to the bindings vector
        (assert (eq 'vector (rc/form-type)))
        (let* ((bvars (rc/bindings-new-and-used-vars))
               (let-used-vars (cdr (assoc 'used-vars bvars)))
               (let-new-vars  (cdr (assoc 'new-vars bvars))))
          (rc/beginning-of-next-expr)
          (while (< (point) end-of-let)
            (let* ((sub-vars (rc/form-used-vars))
                   (free-vars (rc/set-difference sub-vars let-new-vars)))
              (when free-vars
                (setq let-used-vars (append let-used-vars free-vars))))
              (rc/beginning-of-next-expr))
          let-used-vars)))))

(defun rc/form-used-vars ()
  "Build a list of variables which the current form refers to."
  (save-excursion
    (cond
     ((rc/form-toplevel?)
      '())
     ((and (rc/form-special?) (equal "let" (rc/form-fn-name)))
      (rc/let-form-used-vars))         ; deal with bindings
     ((rc/form-special?)
      (cdr (rc/compound-form-used-vars))) ; discard the special symbol
     ((rc/form-compound?)
      (rc/compound-form-used-vars))    ; all symbols used in the form
     ((rc/form-plain-symbol?)
      `(,(rc/form-symbol-name))        ; a singleton list
      ))))

(provide 'refactor-clojure)


;;; refactor-clojure.el ends here
