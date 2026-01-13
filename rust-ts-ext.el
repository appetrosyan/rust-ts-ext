;;; rust-ts-ext -- Tree-sitter extensions for Rust

;;; Commentary:
;; The Tree-sitter allows limited forms of parsing of the code of the
;; program that is being written.  While one is commonly advised to use
;; language servers.  While those are valuable, they are often much too
;; complex for the kind of parsing that is needed to do accurate code
;; editing.  And with the exception of `gopls' are also extremely slow.

;; As such I thought it'd be useful to create a version of custom-made
;; elisp-only solutions to editing Rust code.  It is smart about it and
;; leverages tree-sitter to do operations that would otherwise be too
;; expensive.  The tree-sitter grammar is eventually going to become the
;; standard, the information is already there, why not use it.

;;; Code:
(defun rust-ts-ext-insert-impl-block ()
  "Insert an `impl' block at point."
  (interactive)
  (let
      ((name-of-struct
		(treesit-node-text
         (treesit-node-child-by-field-name
          (treesit-node-inside-p
		   (treesit-node-at (point))
		   "struct_item")
		  "name")
		 t)))
    (cond
     (name-of-struct
      (treesit-end-of-defun 1)
      (newline)
      (insert "impl ")
      (insert name-of-struct))
     ((and (or (bolp) (my:at-indentation))
           (not (treesit-node-inside-p (treesit-node-at (point)) "function_item")))
      (insert "impl ") (company-complete))
     (t (self-insert-command 1)))))

(defvar rust-ts-ext-default-error-type
  "Box<dyn std::error::Error>"
  "The type that is used as a placeholder for `my:rust:ok'.  If in doubt, just use `Box<dyn std::error::Error>`.")

(defun rust-ts-ext-insert-ok-dwim ()
  "Add OK at the end of the function call.

If the point is between function items and not inside one,
we insert a fallible function scaffold.

If the point is inside the function body; then one of the following will
happen.
- If the function has no return type, insert `Result<(), Box<dyn std::error::Error>>`
and `Ok(())` as the final expression in the function body.
- If the function has a return type `T`, we change the return type to `Result<T, Box<dyn std::error::Error>>`,
and wrap the final expression in `Ok`.  If we don't have a final expression, but a `return` statement,
we go into the `return` statement, and wrap its expression in `Ok`.
- If the function already has a return type of `Result` or `Option`, we move the point into
the final expression position, `insert' `Some`, the opening and closing parens, and that is it.
"
  (interactive)
  (let ((fn-item (treesit-node-inside-p (treesit-node-at (point)) "function_item")))
	(if fn-item
		(progn
		  (back-to-indentation)			;TODO: Figure out the body location
		  (if (or (looking-at "Ok") (= (point) (treesit-node-start fn-item)))
			  (let* ((ret (treesit-node-child-by-field-name fn-item "return_type"))
					 (beg (treesit-node-start ret))
					 (end (treesit-node-end ret)))
				(goto-char beg)
				(if (looking-at "Result")
					(goto-char (match-end 0))
				  (goto-char end)
				  (insert ", ")
				  (insert rust-ts-ext-default-error-type)
				  (insert ">")
				  (goto-char beg)
				  (insert "Result<")))
			(save-excursion
			  (insert "Ok(")
			  (when (eolp) (insert "()"))
			  (end-of-visual-line)
			  (backward-up-list)
			  (forward-sexp)
			  (insert ")"))))
	  (funcall-interactively 'my:rust:fn "fallible_function" "" "Result<(), Box<dyn std::error::Error>>"))))

;;; rust-ts-ext.el ends here
