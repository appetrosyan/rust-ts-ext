;;; rust-ts-ext -- Tree-sitter extensions for Rust  -*- lexical-binding: t; -*-

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
(require 'rust-ts-mode)

(defsubst rust-ts-ext-at-indentation-p ()
  "Return t if at indentation."
  (interactive "P")
  ;; TODO: consider making these more efficient
  (string-match-p "^\\s-+$" (buffer-substring-no-properties (line-beginning-position) (point))))

(defsubst treesit-node-inside-p (node type)
  "Recurse up the tree of nodes to quickly check if a NODE is of TYPE and return the node."
  (cond ((null node) nil)
		((string= type (treesit-node-type node)) node)
		(node (treesit-node-inside-p (treesit-node-parent node) type))))

;; TODO: This looks like a use-case for `cond*'.
;; TODO: This does not handle import statement structures.
(defun rust-ts-ext-insert-impl-block-dwim ()
  "Add impl block in Rust code."
  (interactive)
  (let*
	  ((node
		(or (treesit-node-inside-p (treesit-node-at (point)) "struct_item")
			(treesit-node-inside-p (treesit-node-at (point)) "enum_item")))
	   (name-of-struct
		(treesit-node-text (treesit-node-child-by-field-name node "name")))
	   (type-parameters
		(treesit-node-text (treesit-node-child-by-field-name node "type_parameters"))))
	(cond
	 ((treesit-node-inside-p (treesit-node-at (point)) "impl_item")
	  (message "TODO: Find the name of struct and identify the struct"))
	 (name-of-struct
	  (treesit-end-of-defun 1)
	  (newline)
	  (insert "impl")
	  (when type-parameters (insert type-parameters))
	  (insert " ")
	  (insert name-of-struct)
	  (when type-parameters (insert type-parameters))
	  (insert " {\n\n}\n")
	  (previous-line 2)
	  (indent-for-tab-command))
	 (t
	  (unless (eolp) (end-of-line) (newline))
	  (newline-and-indent)
	  (insert "impl ")
	  (save-excursion (insert " {\n\n}\n"))
	  (complete-symbol nil)))))

(defvar rust-ts-ext-default-error-type
  "Box<dyn std::error::Error>"
  "The type that is used as a placeholder for `rust-ts-ext-ok'.  If in doubt, just use `Box<dyn std::error::Error>`.")

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
the final expression position, `insert' `Some`, the opening and closing parens, and that is it."
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
	  (funcall-interactively 'rust-ts-ext-insert-fn "fallible_function" "" (format "Result<(), %s>" rust-ts-ext-default-error-type)))))


(defun rust-ts-ext-rename()
  "Rename the structure item if looking at a structure item.

Rename enumeration if looking at an enumeration."
  (interactive)
  (let ((fn-item (treesit-node-inside-p (treesit-node-at (point)) "function_item")))
	(goto-char (treesit-node-start (treesit-node-child-by-field-name fn-item "name")))
	(mark-sexp)))

(defun rust-ts-ext-pub ()
  "Cycle through visibility modifiers for Rust items."
  (interactive)
  (save-excursion
	;; FIXME: When inside enum with tuple elements the pub should **not** apply to the tuple elements, it should go inside.
	;; FIXME: When function inside an impl trait block, don't add pub.
	;; FIXME: Want to increment the publicity level.
	;; FIXME: Separate function for private
	(back-to-indentation)
	(if (looking-at "pub(crate) ")
		(progn (replace-match "") (indent-for-tab-command))
	  (if (looking-at "pub")
		  (progn (forward-word) (insert "(crate)"))
		(insert "pub ")))))

(defun rust-ts-ext-insert-anonymous-lifetime()
  "Add an anonymous lifetime to the current node."
  (interactive)
  (goto-char (treesit-node-end (treesit-node-at (point))))
  (insert "<'_>"))

(defun rust-ts-ext-insert-fn (&optional fn-name arguments return)
  "Add fn item with the name FN-NAME(ARGUMENTS) -> RETURN."
  (interactive)
  (when (or (bolp) (rust-ts-ext-at-indentation-p))
	(let ((function-name (if fn-name fn-name "function"))
		  (return-clause (if return (concat " -> " return) "")))
	  (progn
		(insert "fn ")
		(indent-for-tab-command)
		(insert (format "%s(%s)%s {\n" function-name (or arguments "") return-clause))
		(save-excursion
		  (indent-for-tab-command)
		  (insert "todo!()\n")
		  (insert "}\n"))
		(indent-for-tab-command)))))

(defun rust-ts-ext--impl-type-name (impl-node)
  "Extract the base type name from IMPL-NODE."
  (let ((type-node (treesit-node-child-by-field-name impl-node "type")))
	(pcase (treesit-node-type type-node)
	  ("generic_type"
	   (treesit-node-text (treesit-node-child-by-field-name type-node "type")))
	  (_ (treesit-node-text type-node)))))

(defun rust-ts-ext--find-new-fn (type-name)
  "Find a `new' function in an inherent impl block for TYPE-NAME."
  (let (result)
	(dolist (node (treesit-node-children (treesit-buffer-root-node) t))
	  (when (and (string= (treesit-node-type node) "impl_item")
				 (not (treesit-node-child-by-field-name node "trait"))
				 (string= (rust-ts-ext--impl-type-name node) type-name))
		(dolist (child (treesit-node-children
						(treesit-node-child-by-field-name node "body") t))
		  (when (and (string= (treesit-node-type child) "function_item")
					 (string= (treesit-node-text
							   (treesit-node-child-by-field-name child "name"))
							  "new"))
			(setq result child)))))
	result))

(defun rust-ts-ext--non-self-params (fn-node)
  "Return the list of non-self parameter nodes of FN-NODE."
  (let ((params (treesit-node-child-by-field-name fn-node "parameters"))
		(result '()))
	(dolist (child (treesit-node-children params t))
	  (when (string= (treesit-node-type child) "parameter")
		(push child result)))
	(nreverse result)))

(defun rust-ts-ext--default-body-for-fields (body-node prefix)
  "Generate Default::default() assignments for fields in BODY-NODE.
PREFIX is prepended (e.g. \"Self\" or \"Self::VariantName\")."
  (let ((body-type (and body-node (treesit-node-type body-node))))
	(pcase body-type
	  ("field_declaration_list"
	   (let ((lines (list (concat prefix " {\n"))))
		 (dolist (child (treesit-node-children body-node t))
		   (when (string= (treesit-node-type child) "field_declaration")
			 (push (concat (treesit-node-text
							(treesit-node-child-by-field-name child "name"))
						   ": Default::default(),\n")
				   lines)))
		 (push "}\n" lines)
		 (apply #'concat (nreverse lines))))
	  ("ordered_field_declaration_list"
	   (let ((count 0))
		 (dolist (child (treesit-node-children body-node t))
		   (when (string= (treesit-node-type child) "ordered_field_declaration")
			 (setq count (1+ count))))
		 (concat prefix "("
				 (mapconcat #'identity (make-list count "Default::default()") ", ")
				 ")\n")))
	  (_ (concat prefix "\n")))))

(defun rust-ts-ext-insert-default ()
  "Insert an `impl Default' block for the struct or enum at point.

When an inherent impl block with `new' exists:
- If `new' takes no arguments (besides self), delegate to Self::new().
- If `new' takes arguments, fill them with Default::default().

When no `new' is found:
- For structs, generate a Self literal with Default::default() for each field.
- For enums, use the first variant as the default."
  (interactive)
  (let* ((node (or (treesit-node-inside-p (treesit-node-at (point)) "struct_item")
				   (treesit-node-inside-p (treesit-node-at (point)) "enum_item")))
		 (node-type (and node (treesit-node-type node)))
		 (name (and node (treesit-node-text
						  (treesit-node-child-by-field-name node "name"))))
		 (type-parameters
		  (and node (treesit-node-text
					 (treesit-node-child-by-field-name node "type_parameters"))))
		 (new-fn (and name (rust-ts-ext--find-new-fn name)))
		 (new-params (and new-fn (rust-ts-ext--non-self-params new-fn))))
	(unless node (user-error "Not inside a struct or enum"))
	(goto-char (treesit-node-end node))
	(let ((start (point)))
	  (insert "\n\nimpl")
	  (when type-parameters (insert type-parameters))
	  (insert " Default for " name)
	  (when type-parameters (insert type-parameters))
	  (insert " {\nfn default() -> Self {\n")
	  (cond
	   ;; new() with zero non-self args -> delegate
	   ((and new-fn (null new-params))
		(insert "Self::new()\n"))
	   ;; new() with args -> Default::default() for each
	   (new-fn
		(insert "Self::new("
				(mapconcat (lambda (_) "Default::default()") new-params ", ")
				")\n"))
	   ;; Struct without new()
	   ((string= node-type "struct_item")
		(insert (rust-ts-ext--default-body-for-fields
				 (treesit-node-child-by-field-name node "body") "Self")))
	   ;; Enum without new() -> first variant
	   ((string= node-type "enum_item")
		(let* ((enum-body (treesit-node-child-by-field-name node "body"))
			   (variant nil))
		  (dolist (child (treesit-node-children enum-body t))
			(when (and (null variant)
					   (string= (treesit-node-type child) "enum_variant"))
			  (setq variant child)))
		  (if variant
			  (insert (rust-ts-ext--default-body-for-fields
					   (treesit-node-child-by-field-name variant "body")
					   (concat "Self::" (treesit-node-text
										 (treesit-node-child-by-field-name variant "name")))))
			(insert "todo!()\n")))))
	  (insert "}\n}\n")
	  (indent-region start (point)))))

(provide 'rust-ts-ext)
;;; rust-ts-ext.el ends here
