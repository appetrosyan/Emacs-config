
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)			

(setq gc-cons-threshold 400000000)
;;; Load the config
(defun reload-config ()
  (interactive)
  (org-babel-load-file (concat user-emacs-directory "config.org")
		       )
  )

(defun reconfigure ()
  (interactive)
  (find-file (concat user-emacs-directory "config.org")
	     )
  )

(reload-config)

(setq gc-cons-threshold 800000)		
