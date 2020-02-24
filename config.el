;;; .doom.d/config.el -*- lexical-binding: t; -*-
;;;
(after! python
  (defun dl-initialize-project-vars()
    (when (or (f-equal? default-directory "~/p/stumpf-diva")
              (f-child-of? default-directory "~/p/stumpf-diva"))
      (setq
       flycheck-python-mypy-executable  "~/p/stumpf-diva/.tox/mypy/bin/mypy"
       flycheck-python-flake8-executable  "~/p/stumpf-diva/.tox/lint/bin/flake8"
       flycheck-enabled-checkers '(python-flake8 python-mypy))))

  (add-hook 'python-mode-hook 'dl-initialize-project-vars))

(after! dap-mode
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  (setq dap-auto-show-output nil)

  (dap-register-debug-template "stumpf-diva"
                               (list
                                :name "Python: Flask"
                                :type "python"
                                :request "launch"
                                :module "flask"
                                :cwd "~/p/stumpf-diva"
                                :env '(("FLASK_APP" . "stumpf.wsgi")
	                                     ("FLASK_ENV" . "development")
	                                     ("FLASK_DEBUG" . "0"))
                                :args (concat
	                                     "run"
	                                     " --no-debugger"
	                                     " --no-reload"))))

(after! magit
  (setq magit-repository-directories '(( "~/p" . 4 )))

  ;; when switching to magit-status save everything
  (setq magit-save-repository-buffers 'dontask
        magit-delete-by-moving-to-trash nil)

  (transient-append-suffix 'magit-push '(0 -1)
    '("+c" "Create merge-request"
      "--push-option=merge_request.create"))

  (transient-append-suffix 'magit-push '(0 -1)
    '("+d" "Against master"
      "--push-option=merge_request.target=master"))

  (transient-append-suffix 'magit-push '(0 -1)
    '("+m" "Merge on success"
      "--push-option=merge_request.merge_when_pipeline_succeeds")))

(after! company
  (setq company-show-numbers t))

;; (spacemacs/set-leader-keys "]y" '(lambda()
;;                                    (interactive)
;;                                    (counsel-projectile-rg "--type yaml")))
;; (spacemacs/set-leader-keys "]p" '(lambda()
;;                                    (interactive)
;;                                    (counsel-projectile-rg "--type py")))


(use-package! super-save
  :config (progn
            (setq super-save-auto-save-when-idle t)
            (setq super-save-idle-duration 8)
            (super-save-mode +1)))
