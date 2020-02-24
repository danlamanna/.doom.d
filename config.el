;;; .doom.d/config.el -*- lexical-binding: t; -*-
;;;
(defun dl-initialize-project-vars()
  (when (string-prefix-p (expand-file-name "~/p/stumpf-diva") default-directory)
    (setq-local
     flycheck-python-mypy-executable  (expand-file-name "~/p/stumpf-diva/.tox/mypy/bin/mypy")
     flycheck-python-mypy-ini (expand-file-name "~/p/stumpf-diva/setup.cfg")
     flycheck-python-flake8-executable  (expand-file-name "~/p/stumpf-diva/.tox/lint/bin/flake8")
     flycheck-flake8rc (expand-file-name "~/p/stumpf-diva/setup.cfg"))
    (flycheck-select-checker 'python-flake8)
    (flycheck-select-checker 'python-mypy)))

(add-hook 'python-mode-hook 'dl-initialize-project-vars)


(defun dl-jump-to-definition()
  (interactive)
  (if (lsp-request "textDocument/definition" (lsp--text-document-position-params))
      (lsp-find-definition)
    (dumb-jump-go)))



(map! :n "z a" 'dl-jump-to-definition)

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
  (setq company-show-numbers t
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(after! avy
  (setq avy-all-windows t
        avy-style 'pre))

;; (use-package smooth-scrolling
;;   :config (progn
;;             (setq smooth-scroll-margin 20)
;;             (smooth-scrolling-mode 1)))

;; expand region
;; key chords
;; dumb jump

;; contextual grep
;; deadgrep improvements

(map! :n "z z" 'evil-avy-goto-char-timer)
(map! :n "z s" 'swiper)
(map! :n "z r" 'er/expand-region)
(map! :n "z f" 'projectile-find-file-other-window)

(after! company-lsp
  (setq company-lsp-cache-candidates 'auto)
  (add-to-list 'company-lsp-filter-candidates '(mspyls . t))
  (defun company-lsp--on-completion (response prefix)
    "Handle completion RESPONSE.
PREFIX is a string of the prefix when the completion is requested.
Return a list of strings as the completion candidates."
    (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
           (items (cond ((hash-table-p response) (gethash "items" response))
                        ((sequencep response) response)))
           (candidates (mapcar (lambda (item)
                                 (company-lsp--make-candidate item prefix))
                               (lsp--sort-completions items)))
           (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
           (should-filter (or (eq company-lsp-cache-candidates 'auto) ; change from t to 'auto
                              (and (null company-lsp-cache-candidates)
                                   (company-lsp--get-config company-lsp-filter-candidates server-id)))))
      (when (null company-lsp--completion-cache)
        (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
        (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
      (when (eq company-lsp-cache-candidates 'auto)
        ;; Only cache candidates on auto mode. If it's t company caches the
        ;; candidates for us.
        (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
      (if should-filter
          (company-lsp--filter-candidates candidates prefix)
        candidates))))

(use-package! super-save
  :config (progn
            (setq super-save-auto-save-when-idle nil)
            (super-save-mode +1)))
