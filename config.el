;;; .doom.d/config.el -*- lexical-binding: t; -*-

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


(after! super-save
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 8)
  (super-save-mode +1))
