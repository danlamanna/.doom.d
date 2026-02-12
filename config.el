;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(when noninteractive
(add-to-list 'doom-env-whitelist "^SSH_"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;(setq doom-theme 'doom-challenger-deep)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; https://discourse.doomemacs.org/t/why-is-my-path-being-shown-as-an-ellipsis-and-how-do-i-reveal-it/3363
(advice-add '+emacs-lisp-truncate-pin :override (lambda () ()) )

(use-package! super-save
  :demand t)

(after! super-save
  (setq super-save-auto-save-when-idle t)
  (setq super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-mode +1))


;; Load copilot whitelist configuration if it exists
(load (expand-file-name "~/.config/doom/copilot-whitelist.el") t)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook copilot-mode
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  ;; Suppress only the popup for copilot indentation warnings (still logs them)
  (add-to-list 'warning-suppress-types 'copilot--infer-indentation-offset)

  ;; Safe projects file
  (defvar copilot-safe-projects-file
    (expand-file-name ".copilot-safe-projects" doom-user-dir))

  ;; Check if current project is safe
  (defun copilot--project-safe-p ()
    (when-let ((root (projectile-project-root)))
      (and (file-exists-p copilot-safe-projects-file)
           (with-temp-buffer
             (insert-file-contents copilot-safe-projects-file)
             (member root (split-string (buffer-string) "\n" t))))))

  ;; Wrap copilot-mode to check safety
  (define-advice copilot-mode (:around (fn &optional arg) check-safe-project)
    "Only enable copilot in safe projects. Prompt if interactive, silently block if not."
    (if (and (> (prefix-numeric-value arg) 0)  ; Enabling, not disabling
             (projectile-project-root)          ; In a project
             (not (copilot--project-safe-p)))   ; Not safe yet
        ;; Unsafe project - prompt if interactive, block if from hook
        (when (and (called-interactively-p 'interactive)
                   (y-or-n-p (format "Enable copilot in %s and mark as safe? "
                                     (projectile-project-root))))
          ;; Add to safe projects
          (write-region (concat (projectile-project-root) "\n") nil
                        copilot-safe-projects-file 'append)
          (funcall fn arg))
      ;; Safe project, no project, or disabling - proceed normally
      (funcall fn arg))))

;; https://discourse.doomemacs.org/t/permanently-display-workspaces-in-the-tab-bar/4088
(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))

;; Make magit-status the default action when selecting projects with projectile
(after! projectile
  (setq projectile-switch-project-action #'magit-status))

;; Save all project files when opening magit-status
(after! magit
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (projectile-save-project-buffers))))

(after! lsp-mode
  ;; https://docs.astral.sh/ty/reference/editor-settings/#diagnosticmode
  (lsp-register-custom-settings '(("ty.diagnosticMode" "off"))))

(use-package! goto-addr
  :demand t)
(after! goto-addr
  (global-goto-address-mode))

(use-package! xt-mouse
  :demand t)
(after! xt-mouse
  (xterm-mouse-mode))

(use-package! which-key
  :demand t)
(after! which-key
  (which-key-mode +1))

(after! flymake
  (setq flymake-show-diagnostics-at-end-of-line t))
