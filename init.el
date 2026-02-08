;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults +all)   ; tame sudden yet inevitable temporary windows
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       (whitespace +trim)

       :emacs
       (dired +dirvish)             ; making dired pretty [functional]
       tramp
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       shell             ; simple shell REPL for Emacs

       :checkers
       (syntax +flymake +icons)              ; tasing you for every semicolon you forget

       :tools
       ansible
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp +tree-sitter)
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       llm
       (lookup +docsets)              ; navigate your code and its documentation
       lsp               ; M-x vscode
       (magit +forge)             ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       (terraform +lsp)         ; infrastructure as code
       tree-sitter       ; syntax and parsing, sitting in a tree...

       :os
       (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       (tty +osc)               ; improve the terminal Emacs experience

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (go +lsp +tree-sitter)         ; the hipster dialect
       (json +lsp +tree-sitter)              ; At least it ain't XML
       (javascript +lsp +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
       (lua +lsp +tree-sitter)               ; one-based indices? one-based indices
       (markdown +grip +tree-sitter)         ; writing docs for people to ignore
       org               ; organize your plain life in plain text
       (python +lsp +uv +tree-sitter)            ; beautiful is better than ugly
       (rest +jq)              ; Emacs as a REST client
       (sh +lsp +fish)                ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp +tree-sitter)               ; the tubes
       (yaml +lsp +tree-sitter)              ; JSON, but readable


       :app
       irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       (default +bindings +smartparens))
