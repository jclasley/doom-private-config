
# Table of Contents

1.  [General](#org59e508c)
    1.  [Doom doctor recommendations](#org1abaadc)
    2.  [More line spacing](#org04fcaad)
    3.  [Always show perspectives in minibuffer](#org5f635e8)
2.  [Convenience](#orgfafbc76)
    1.  [Global keybindings, regardless of mode](#org13d2e91)
    2.  [Normal mode keybindings](#orga728790)
    3.  [Popper](#orgd6fb907)
3.  [Org](#org9de0705)
    1.  [Variables](#org01efa28)
    2.  [Open org agenda in a popup buffer window](#orgd90bb6e)
    3.  [org-super-agenda-mode](#org5ad91cf)
    4.  [org roam](#org4f5a37d)
        1.  [Directory](#orga096485)
        2.  [Capture templates](#orgd0291a6)
    5.  [Capture templates](#orgec862df)
4.  [Language-specific](#orge526949)
    1.  [Go](#orgd03cbe2)
        1.  [hooks](#orgce4bc55)
    2.  [TSX](#org66b3a67)
        1.  [Prettier](#org1a9f205)
5.  [K8s helm](#org1b66e00)
6.  [Treesitter](#orgb08a89b)
7.  [Hacks](#org736117b)
    1.  [Remove file watches](#org1d85703)
8.  [Treemacs theme](#orgcd0d1cd)



<a id="org59e508c"></a>

# General


<a id="org1abaadc"></a>

## Doom doctor recommendations

    (setq shell-file-name (executable-find "bash")) ;; emacs
    (setq-default vterm-shell (executable-find "fish")) ;; vterm


<a id="org04fcaad"></a>

## More line spacing

    (setq line-spacing 2)


<a id="org5f635e8"></a>

## Always show perspectives in minibuffer

    (after! persp-mode
      (defun display-workspaces-in-minibuffer ()
        (with-current-buffer " *Minibuf-0*"
          (erase-buffer)
          (insert (+workspace--tabline))))
      (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
      (+workspace/display))


<a id="orgfafbc76"></a>

# Convenience


<a id="org13d2e91"></a>

## Global keybindings, regardless of mode

    ;; keybindings
    (map!
     "C-c o" #'lsp-ui-doc-glance
     "C-c i" #'lsp-ui-imenu
     "C-c t" #'lsp-ui-imenu--refresh
     "s-[" #'winner-undo
     "s-]" #'winner-redo
     "M-s-<left>" #'winner-undo
     "M-s-<right>" #'winner-redo)


<a id="orga728790"></a>

## Normal mode keybindings

Convenience macro to allow for multiply normal mode maps in a row

    (defmacro normal-map! (&rest args)
      "ALIST of form (keybinding . func)"
      (let (value)
        (append (list 'map!) (dolist (arg args value)
                               (setq value (append `(:n ,(car arg) ,(cdr arg)) value))))))

Function to highlight everything on this line and jump to the matching end brace

    ;; visual mode select block
    (defun visual-select-block ()
      (interactive)
      (evil-visual-line)
      (evil-end-of-line)
      (evil-jump-item))

    ;; normal mode maps
    (normal-map!
     ("SPC v" . #'visual-select-block)
     ("SPC p ~" . #'project-dired)
     ("SPC o c" . #'cfw:open-org-calendar)
     ("] e" . #'flycheck-next-error)
     ("[ e" . #'flycheck-previous-error)
     ("SPC c p" . #'flycheck-projectile-list-errors)
     ("SPC b Y" . #'centaur-tabs--copy-file-name-to-clipboard)
     ("SPC f n" . #'treemacs-create-file))


<a id="orgd6fb907"></a>

## Popper

[reference](https://github.com/karthink/popper)

    (use-package! popper
      :bind (("C-`" . popper-toggle)
             ("s-." . popper-cycle)
             ("C-<escape>" . popper-kill-latest-popup))
      :init ;; set certain buffers to be automatically popups
      (setq popper-reference-buffers
            '("\\*Ibuffer\\*"
              "\\*Messages\\*"
              "\\*info\\*"
              "\\*helpful.*"
              vterm-mode))
      :config
      (popper-mode +1))

Custom function to group popups based on workspace

    (defun popper-group-by-workspace ()
      (+workspace-current-name))
    
    (setq popper-group-function #'popper-group-by-workspace)


<a id="org9de0705"></a>

# Org


<a id="org01efa28"></a>

## Variables

Used later on so have to be defined now

    (setq +org-capture-baby-file "baby.org"
          +org-chores-file "~/org/chores.org")

The doom TODO statuses are suuuuper overtuned &#x2013; let&rsquo;s fix them
Also, going to make it so that they will work better with `org-super-agenda-mode`

[org-todo-keywords reference](https://orgmode.org/manual/TODO-Extensions.html)
[org-keywords special characters](https://orgmode.org/manual/Tracking-TODO-state-changes.html)

    (setq my/org-todo-keywords '(
                                 ("TODO(t)" . org-todo)
                                 ("NEXT(n)" .  (:foreground "#34ebd8" :weight bold :slant italic))
                                 ("PROG(p!)" . +org-todo-active)
                                 ("HOLD(h@)" . (:background "orange" :foreground "white")) ; in progress but held up
                                 "|"
                                 ("DONE(d!)" . org-done)
                                 ("WONT(w@/!)" . +org-todo-cancel)))
    
    
    ;; have to set it after org loads
    (after! org
      ;; keywords are the keys of the alist of ~my/org-todo-keywords~
      (setq org-todo-keywords (list (append '(sequence)
                                            (seq-map #'(lambda (elt)
                                                         (if (listp elt)
                                                             (car elt)
                                                           elt))
                                                     my/org-todo-keywords))))
    
      (setq org-todo-keyword-faces (let ((f (lambda (elt)
                                             (if (listp elt)
                                                 `(
                                                  ,(seq-take-while #'(lambda (elt) (not (equal ?\( elt))) (car elt))
                                                  .
                                                  ,(cdr elt))
                                               elt)
                                             )))
                                     (seq-map #'(lambda (elt) (funcall f elt)) my/org-todo-keywords)))
    )


<a id="orgd90bb6e"></a>

## Open org agenda in a popup buffer window

I wrote this function myself! Super proud

    (defun org-agenda-popup ()
      (interactive)
      (let ((buf (get-buffer-create "*Org Agenda*"))
            (org-agenda-start-day nil)
            (org-agenda-span 1))
        (display-buffer buf '(display-buffer-pop-up-window . ((dedicated . t) (window-width . 80))))
        (org-agenda nil "c")))
    
    (map! :n "SPC w a" #'org-agenda-popup)


<a id="org5ad91cf"></a>

## org-super-agenda-mode

`org-super-agenda-mode` gives a much prettier, organized agenda

    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
          '(("c" "Super agenda"
             ((agenda "" ((org-agenda-overriding-header "")
                          (org-super-agenda-groups
                           '((:log t)
                             (:name "Today"
                              :time-grid t
                              :date today)
                             ))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            `((:log t)
                              (:name "Important"
                               :priority "A"
                               :face (:weight ultra-bold :background "blue")
                               :order 1)
                             (:name "Next"
                                    :todo "NEXT"
                                    :order 0)
                              (:name "Daphne"
                               :tag "daphne")
                              (:name "Chores"
                               :file-path ,(expand-file-name +org-chores-file) ; back-quoted list allows evaluation with `,`
                               :face (:slant italic)
                               :order 2)
                              (:name "Can wait"
                               :priority "C")
                              (:name "If time"
                               :priority "B")))))))))


<a id="org4f5a37d"></a>

## org roam


<a id="orga096485"></a>

### Directory

    (setq org-roam-directory "~/org-roam")


<a id="orgd0291a6"></a>

### Capture templates

    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry "* %<%I:%M %p>: %?"
             :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
             :unnarrowed t
             :empty-lines-before 1)))
    
    (setq org-roam-capture-templates '(("c" "default" plain "%?"
                                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n")
                                        :unnarrowed t)
                                       ("n" "node" entry "* %?"
                                        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n")
                                        :empty-lines-before 1)))


<a id="orgec862df"></a>

## Capture templates

    (after! org
      (setq +org-capture-agenda-file "~/org/agenda.org")
      (setq org-capture-templates (append '(
                                            ("a" "Agenda")
                                            ("ad" "Deadline" entry
                                             (file+olp+datetree +org-capture-agenda-file)
                                             "* %?\nDEADLINE: %^{at}t")
                                            ("t" "Todos")
                                            ("tt" "project todo" entry
                                             (file "~/org/projects.org")
                                             "* TODO %?\n%i")
                                            ("tn" "today" entry
                                             (file+headline "~/org/todo.org" "Todos")
                                             "* TODO %?\n%t")
                                            ("td" "deadline" entry
                                             (file+headline "~/org/todo.org" "Todos")
                                             "* TODO %?\nDEADLINE: %^{at}t")
                                            ("ts" "scheduled" entry
                                             (file+headline "~/org/todo.org" "Todos")
                                             "* TODO %?\nSCHEDULED: %^{at}t")
                                            ("c" "chore" entry
                                             (file +org-chores-file)
                                             "* TODO %?\nDEADLINE: %t")
                                            ) org-capture-templates))
      )


<a id="orge526949"></a>

# Language-specific


<a id="orgd03cbe2"></a>

## Go

    (setq lsp-go-use-gofumpt t)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))


<a id="orgce4bc55"></a>

### hooks

    (add-hook! 'go-mode-hook
      (which-function-mode 1)
      (add-hook! 'before-save-hook #'lsp-format-buffer t t)
      (add-hook! 'before-save-hook #'lsp-organize-imports t t))
    (add-hook! 'go-ts-mode-hook
      (lsp))


<a id="org66b3a67"></a>

## TSX

    (add-hook! 'tsx-ts-mode
      (lsp)
      (setq-local tab-width 4))

Format on save

    (add-hook! 'tsx-ts-mode
      (add-hook! 'before-save-hook #'+format/buffer))


<a id="org1a9f205"></a>

### Prettier

    (add-hook! 'tsx-ts-mode 'prettier-js-mode)
    (setq prettier-js-args '(
                             "--trailing-comma" "es5"
                             "--bracket-spacing" "true"
                             "--single-quote" "true"
                             ))


<a id="org1b66e00"></a>

# K8s helm

Create an LSP for k8s helm so that I get nice syntax highlighting and definitions

    (define-derived-mode k8s-helm-mode yaml-mode "Helm"
      "A mode for editing helm charts.
      \\{k8s-helm-mode-map}")
    
    (after! lsp-mode
      (add-to-list 'lsp-language-id-configuration '(k8s-helm-mode . "yaml"))
    
      (lsp-register-client (make-lsp-client
                            :new-connection (lsp-stdio-connection '("helm_ls" "serve"))
                            :activation-fn (lsp-activate-on "yaml")
                            :server-id 'yaml))
      )
    
    (add-hook! 'k8s-helm-mode
      (lsp))


<a id="orgb08a89b"></a>

# Treesitter

Treesitter is helpful for syntax highlighting and supposedly has faster node navigation

    (setq treesit-font-lock-level 4)
    
    (setq treesit-language-source-alist
          '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
             "v0.20.3"
             "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                        "v0.20.3"
                        "typescript/src")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (go "https://github.com/tree-sitter/tree-sitter-go" "v0.19.1")))


<a id="org736117b"></a>

# Hacks


<a id="org1d85703"></a>

## Remove file watches

    (defun file-notify-rm-all-watches ()
      "Remove all existing file notification watches from Emacs."
      (interactive)
      (maphash
       (lambda (key _value)
         (file-notify-rm-watch key))
       file-notify-descriptors))


<a id="orgcd0d1cd"></a>

# Treemacs theme

    (setq doom-themes-treemacs-theme "doom-colors")

