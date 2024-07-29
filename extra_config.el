(setq shell-file-name (executable-find "bash")) ;; emacs
(setq-default vterm-shell (executable-find "fish")) ;; vterm

(setq line-spacing 2)

(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))

;; keybindings
(map!
 "C-c o" #'lsp-ui-doc-glance
 "C-c i" #'lsp-ui-imenu
 "C-c t" #'lsp-ui-imenu--refresh
 "s-[" #'winner-undo
 "s-]" #'winner-redo
 "M-s-<left>" #'winner-undo
 "M-s-<right>" #'winner-redo)

(defmacro normal-map! (&rest args)
  "ALIST of form (keybinding . func)"
  (let (value)
    (append (list 'map!) (dolist (arg args value)
                           (setq value (append `(:n ,(car arg) ,(cdr arg)) value))))))

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

(defun popper-group-by-workspace ()
  (+workspace-current-name))

(setq popper-group-function #'popper-group-by-workspace)

(setq +org-capture-baby-file "baby.org"
      +org-chores-file "~/org/chores.org")

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

(defun org-agenda-popup ()
  (interactive)
  (let ((buf (get-buffer-create "*Org Agenda*"))
        (org-agenda-start-day nil)
        (org-agenda-span 1))
    (display-buffer buf '(display-buffer-pop-up-window . ((dedicated . t) (window-width . 80))))
    (org-agenda nil "c")))

(map! :n "SPC w a" #'org-agenda-popup)

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

(setq org-roam-directory "~/org-roam")

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

(setq lsp-go-use-gofumpt t)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(add-hook! 'go-mode-hook
  (which-function-mode 1)
  (add-hook! 'before-save-hook #'lsp-format-buffer t t)
  (add-hook! 'before-save-hook #'lsp-organize-imports t t))
(add-hook! 'go-ts-mode-hook
  (lsp))

(add-hook! 'tsx-ts-mode
  (lsp)
  (setq-local tab-width 4))

(add-hook! 'tsx-ts-mode
  (add-hook! 'before-save-hook #'+format/buffer))

(add-hook! 'tsx-ts-mode 'prettier-js-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "es5"
                         "--bracket-spacing" "true"
                         "--single-quote" "true"
                         ))

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

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(setq doom-themes-treemacs-theme "doom-colors")
