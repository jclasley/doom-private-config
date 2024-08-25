(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq shell-file-name (executable-find "bash")) ;; emacs
(setq-default vterm-shell (executable-find "fish")) ;; vterm

(setq line-spacing 1)

;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

;; keybindings
(map!
 :n "g h" #'lsp-ui-doc-glance
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
          "\\*Embark Export:.*"
          vterm-mode))
  :config
  (popper-mode +1))

(defun popper-group-by-workspace ()
  (+workspace-current-name))

(setq popper-group-function #'popper-group-by-workspace)

(map!
 :after origami
 :leader
 :mode origami-mode
 :prefix ("z" . "origami")
 :n "o" #'origami-open-node
 :n "O" #'origami-open-all-nodes
 :n "c" #'origami-close-node
 :n "C" #'origami-close-all-nodes
 :n "z" #'origami-forward-toggle-node
 :n "n" #'origami-show-only-node
 :n "u" #'origami-undo)

(setq +org-capture-baby-file "baby.org"
      +org-chores-file "~/org/chores.org")

(setq my/org-todo-keywords '(
                             ("TODO(t)" . org-todo)
                             ("NEXT(n)" .  (:foreground "#34ebd8" :weight bold :slant italic))
                             ("PROG(p!)" . +org-todo-active)
                             ("HOLD(h@)" . (:background "orange" :foreground "white")) ; in progress but held up
                             ("|")
                             ("DONE(d!)" . org-done)
                             ("WONT(w@/!)" . +org-todo-cancel)))

(defmacro org-keywords! (kws)
  (let ((keywords-only (seq-map #'car kws))
        (keywords-drop-special (seq-map #'(lambda (elt)
                                            (let* ((kw (car elt))
                                                   (i (seq-position kw ?\()))
                                              `(,(substring kw 0 i) . ,(cdr elt)))) kws))))
  `(setq org-todo-keywords `(sequence ,keywords-only)
         org-todo-keyword-faces ,keywords-drop-special))

;; (macroexpand '(org-keywords! my/org-todo-keywords))

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

(setq org-tag-alist (append '((:startgroup . nil) ; at most one of the following
                              ("@home" . ?h)
                              ("@work" . ?w)
                              ("@out". ?o)
                              (:endgroup . nil)
                              ;; any of the following
                              ("project" . ?p)
                              ("learning" . ?l))
                            org-tag-alist-for-agenda))

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
                         (:name "Overdue"
                          :deadline past)
                         (:name "Habits"
                          :habit t)
                         (:name "Today"
                          :time-grid t
                          :date today)
                         (:name "Important"
                          :priority "A"
                          :face (:weight ultra-bold :background "blue")
                          :order 1)
                         (:name "Unstarted"
                          :scheduled past)
                         ))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        `((:log t)
                          (:name "Important"
                           :priority "A"
                           :face (:weight ultra-bold :background "blue")
                           :order 0)
                          (:name "Next"
                           :todo "NEXT"
                           :order 2)
                         (:name "Coming up"
                                :scheduled future)
                          (:name "In progress"
                           :todo "PROG"
                           :order 1)
                          (:name "Daphne"
                           :tag "daphne")
                          (:name "Chores"
                           :file-path ,(expand-file-name +org-chores-file) ; back-quoted list allows evaluation with `,`
                           :face (:slant italic)
                           :order 3)
                          (:name "Can wait"
                           :priority "C")
                          (:name "If time"
                           :priority "B")
                          (:discard (:file-path ,(expand-file-name "~/org/bills.org")))
                          (:discard (:file-path ,(expand-file-name "~/org/habits.org")))))))))))

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
                                        ("tf" "file todo" entry
                                         (file+headline "~/org/projects.org" "Project todos")
                                         "* TODO %? %^g\n%a")
                                        ("c" "chore" entry
                                         (file +org-chores-file)
                                         "* TODO %?\nDEADLINE: %t")
                                        ) org-capture-templates))
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (elixir . t)))

(setq lsp-go-use-gofumpt t)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

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
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/tree-sitter/tree-sitter-go-mod")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (templ "https://github.com/vrischmann/tree-sitter-templ")))

;; ;; TODO: doesn't work in Go -- have to fix this to be language agnostic

;; (defvar last-line 0) ;; local var for lines

;; (define-minor-mode sticky-mode
;;   "Use treesit to parse the syntax tree and have contextual opening block information
;; displayed in an unintrusive buffer at the top of the screen"
;;   :init-value nil ;; initial value
;;   ;; indicator
;;   :lighter " sticky"
;;   :keymap '(([C-tab] . sticky-toggle) ([C-backspace] . sticky-live-init))
;;   (if-let* ((lang (treesit-language-at 1))
;;             (treesit-ready-p lang))
;;       (make-variable-buffer-local 'sticky-buffer-name)
;;     (message "Treesit required for sticky-mode")))

;; ;; turn off the live window when sticky-mode is turned off
;; (add-hook! 'sticky-mode-hook (sticky-live-toggle sticky-mode))

;; (defvar live-toggle nil
;;   "Whether this buffer has live-toggle enabled")

;; (defun sticky-live-toggle (&optional on)
;;   "Set up all the local variables and hooks necessary to update the overlay"
;;   (interactive)
;;   (make-variable-buffer-local 'last-line)
;;   (make-variable-buffer-local 'live-toggle)
;;   ;; check if we should rerun every single time a command is issued (performance?)
;;   (if (or live-toggle (not on))
;;       (progn
;;         (setq-local live-toggle nil)
;;         (remove-hook 'post-command-hook #'sticky--should-rerun t)
;;         (sticky-close-buffer))
;;     (setq-local live-toggle t)
;;     (add-to-list 'post-command-hook #'sticky--should-rerun)))

;; ;; (defvar sticky--display)
;; (defun sticky--display (content)
;;   (sticky--window content))

;; (defvar sticky-buffer-alist nil
;;   "An ALIST of buffers and their associated sticky buffers")

;; (defun sticky-buffer-for-buffer ()
;;   (if-let ((buf (alist-get (current-buffer) sticky-buffer-alist))
;;            (_ (buffer-live-p buf)))
;;      buf
;;     ;; just in case we have a hanging ref, clean it up
;;     (setq sticky-buffer-alist (assoc-delete-all (current-buffer) sticky-buffer-alist))
;;     (let ((buf (generate-new-buffer "*sticky*")))
;;       (setq sticky-buffer-alist (cons `(,(current-buffer) . ,buf) sticky-buffer-alist))
;;     buf)))

;; (defun sticky-close-buffer (&optional kill)
;;   (if-let ((buf-list (assoc (current-buffer) sticky-buffer-alist)))
;;       ;; kill the buffer and the window
;;       (if kill
;;           (unwind-protect ; in case the user has deleted the buffer on their own
;;               (if-let ((w (get-buffer-window (cdr buf-list))))
;;                   (quit-window t w)
;;                 (kill-buffer (cdr buf-list)))
;;             (setq sticky-buffer-alist
;;                   (assoc-delete-all (car buf-list) sticky-buffer-alist)))
;;       (quit-windows-on (cdr buf-list)))))

;; (defun sticky--window (content)
;;   (let ((mode major-mode)
;;         (buf (sticky-buffer-for-buffer)))
;;     (if (equal content "")
;;         ;; kill it if it's empty
;;         (sticky-close-buffer)
;;       ;; otherwise, create the buffer
;;       (with-current-buffer buf
;;         ;; only change the major mode if necessary
;;         (unless (eq major-mode mode)
;;           ;; TODO: turn off any hooks affecting the mode
;;           (funcall mode))
;;         (hide-mode-line-mode 1)
;;         (erase-buffer)
;;         (quiet! (insert content))
;;         (goto-char 1))
;;       ;; only make the window if there isn't one yet
;;       ;; (unless (get-buffer-window buf)
;;       (display-buffer
;;        buf
;;        `(display-buffer-in-direction
;;          (direction . above) (window-height . ,(seq-count (lambda (elt) (equal elt ?\n)) content))
;;          (preserve-size . (nil . t))
;;          (set-window-parameter . ((no-other-window . t)
;;                                   (no-delete-other-window . t))))))))

;; (defun sticky--display-overlay (content)
;;   (overlay-put sticky-overlay
;;                'before-string (propertize content 'face
;;                                           '(:slant italic :weight bold
;;                                             :height 1.1 :box t))))

;;   (defun sticky--should-rerun ()
;;     "Only rerun if the new line is different from the last time we checked."
;;     (while-no-input
;;       (redisplay)
;;         (let* ((pos (point))
;;                (line (line-number-at-pos pos)))
;;           (unless (equal line last-line)
;;             (sticky--window (sticky--content pos)))
;;           (setq-local last-line line))))

;;   (defvar sticky-p nil)

;; (defun sticky--buffer-hook-func ()
;;   "Check the sticky-buffer-alist to see if any of the parent buffers
;; have been killed. At the same time, check if any non-killed buffers are no longer
;; actively being shown. If so, bury their sticky buffers."
;;   (dolist (cell sticky-buffer-alist)
;;     (if (or (not (buffer-live-p (car cell)))
;;                 (not (some-window #'(lambda (w)
;;                                   (eq (window-buffer w) (car cell))))))
;;       (with-current-buffer (cdr cell)
;;         ;; need to remove it from list first
;;         (setq sticky-buffer-alist
;;                   (assoc-delete-all (car cell) sticky-buffer-alist))
;;         (kill-buffer-and-window)))))

;; (add-hook 'buffer-list-update-hook #'sticky--buffer-hook-func)


;;   (defun sticky-toggle (pos)
;;     "Briefly show the sticky window relevant to the current position."
;;     (interactive "d")
;;     (if sticky-mode
;;       (message "Sticky mode already enabled")
;;      (let ((content (sticky--content pos)))
;;       (if (eql content "")
;;           (message "No outer blocks to display")
;;         (sticky--window content)
;;     (setq-local sticky-quit nil)
;;     (add-hook 'post-command-hook #'sticky-toggle-hide-hook)))))

;;   (defun sticky-toggle-hide-hook ()
;;     "Quit the `*sticky' window on the next command input"
;;     (while-no-input
;;       (redisplay)
;;      (when (and (not sticky-mode) (assoc (current-buffer) sticky-buffer-alist))
;;      (if (not sticky-quit)
;;         (setq-local sticky-quit t)
;;        ;; close window and kill buffer
;;         (sticky-close-buffer t)))))

;;   (defun sticky--content (pos)
;;     ;; find the offscreen lines, and concat the content
;;     (let ((lines (sticky--offscreen-lines pos)))
;;      (mapconcat #'(lambda (elt) (concat elt "\n")) (reverse lines))))

;; ;; TODO: variables to indicate blocks
;; (defgroup sticky-scope-characters
;;   nil
;;   "A list of scope characters per language, for identifying sticky blocks")

;; (defvar sticky--language-list
;;   '((go . (("{" . "}") ("(" . ")")))
;;     (tsx . (("{" . "}") ("(" . ")")))))

;; (add-to-list 'sticky--language-list '(rust . (("{" . "}") ("(" . ")"))))

;; (defun sticky--build-treesit-query ()
;;   (let* ((lang (treesit-language-at 1))
;;          (symbols (alist-get lang sticky--language-list))
;;          (opening-q '(@opening))
;;          (closing-q '(@closing))
;;          (openers (seq-reduce #'(lambda (memo elt)
;;                                   (push (car elt) memo)) symbols nil))
;;          (closers (seq-reduce #'(lambda (memo elt)
;;                                   (push (cdr elt) memo)) symbols nil)))
;;     ;; return in form of `(OPENING . CLOSING)`
;;     (list `(_ ,(vconcat openers) @opening)
;;           `(_ ,(vconcat closers) @closing))))
    

;;   (defun sticky--block-line-numbers ()
;;     "Find all the opening and closing blocks in the buffer, then create
;; an ALIST with it"
;;     (let ((opening-blocks-q '(statement_block "{" @opening))
;;            (closing-blocks-q '(statement_block "}" @closing))
;;            (query (sticky--build-treesit-query)))
;;       ;; add all the openings to a list
;;       (setq-local sticky--blocks
;;                   (treesit-query-capture (treesit-buffer-root-node) query))
;;       (sticky--section-line-alist sticky--blocks)))

;;   (defun sticky--section-line-alist (capture-alist)
;;     "Turn a list of all opening and closing captures into an alist with in the form
;; of `(start_line . end_line)'"
;;     (let (stack (ret '()))
;;       (dolist (capture capture-alist stack)
;;         (let ((line (line-number-at-pos (treesit-node-start (cdr capture)))))
;;         (if (eql (car capture) 'opening)
;;             ;; use the classic push-to-stack to find matching parens
;;             (push line stack)

;;           ;; have to pop and assign to new -- maybe better way but do that
;;           ;; don't push single line blocks or duplicates
;;           (let ((open (pop stack)))
;;             (unless (or (eql open line)
;;                          (assoc open ret))
;;               (push (cons open line) ret))))))
;;       ret))

;;   (defun sticky--unclosed-blocks-before (pos)
;;     (let ((line (line-number-at-pos pos)))
;;       (seq-filter #'(lambda (elt)
;;                       (and
;;                        ;; first check that it's offscreen
;;                        (< (car elt) (line-number-at-pos (window-start)))
;;                        ;; then check that it's before our line
;;                        (< (car elt) line)
;;                        ;; then check that its closing is after our line
;;                        (> (cdr elt) line)))
;;                   (sticky--block-line-numbers))))

;;   (defun sticky--offscreen-lines (pos)
;;     "Get the string content of the offscreen opening blocks"
;;     (when-let ((lines (sticky--unclosed-blocks-before pos)))
;;       (let (ret)
;;         (dolist (line lines ret)
;;           (save-excursion
;;             (goto-line (car line))
;;             (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ret))))))

(normal-map! ("SPC c ;" . #'sticky-toggle))

(use-package! sticky-scroll-mode)

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(setq doom-themes-treemacs-theme "doom-colors")
