(add-hook! 'haskell-mode-hook
  (let ((workspaces-without-lsp
         '("example" "example 2")))
    (when (member (+workspace-current-name) workspaces-without-lsp)
      (lsp-disconnect) ; need to wait -- not working
      (message "LSP disconnected"))))


(add-hook! 'haskell-mode-hook (message "hello!"))

(defun treemacs-git-project ()
  (interactive)
  (if-let ((root (project-root (project-current t)))
           (name (project-name (project-current t))))
      (treemacs-do-add-project-to-workspace root name)))
(add-hook 'treemacs-post-buffer-init-hook (message "hello"))

