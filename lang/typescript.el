;; Use web-mode instead of typescript-mode for better syntax highlighting.
(require 'web-mode)
(require 'tide)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(defun ki/set-up-tide-on-web-mode ()
  (when (member (file-name-extension (or buffer-file-name "")) '("ts"))
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
    (eldoc-mode +1)))


;; Add web-mode to typescript-tslint supported modes
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; Ensure both tide an tslint checkers are used
;; https://github.com/ananthakumaran/tide/issues/95
;; (with-eval-after-load 'flycheck
;;   (with-eval-after-load 'tide
;;     (flycheck-add-next-checker 'typescript-tslint 'web-mode)))

;;(add-hook 'before-save-hook 'tide-format-before-save)

;; Keybindings for tide
(defun tide-set-keys ()
  (local-set-key (kbd "C-c C-t r") 'tide-rename-symbol)
  (local-set-key (kbd "C-c C-t s") 'tide-start-server))

(add-hook 'web-mode-hook 'ki/set-up-tide-on-web-mode)
(add-hook 'typescript-mode-hook 'ki/tide-mode-setup)
