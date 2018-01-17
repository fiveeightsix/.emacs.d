
;; Use MELPA package repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (require 'diminish)

(org-babel-load-file (expand-file-name "~/.emacs.d/dot.emacs.org"))
