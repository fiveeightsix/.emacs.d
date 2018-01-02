
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq user-mail-address "mail@karlinglis.net")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)   ;; Delete/type over selection
(setq-default indent-tabs-mode nil)
(setq tramp-default-method "ssh")
(setq set-mark-command-repeat-pop t)   ;; Slightly faster pop-to-mark command

(setq backup-directory-alist '(("" . "~/.emacs.d/backup-files")))

;; Auto-revert for buffers
;; Reference: http://pragmaticemacs.com/emacs/automatically-revert-buffers/
;; (global-auto-revert-mode 1)
;; (setq global-auto-revert-non-file-buffers t)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'bar)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

(load "~/.emacs.secrets")

;; Use ibuffer:
(defalias 'list-buffers 'ibuffer)

;; Use Shift+arrow_keys to move cursor around split panes:
(windmove-default-keybindings)

;; When cursor is on edge, move to the other side, as in a toroidal space:
(setq windmove-wrap-around t)

;; Buffer move
;; Reference: http://www.emacswiki.org/cgi-bin/wiki/buffer-move.el
(require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; When using dired in split windows, make the default copy target
;; the directory shown in the second window.
(setq dired-dwim-target t)

;; ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; (require 'ido)
;; (ido-mode 1)
;; (setq ido-enable-flex-matching t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired))

(require 'auto-complete-config)
(ac-config-default)

(setq ac-ignore-case nil)

(setq ac-use-menu-map t)
(define-key ac-completing-map (kbd "<C-n>") 'ac-next)
(define-key ac-completing-map (kbd "<C-p>") 'ac-previous)

(require 'flycheck)

(defun ki/enable-flycheck-for-mode ()
  "Add to mode hooks to set flycheck-mode t."
  (flycheck-mode t))

;; Use rainbow mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; Enable flycheck
(add-hook 'css-mode-hook 'ki/enable-flycheck-for-mode)

;; SCSS
(add-hook 'scss-mode-hook 'ki/enable-flycheck-for-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)

;; Stop flycheck using jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; Use js2-mode on .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun ki/js2-mode-setup ()
  "Set options for js2-mode."
  (setq-default js2-global-externs '("module" "require"))
  (setq-default js2-basic-offset 2)
  (setq-default js2-mode-show-parse-errors nil)  ; Leave to flycheck
  ;; Use flycheck and eslint
  (flycheck-mode t)
  (when (executable-find "eslint")
    (flycheck-select-checker 'javascript-eslint))
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'js2-mode-hook 'ki/js2-mode-setup)

;; Use web-mode on .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; Web mode

(defun ki/web-mode-setup ()
  "Set options for web-mode."
  (electric-pair-mode t)
  (subword-mode t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(add-hook 'web-mode-hook 'ki/web-mode-setup)

;; eslint can check .jsx syntax - use this in web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)

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

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . web-mode))

(setq geiser-default-implementation 'guile)
(setq geiser-active-implementations '(guile))
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(setq python-shell-interpreter "python3")
;; Use jedi for autocomplete sources
(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)

(add-to-list 'auto-mode-alist '("\\.shader\\'" . glsl-mode))

(add-to-list 'load-path "/usr/share/maxima/5.32.1/emacs")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Front-end for maxima with image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
;; (autoload 'imath-mode "imath-mode" "Imath mode for maths formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

(require 'org)

(setq org-directory "~/org")


;; Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "<f11>") 'org-clock-goto)
; (global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f12>") 'org-agenda)

(defun ki/org-local-keys ()
  "Local keybindings for use in Org mode."
  (local-set-key (kbd "C-c d") 'org-decrypt-entry))

(add-hook 'org-mode-hook 'ki/org-local-keys)

(defun ki/org-agenda-local-keys ()
  "Local keybindings for use in Org Agenda mode."
  (local-set-key (kbd "C-c s") 'ki/org-git-checkpoint))

(add-hook 'org-agenda-mode-hook 'ki/org-agenda-local-keys)


;; Headline ID links
(add-to-list 'org-modules 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)


;; Org crypt
(require 'org-crypt)
(add-to-list 'org-modules 'org-crypt)

; Encrypt entries before saving.
(org-crypt-use-before-save-magic)

; Set tag for encrypted headings.
(setq org-crypt-tag-matcher "CRYPT")
(setq org-tags-exclude-from-inheritance (quote ("CRYPT")))


; Prevent org-crypt from disabling auto-save.
(setq org-crypt-disable-auto-save nil)


;; Wrapping and lines
(setq org-cycle-separator-lines 0)
(setq org-startup-truncated nil)

(defun ki/org-wrapping ()
  "Set text wrapping for Org mode."
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'ki/org-wrapping)

; Override these settings for the agenda
; Reference: http://superuser.com/questions/530363/emacs-org-mode-how-to-disable-visual-line-wrap-for-agenda-buffers-only
(defun ki/org-agenda-wrapping ()
  "Set text wrapping for Org mode agenda."
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(add-hook 'org-agenda-mode-hook 'ki/org-agenda-wrapping)


;; Save clock history accross emacs sessions.
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;; Logging
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)


;; Show more tasks in clock history
(setq org-clock-history-length 24)


;; TODO keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))))

;; (setq org-todo-keyword-faces
;;       (quote (("WAITING" :foreground "orange" :weight "bold")
;;            ("CANCELLED" :foreground "forest green" :weight "bold"))))


;; Default column view
(setq org-columns-default-format "%25ITEM(Task) %TODO %TAGS")


;; Images
(setq org-startup-with-inline-images nil)
(setq org-image-actual-width 600)

;; Agenda settings
(setq org-agenda-files
      (quote ("~/org"
              "~/org/projects")))

(setq org-agenda-span 'day)

; Habits
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

; Make agenda buffer use a full window
(setq org-agenda-window-setup 'current-window)

; Allow tag searches to ignore scheduled and deadlined tasks
(setq org-agenda-tags-todo-honor-ignore-options t)

; Custom adgenda commands
(setq org-agenda-custom-commands
      (quote (("n" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")))
              ("o" "Someday" todo "SOMEDAY"
               ((org-agenda-overriding-header "Someday...")))
              ("r" "Tasks to refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to refile")))
              ("R" "Tasks eligible for archiving" tags "CLOSED<=\"<-90d>\"-NOARCHIVE"
               ((org-agenda-overriding-header "Tasks eligible for archiving (closed over 90 days ago)")))
              ("j" . "Jujitsu syllabus") ; description for "j" prefix. 
              ("jt" "Current techniques" tags-todo "+jujitsu+SYLLABUS/!+NEW"
               ((org-agenda-overriding-header "Jujitsu syllabus - current techniques")))
              ("js" "Future techniques" tags "+jujitsu+SYLLABUS"
               ((org-agenda-overriding-header "Jujitsu syllabus - future techniques")))
              ("p" "Passwords" tags "PASSWD"
               ((org-agenda-overriding-header "Passwords")))
              (" " "Agenda" 
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks/notes to refile")))
                (tags-todo "-SYLLABUS/!-WAITING-SOMEDAY"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-todo-ignore-scheduled 'all)
                            (org-agenda-todo-ignore-deadlines 'near)))
                (tags-todo "-SYLLABUS/!+WAITING-SOMEDAY"
                           ((org-agenda-overriding-header "Waiting and postponed tasks")
                            (org-agenda-todo-ignore-scheduled 'future)))
               nil)))))


;; Capture settings
(setq org-default-notes-file (concat org-directory "/capture.org"))

(setq org-capture-templates
      (quote (("n" "Note" entry (file "~/org/capture.org")
               "* %? :NOTE:\n %U\n %a")
              ("t" "Task" entry (file "~/org/capture.org")
               "* TODO %?\n %U\n %a\n")
              ("l" "Note with web link" entry (file "~/org/capture.org")
               "* %? :NOTE:\n %U\n %x")
              ("m" "Meeting" entry (file "~/org/capture.org")
               "* %? :MEETING:\n %U\n" :clock-in t :clock-out t)
              ("j" "Journal" entry (file+datetree "~/org/journal.org")
               "* %U\n %?")
              ("h" "Habit" entry (file "~/org/capture.org")
               "* TODO %?\n%U\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:END:")
              ("p" "Password" entry (file "~/org/capture.org")
               "* %? :PASSWD:CRYPT:\n %U\n\n user: \n pass: ")
              ("w" "Weight reading" table-line (file+headline "~/org/personal.org" "Weight")
               "| %u |   %? |")
              ("6" "6Music now playing" entry (file "~/org/capture.org")
               "* %(ki/bbc-radio-nowplaying \"http://polling.bbc.co.uk/radio/nowandnextservice/bbc_6music.jsonp\") :NOTE:music:\n %U\n %?"))))


;; Refile settings
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil . (:maxlevel . 9))
                                 (org-agenda-files . (:maxlevel . 9)))))

; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))


;; Archive settings
(setq org-archive-location "%s_archive::* Archived Tasks")
; Don't loose TODO state
(setq org-archive-mark-done nil)


;; Export settings
(setq org-export-allow-bind-keywords t)
(setq org-html-validation-link nil)
(setq org-html-postamble t)
(setq org-html-postamble-format 
      (quote (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Created: %T</p>
<p class=\"creator\">%c</p>"))))
; Images not links
(setq org-html-inline-images t)
; Basic styles to improve readability.
(setq org-html-head "<link rel=\"stylesheet\" href=\"css/org-style.css\" type=\"text/css\" />
<meta name=\"viewport\" content=\"width=device-width\" />")
;; (setq org-html-head-extra "<meta name=\"viewport\" content=\"width=device-width\" />")

(setq org-publish-project-alist
      (quote (("org-org"
               :base-directory "~/org"
               :publishing-directory "~/Dropbox/org-publish"
               :publishing-function org-html-publish-to-html)
              ("org-static"
               :base-directory "~/org"
               :recursive t
               :base-extension "js\\|css\\|png\\|jpg\\|pdf"
               :publishing-directory "~/Dropbox/org-publish"
               :publishing-function org-publish-attachment)
              ("org"
               :components ("org-org"
                            "org-static"))
              ("web.karlinglis.net-org"
               :base-directory "~/Projects/web.karlinglis.net/org"
               :base-extension "org"
               :recursive t
               :publishing-directory "~/Projects/village/www/web.karlinglis.net"
               :publishing-function org-html-publish-to-html
               :html-head-include-default-style nil
               :html-head-include-scripts nil)
              ("web.karlinglis.net-static"
               :base-directory "~/Projects/web.karlinglis.net/org"
               :base-extension "css\\|js\\|png\\|jpg\\|pdf"
               :recursive t
               :publishing-directory "~/Projects/village/www/web.karlinglis.net"
               :publishing-function org-publish-attachment)
              ("web.karlinglis.net"
               :components ("web.karlinglis.net-org"
                            "web.karlinglis.net-static"))
              ("start.karlinglis.net-org"
               :base-directory "~/Projects/start.karlinglis.net/org"
               :base-extension "org"
               :publishing-directory "~/Projects/start.karlinglis.net/html"
               :publishing-function org-html-publish-to-html)
              ("start.karlinglis.net-static"
               :base-directory "~/Projects/start.karlinglis.net/static"
               :base-extension "css\\|js"
               :publishing-directory "~/Projects/start.karlinglis.net/html"
               :publishing-function org-publish-attachment)
              ("start.karlinglis.net"
               :components ("start.karlinglis.net-org"
                            "start.karlinglis.net-static")))))


(defun ki/org-html-format-drawer-function (name contents)
  "Override drawer formatting for HTML export."
  (concat "<div class=\"drawer drawer-" (downcase name) "\">\n"
          contents
          "\n</div>"))

(setq org-html-format-drawer-function 'ki/org-html-format-drawer-function)


;; Org babel
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (sh . t)
         (ditaa . t)
         (dot . t) ; Graphviz
         (python . t)
         (js . t)
         (scheme . t)
         (css . t)
         (gnuplot . t)
         (maxima . t)
         (sqlite . t))))

(setq org-babel-python-command "python3")

;; Link types
;; (defun ki/org-custom-link-pic-follow (path)
;;   (org-open-file-with-emacs
;;    (concat picture-directory "/" path)))

;; (org-add-link-type "pic" 'ki/org-custom-link-pic-follow)

;; (setq org-link-abbrev-alist
;;       (quote (("pic" . "~/Pictures/"))))

;; LaTeX fragments
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.8))
(setq org-latex-packages-alist
      (quote (("" "esdiff" t)
              ("" "mathpartir" t)
              ("" "krimaths" t) ; Custom definitions
              )))

;; Ditaa
(setq org-ditaa-jar-path "/usr/bin/ditaa")

;; LaTeX maths in ODT export
(require 'ox-odt)
(setq org-latex-to-mathml-convert-command
      "latexmlmath \"%i\" --presentationmathml=%o")

;; Additional pretty entities
(add-to-list 'org-entities-user
             '("supseteq" "\\supseteq" t "&supe;"
               "[superset of or equal to]"
               "[superset of or equal to]" "⊇"))
(add-to-list 'org-entities-user
             '("subseteq" "\\subseteq" t "&sube;"
               "[subset of or equal to]"
               "[subset of or equal to]" "⊆"))
(add-to-list 'org-entities-user
             '("vdash" "\\vdash" t "&#8866;"
               "[right tack]"
               "[right tack]" "⊢"))


;; Compatibility with windmove in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Compatibility with ispell
(defun ki/org-ispell ()
  "Configure 'ispell-skip-region-alist' for org-mode."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^[[:space:]]*#\\+BEGIN_SRC" . "^[[:space:]]*#\\+END_SRC")))

(add-hook 'org-mode-hook 'ki/org-ispell)

;; Org git checkpoint
(defun ki/org-git-checkpoint ()
  "Save all Org mode buffers and run org-cp script."
  (interactive)
  (org-save-all-org-buffers)
  (shell-command "org-cp"))

(setq jiralib-url "https://shapecast.atlassian.net")

(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

(eval-after-load 'tex-mode
  '(add-to-list 'tex-compile-commands
                '((concat "xelatex "
                          (if (< 0 (length tex-start-commands))
                              (shell-quote-argument tex-start-commands))
                          " %f")
                  t "%r.pdf")))

(defun ki/ispell-region-or-buffer (r-beg r-end)
  "Call ispell-region or ispell-buffer depending on whether mark is set."
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (ispell-region r-beg r-end)
    (ispell-buffer)))

(global-set-key (kbd "<f7>") 'ki/ispell-region-or-buffer)

(require 's)

(define-skeleton skel-html5-doc
  "Insert skeleton HTML5 document, querying for title."
  "Title: "
  "<!DOCTYPE html>\n"
  "<html lang=\"en-UK\">\n"
  "<head>\n"
  > "<meta charset=\"utf-8\" />\n"
  > "<title>" str "</title>\n"
  "</head>\n"
  "<body>\n"
  > _"\n"
  "</body>\n"
  "</html>")

(define-skeleton skel-jquery-iife
  "Insert immediately invoked function expression to wrap jQuery code."
  nil
  "(function ($) {\n\n"
  > _"\n\n"
  "}(jQuery));")

(define-skeleton skel-d3-closure-getter-setter
  "Insert getter-setter method prompting for object and value name."
  nil
  > (setq obj (skeleton-read "Object: ")) "." 
  (setq prop (skeleton-read "Property: ")) " = function(v) {" \n
  > "if(!arguments.length) return " prop ";" \n
  > prop " = v;" \n
  > "return " obj ";" \n
  > "};" \n\n)

(define-skeleton skel-react-component
  "Insert empty react.js component, prompting for component name."
  "Component name: "
  "var " str " = React.createClass({" \n \n
  > "render: function () {" \n \n
  > > "return (" \n
  > > > _ \n
  > > ");" \n
  > "}" \n \n
  "});")

(define-skeleton skel-react-module
  "Insert empty react.js component module, prompting for component name."
  "Component name: "
  "var React = require('react');" \n \n
  "var " str " = React.createClass({" \n \n
  > "render: function () {" \n \n
  > > "return (" \n
  > > > _ \n
  > > ");" \n
  > "}" \n \n
  "});" \n \n
  "module.exports = " str ";")

(define-skeleton skel-org-food-recipie
  "Insert necessary headings for recipie entry, querying for title."
  "Title: " 
  "** " str "\n"
  > ":PROPERTIES:\n"
  > ":source:    \n"
  > ":serves:    \n"
  > ":status:    \n"
  > ":END:\n\n"
  "*** Ingredients\n\n"
  > _"\n\n"
  "*** Equipment\n\n"
  "*** Method\n\n")

(define-skeleton skel-org-feed
  "Insert feedpage entry, querying for title and URL."
  nil
  "*** " (setq title (skeleton-read "Feed title: ")) "\n"
  ":FEEDURL:\n"
  (setq url (skeleton-read "Feed URL: ")) "\n"
  ":END:\n\n")

(define-skeleton skel-org-block-ditaa
  "Insert an org ditaa block, querying for filename."
  "File (no extension): "
  "#+BEGIN_SRC ditaa :file " str ".png :cmdline -E -S\n"
  > _ - \n
  "#+END_SRC\n")

(define-skeleton skel-org-block-gnuplot
  "Insert an org gnuplot block, querying for filename."
  "File (no extension): "
  "#+BEGIN_SRC gnuplot :file " str ".png\n"
  > "reset" \n
  > "set terminal png size 600,400" \n
  > _ - \n
  "#+END_SRC\n")

(define-skeleton skel-org-block-graphviz-dot
  "Insert an org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+BEGIN_SRC dot :file " str ".png" \n
  > "digraph G {" \n
  > > _ - \n
  > "}" \n
  "#+END_SRC" \n)

(define-skeleton skel-org-block-maxima-latex
  "Insert an org maxima block set up for inline latex display."
  nil
  "#+BEGIN_SRC maxima :exports none :results raw\n"
  > "tex(" _ ")" \n
  "#+END_SRC\n")

(define-skeleton skel-ng-action-class
  "Insert ngrx action class, promting for name and type."
  nil
  '(setq name (skeleton-read "Name (space-separated words): "))
  "export class " (s-upper-camel-case name) " implements Action {\n"
  > "readonly type = " (upcase (s-snake-case name)) ";\n"
  > "constructor(public payload: any) { }\n"
  "}\n")

(define-skeleton skel-wp-plugin
  "Insert basic plugin file skeleton, querying for name and description."
  nil
  "<?php\n"
  "/**\n"
  " * Plugin Name: " (setq name (skeleton-read "Plugin name: ")) "\n"
  " * Description: " (setq description (skeleton-read "Plugin description: ")) "\n"
  " * Author: Karl Inglis\n"
  " * Author URI: http://web.karlinglis.net\n"
  " * Version: 1.0.0\n"
  "*/\n\n"
  _"\n\n"
  "?>")

(define-skeleton skel-latex-split-equation-block
  "Inset LaTeX equation* split block while in org mode."
  nil
  "\\begin{equation*}" \n
  "\\begin{split}" \n
  > _ \n
  "\\end{split}" \n
  "\\end{equation*}" \n)

(define-skeleton skel-latex-tma
  "Template for LaTeX TMAs, prompting for date and title."
  nil
  "\\documentclass{article}" \n
  "\\usepackage{amsmath}" \n
  "\\usepackage{amssymb}" \n
  "\\usepackage{siunitx}" \n
  "\\usepackage{array} % For advanced column specification in tabular" \n
  "\\usepackage{blkarray} % For labelled matrices" \n
  "\\usepackage{commath} % For \\abs" \n
  "\\usepackage{graphicx}" \n
  "\\usepackage{fancyhdr}" \n
  "\\usepackage{pdftricks2}" \n
  "\\usepackage{esdiff}" \n
  "\\usepackage{polynom}" \n \n
  "\\usepackage{krimaths}" \n \n
  "\\newcommand\\addtag{\\refstepcounter{equation}\\tag{\\theequation}}" \n \n
  "\\pagestyle{fancy}" \n
  "\\lfoot{Karl Inglis - D1289717}" \n
  "\\cfoot{}" \n
  "\\rfoot{\\thepage}" \n \n
  "\\renewcommand{\\headrulewidth}{0pt}" \n
  "\\renewcommand{\\footrulewidth}{0.4pt}" \n \n
  "\\author{Karl Inglis - D1289717}" \n
  "\\date{" (setq date (skeleton-read "Date (YYYY-MM-DD): ")) "}" \n \n
  "\\title{" (setq title (skeleton-read "Title (M TMA N): ")) "}" \n \n
  "\\begin{document}" \n \n
  "\\maketitle" \n \n
  "\\section*{1.}" \n \n
  _ \n \n
  "\\end{document}")

(defun dot-emacs ()
  "Opens .emacs (init.el) file for customisation."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Lookup fuctions 
;; Reference: http://xahlee.org/emacs/emacs_lookup_ref.html

(defun web-lookup-function (base-url)
  "Look up the fuction name under cursor in an online documentation source,
given by base-url. If there is a text selection (a phrase), use that.

This command is intended to be called by specialised wrapper functions,
and switches to the browser."
  (interactive)
  (let (function-name url)
    (setq function-name
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq function-name (replace-regexp-in-string " " "_" function-name))
    (setq url (concat base-url function-name))
    (browse-url url)))


(defun web-lookup-function-php ()
  "Look up the word under the cursor in the online PHP manual at uk.php.net.
This command switches to the browser"
  (interactive)
  (web-lookup-function "http://uk.php.net/"))


(defun web-lookup-function-wordpress ()
  "Look up the word under the cursor in the wordpress documentation at codex.wordpress.org.
This command switches to the browser"
  (interactive)
  (web-lookup-function "http://codex.wordpress.org/Function_Reference/"))


;; Bind to useful keys:
(global-set-key (kbd "C-h C-p") 'web-lookup-function-php)
(global-set-key (kbd "C-h C-q") 'web-lookup-function-jquery)
(global-set-key (kbd "C-h C-w") 'web-lookup-function-wordpress)


;; Getting the atrist/title of the current track on the radio

(require 'json)


(defun ki/json-url-to-alist (url)
  "Return alist representation of JSON data at url."
  (with-current-buffer (url-retrieve-synchronously url)
   ;; match the opening {, to ensure we start on a JSON object
   (goto-char (point-min))
   (re-search-forward "{")
   (goto-char (match-beginning 0))
   (let ((json-object-type 'alist))
     (json-read))))


(defun ki/bbc-radio-nowplaying (url)
  "Get title/artist of current track from BBC nowandnext JSON request url.
Returns string in the form \"<artist> - <title>\"."
  (let* ((nn-alist (ki/json-url-to-alist url))
         (message-alist (assq 'message nn-alist)))
    (concat (assoc-default 'artist message-alist) " - " (assoc-default 'title message-alist))))


;; Converting px to rem in Wordpress CSS

(defun ki/px-to-rem (px-value rembase)
  "Convert CSS dimension from px to rem.
Returns result as string shortened to 11 characters and suffixed with 'rem' (bit hackish, but of sufficient precision)."
  (if (string-match "\\([0-9]+\\)px" px-value)
      (let (result-string
            (max-length 11))
        (setq result-string
              (number-to-string
               (/ (string-to-number (match-string 1 px-value)) 
                  (float rembase))))
        (concat
         (if (> (length result-string) max-length)
             (substring result-string 0 max-length)
           result-string)
         "rem"))
        nil))


(defun ki/px-to-rem-at-point-killring ()
  "Convert CSS px value at point to rem (base 14px), copying result to kill ring.
If there is a text selection (a phrase), use that."
  (interactive)
  (let (px-value)
    (setq px-value
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (kill-new (ki/px-to-rem px-value 14))))

(defvar title-case-exclude '("at" "or" "but" "by" "for" "from" "in" "into" 
                                   "like" "near" "of" "off" "on" "onto" "out" 
                                   "over" "to" "up" "upon" "with" "nor" "so" 
                                   "yet" "the" "if" "and")
  "List of words not to capitalize when in titles.")


(defun in-list-p (object list)
  "Returns t if supplied object is equal to one or more values in the given list."
  (if (equal object (car list))
      t
    (if (not (equal nil (cdr list)))
        (in-list-p object (cdr list))
      nil)))


(defun title-case-region (r-beg r-end)
  "Capitalize important words in the selected region, like a title."
  (interactive "r")
  (let (word 
        (count 0)) ; keep track of number of words
    (save-excursion
      (save-restriction
        (narrow-to-region r-beg r-end)
        ;; Make everything lowercase, or matching won't work:
        (downcase-region r-beg r-end)
        (goto-char (point-min))
        ;; Isolate words, work on one at a time:
        (while (re-search-forward "\\w\\{2,\\}" nil t)
          (setq word (match-string 0)) 
          (delete-region (match-beginning 0) (match-end 0))
          ;; Capitalize word only if it's the first, or if it's not in the list:
          (if (or (zerop count)
                  (not (in-list-p word title-case-exclude)))
              (insert (capitalize word))
            (insert word))
          (setq count (1+ count)))))))


(defun title-case-string (t-str)
  "Capitalize important words in string, like a title."
  (with-temp-buffer
    (goto-char (point-min))
    (insert t-str)
    (title-case-region (point-min) (point-max))
    (buffer-string)))


(defun format-as-identifier (ws-str &optional separator rep-regexp)
  "Replace whitespace and punctuation in the given string with a separator."
  (interactive)
  ;; set default values if no optinal arguments given
  (if (not rep-regexp)
      (setq rep-regexp "[^A-Za-z0-9-]+"))
  (if (not separator) 
      (setq separator "-"))
  ;; trim ends
  (setq ws-str (replace-regexp-in-string "^[^A-Za-z0-9]+" "" ws-str))
  (setq ws-str (replace-regexp-in-string "[^A-Za-z0-9]+$" "" ws-str))
  ;; replace unwanted characters
  (setq ws-str (replace-regexp-in-string rep-regexp separator ws-str))
  ;; return string with no caps
  (downcase ws-str))

(defun count-words (beginning end)
  "Returns the number of words in the region.
Use count-words-region to call interactively."
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      count)))
    

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (let (count)
      ;; get the word count:
      (setq count (count-words beginning end))
      ;; print in message:
      (cond ((zerop count)
             (message "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word."))
            (t
             (message "The region has %d words." count))))))


(defun count-words-xml (beginning end)
  "Returns the number of words in region, excluding XML tags.
Use count-words-region-xml to call interactively."
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf beginning end)
      (goto-char (point-min))
      ;; remove all tags:
      (while (re-search-forward "</?[^\0]*?>" nil t)
        (replace-match "" nil nil))
      ;; count what remains:
      (count-words (point-min) (point-max)))))


(defun count-words-region-xml (beginning end)
  "Print number of words in region, excluding XML tags."
  (interactive "r")
  (save-excursion
    (let (count)
      ;; get the word count:
      (setq count (count-words-xml beginning end))
      ;; print in message:
      (cond ((zerop count)
             (message "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word, excluding XML tags."))
            (t
             (message "The region has %d words, excluding XML tags." count))))))
