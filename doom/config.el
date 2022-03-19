;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Junwei Chen"
      user-mail-address "kngzhi@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; Chinese Input Problem
(setq doom-font (font-spec :family "Sarasa Mono SC Nerd" :size 18))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")
;; Recursivly search all org files at [https://stackoverflow.com/a/41969519]
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(setq org-log-done t)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; enable fill-column-indicator (since emacs 27.1) in prog mode
;; see https://www.gnu.org/software/emacs/manual/html_node/emacs/Displaying-Boundaries.html
(add-hook 'prog-mode-hook (lambda ()
        (setq fill-column 80)
        (setq display-fill-column-indicator t)
        (setq display-fill-column-indicator-column t)
        (display-fill-column-indicator-mode)
        (global-display-fill-column-indicator-mode)
))


;; decrease delay for company https://emacs-china.org/t/doom-emacs-spacemacs/10956
(setq company-idle-delay 0.05
      company-show-numbers t)

;; Setting for Eww
;; (setq browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
;;         shr-use-fonts  nil                          ; No special fonts
;;         shr-use-colors nil                          ; No colours
;;         shr-indentation 2                           ; Left-side margin
;;         shr-width 80                                ; Fold text to 70 columns
;;         eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; https://rameezkhan.me/adding-keybindings-to-doom-emacs/
;; TODO: 要设置 when 对于 super(command)
(map!
        (:leader
                ";" #'execute-extended-command
                ":" #'pp-eval-expression)
        (:i "s-;" #'execute-extended-command))

;; for solve <kbd>s</kbd> problem
;; (after! evil-snipe (evil-snipe-mode -1))

(when (eq system-type 'darwin)  ; mac specific bindings
  (setq mac-right-option-modifier 'meta ; right option = special characters
))


(use-package! counsel-spotify
  :config
  (setq counsel-spotify-client-id "98c039fb397d4fdc83204163b98598b1"
        counsel-spotify-client-secret "e79d6063b6a440d39c85f1f8453c3678"))

;; setting for telega
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 7891 :enable t
                 :type (:@type "proxyTypeSocks5"
                               :username "" :password ""))
       ))

(setq org-highlight-latex-and-related '(latex script entities))
(use-package! org-roam
  :init
        (setq org-roam-directory "~/org/org-roam")
        (org-roam-db-autosync-mode))

;; org-protocol support for opening a file - needed for ‘my-anki-editor-backlink’.
;; https://org-roam.discourse.group/t/org-roam-and-anki/589/4
(after! org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("org-open-file"
                 :protocol "open-file"
                 :function org-protocol-open-file)))
(defun org-protocol-open-file (fname)
  "Process an org-protocol://open-file?url= style URL with FNAME.
Change a filename by mapping URLs to local filenames as set
in `org-protocol-project-alist'.
The location for a browser's bookmark should look like this:
  javascript:location.href = \\='org-protocol://open-source?url=\\=' + \\
        encodeURIComponent(location.href)"
  ;; As we enter this function for a match on our protocol, the return value
  ;; defaults to nil.
  (let ((f (org-protocol-sanitize-uri
            (plist-get (org-protocol-parse-parameters fname nil '(:file))
                       :file))))
    f))
(defadvice! my-anki-editor-backlink (fn &rest r)
  "Add links from Anki cards back to the file that generated them."
  :around #'anki-editor--build-fields
  (require 'url-util)
  (let ((fields (apply fn r)))
    (when-let*
        (((and fields))
         (note-type (org-entry-get nil anki-editor-prop-note-type))
         (current-file
          (when-let ((f (buffer-file-name)))
            (abbreviate-file-name f)))
         (field-name
          (cond
           ((string= note-type "keypoint") "rel")
           (t nil)))
         (field (assoc field-name fields)))
      (setf (alist-get field-name fields nil nil #'equal)
            (concat (cdr field)
                    (format "<div><hr><p>Source: <a href=\"org-protocol://open-file?file=%s\">%s</p></div>"
                            (url-hexify-string current-file)
                            (org-html-encode-plain-text current-file)))))
    fields))
(after! org
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window))
