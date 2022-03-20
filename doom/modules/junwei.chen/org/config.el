;;; junwei.chen/org/config.el -*- lexical-binding: t; -*-
(require 'request)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

;; Recursivly search all org files at [https://stackoverflow.com/a/41969519]
;; (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-files
 (directory-files-recursively "~/org/org-agenda/" "\\.org$"))


(use-package! org-pomodoro
  :hook (org-pomodoro-finished .
         (lambda ()
            (request "https://api.pushcut.io/Ntr_s1RdVcTEhurSzLt4t/notifications/Starting%20short%20break"
             :success (message "notified"))))
  :config
  (setq org-pomodoro-length 30
        org-pomodoro-count 3
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 12
        org-pomodoro-manual-break t
        org-pomodoro-short-break-format "%s"
        org-pomodoro-long-break-format "%s"
        org-pomodoro-format "%s"
        org-pomodoro-finished-sound "~/.config/doom/assets/alarm-bird.mp3"
        org-pomodoro-short-break-sound "~/.config/doom/assets/alarm-bird.mp3"
        org-pomodoro-long-break-sound "~/.config/doom/assets/alarm-bird.mp3"))
;; TODO add notifacation for org-pomodoro
;; https://github.com/cnsunyour/.doom.d/blob/5057fecb5b90d9f013a529d54aac948ac40dbd09/modules/cnsunyour/org/config.el#L336

;; Org Roam
;; https://coredumped.dev/2021/05/26/taking-org-roam-everywhere-with-logseq/
(use-package! org-roam
  :init (setq org-roam-directory "~/org/org-roam"
              org-roam-dailies-directory "journals/")
  :custom
  (org-roam-complete-everywhere t)
  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
  (org-roam-capture-templates
        '(("d" "default" plain
           #'org-roam-capture--get-target "%?"
           :file-name "pages/${slug}" :head "#+title: ${title}\n" :unnarrowed t)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  :config
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

(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; (org-super-agenda-mode)
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-block-separator nil
        org-agenda-tags-column 100
        org-agenda-start-with-log-mode '(closed clock state)
        org-agenda-compact-blocks t)
  (setq org-super-agenda-groups '((:name "Ankify"
                                   :todo "HOLD")
                                  (:name "Today"
                                   :time-grid t
                                   :todo "TODO"
                                   :scheduled t))))
(map! :mode org-mode
      :ni "s-i" #'org-roam-node-insert)

(use-package! org
  :config (setq org-log-done t)
  :after (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window))

;; (setq org-cite-csl-styles-dir "~/Zotero/styles")
;; (setq! citar-bibliography '("~/Zotero/better-bibtex/zotero.bib")
;;        citar-library-paths '("~/Zotero"))
;; (setq! bibtex-completion-library-path '("~/Zotero/better-bibtex/zotero.bib")
;;        bibtex-completion-notes-path '("~/org/zotero-notes"))
