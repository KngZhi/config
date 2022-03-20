;; -*- no-byte-compile: t; -*-
;;; junwei.chen/org/packages.el


(package! org-ref)

(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree"))

(package! org-super-agenda)

(require 'org-habit)
