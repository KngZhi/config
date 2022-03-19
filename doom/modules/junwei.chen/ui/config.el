;;; junwei.chen/ui/config.el -*- lexical-binding: t; -*-

;; 拆分窗口时默认把焦点定在新窗口，doom为了和vim保持一致，竟然把这点改回去了
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; 调整Mac下窗口和全屏显示方式
(when (eq window-system 'ns)
  (setq ns-use-thin-smoothing t
        ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil))

;; Frame maximized when startup
(add-hook 'window-setup-hook #'toggle-frame-maximized t)

;; enable fill-column-indicator (since emacs 27.1) in prog mode see
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Displaying-Boundaries.html
(add-hook 'prog-mode-hook (lambda ()
        (setq fill-column 80)
        (setq display-fill-column-indicator t)
        (setq display-fill-column-indicator-column t)
        (display-fill-column-indicator-mode)
        (global-display-fill-column-indicator-mode)))


(use-package! ace-window
  :init
  (map! (:leader "w w" #'ace-window))
  (setq aw-scope 'global)
  (custom-set-faces
        '(aw-leading-char-face
          ((t (:inherit ace-jump-face-foreground :height 3.0))))))


(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
