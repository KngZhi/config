;;; junwei.chen/bindings/config.el -*- lexical-binding: t; -*-


(when (eq system-type 'darwin)  ; mac specific bindings
  (setq mac-right-option-modifier 'meta ; right option = special characters
))


(map! (:leader
        ";" #'execute-extended-command
        ":" #'pp-eval-expression)
      (:i "s-;" #'execute-extended-command))
