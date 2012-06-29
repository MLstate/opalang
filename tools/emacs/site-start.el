(autoload 'opa-js-mode "opa-js-mode" "OPA JS editing mode." t)
(autoload 'opa-classic-mode "opa-mode" "OPA CLASSIC editing mode." t)
(add-to-list 'auto-mode-alist '("\\.opa$" . opa-js-mode)) ;; <-- Set the default mode here
(add-to-list 'auto-mode-alist '("\\.js\\.opa$" . opa-js-mode))
(add-to-list 'auto-mode-alist '("\\.classic\\.opa$" . opa-classic-mode))

