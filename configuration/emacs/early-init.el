(setq package-enable-at-startup nil)
(setq package-archives nil)

(setq gc-cons-threshold 134217728)

(advice-add #'x-apply-session-resources :override #'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-inhibit-implied-resize t)
(setq comp-deferred-compilation nil)
