;;; A small init file setting you up for tests.
(package-initialize)
(setq backup-inhibited t)
(setq auto-save-default nil)
(add-hook 'org-mode-hook
          (lambda ()
            (org-menu-mode 1)))
