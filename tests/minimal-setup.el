;;; A small init file setting you up for tests.
(package-initialize)

(add-hook 'org-mode-hook
          (lambda ()
            (org-menu-mode 1)))
