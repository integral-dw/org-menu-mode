(defun om-spy ()
  (interactive)
  (add-hook 'post-command-hook
            #'om-spy-update t t)
  (om-spy-update)
  (display-buffer "*OM Spy*"))

(add-hook 'org-mode-hook #'om-spy)
(add-hook 'org-mode-hook #'org-menu-mode)

(defun om-spy-update ()
  (let* ((spy (get-buffer-create "*OM Spy*"))
         (infos nil))
    (setq infos
          `(("Point" ,(point))
            ("M Point" ,org-menu-fl--point)
            ("Active Region" ,org-menu-fl--active-region)
            ("Right Edge" ,(org-menu-fl--get-prop 'right-edge))
            ("Active Region (Text Property)"
             ,(get-text-property (point) 'org-menu-region))))
    (with-current-buffer spy
      (erase-buffer)
      (goto-char (point-min))
      (dolist (info infos)
        (insert (apply #'format "%s: %s" info))
        (newline)))))
