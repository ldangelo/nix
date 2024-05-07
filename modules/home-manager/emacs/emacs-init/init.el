;; load the new version of org-mode before loading 'literate' config

;;
;; this is needed to force org babel to re-tangle the file
;; otherwise when using nix it loads config.el instead
;; for some strange reason
(delete-file "~/.config/emacs/config.el")

(org-babel-load-file "~/.config/emacs/config.org")
