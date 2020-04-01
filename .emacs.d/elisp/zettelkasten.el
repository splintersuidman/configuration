(use-package zetteldeft
  :init
  (setq deft-directory "~/docs/notities")
  (setq zetteldeft-id-format "%Y-%m-%d-%H%M")
  (setq zetteldeft-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{3\\}")
  (setq zetteldeft-title-prefix "#+TITLE: ")
  ;; (setq zetteldeft-title-suffix "\n\n")
  (setq deft-extensions '("org" "md" "markdown" "txt" "text"))
  (setq deft-recursive t))
