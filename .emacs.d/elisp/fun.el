;; xkcd comics
(use-package xkcd
  :commands xkcd
  :config
  (evil-leader/set-key-for-mode 'xkcd-mode
    "cc" 'xkcd-get
    "cn" 'xkcd-next
    "cp" 'xkcd-prev
    "cr" 'xkcd-rand
    "ct" 'xkcd-alt-text))
