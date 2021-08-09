(set-face-attribute 'default nil
                    :family "Iosevka Custom"
                    :height 105)

(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil
                    :slant 'italic)

;; Test characters:
;; ∘ ℕ ℤ ℚ ℝ ⊥ ⊤ ≡ ≈ ≟ ∀ ∃

;; Variable pitch face.
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Aile"
                    :height 105)
;; Fixed pitch face.
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Custom")

(provide 'init-font)
