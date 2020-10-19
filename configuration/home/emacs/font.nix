{ pkgs, ... }: {
  home.packages = [ pkgs.dejavu_fonts ];

  programs.emacs.init.prelude = ''
    (set-face-attribute 'default nil
                        :family "Iosevka Custom"
                        :height 110)

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
  '';
}
