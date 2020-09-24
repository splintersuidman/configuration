{ pkgs, ... }: {
  home.packages = [ pkgs.gyre-fonts ];

  programs.emacs.init.prelude = ''
    (set-face-attribute
     'default
     nil
     :height 105
     :family "DejaVu Sans Mono")

    ;; Test characters:
    ;; ∘ ℕ ℤ ℚ ℝ ⊥ ⊤ ≡ ≈ ≟ ∀ ∃

    ;; Variable pitch face.
    (set-face-attribute 'variable-pitch nil
                        :family "TeX Gyre Pagella"
                        :height 125)
  '';
}
