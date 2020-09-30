{ pkgs, ... }: {
  home.packages = [ pkgs.dejavu_fonts ];

  programs.emacs.init.prelude = ''
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 105)

    ;; Test characters:
    ;; ∘ ℕ ℤ ℚ ℝ ⊥ ⊤ ≡ ≈ ≟ ∀ ∃

    ;; Variable pitch face.
    (set-face-attribute 'variable-pitch nil
                        :family "DejaVu Sans"
                        :height 105)
  '';
}
