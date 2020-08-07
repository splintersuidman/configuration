{ ... }:
{
  programs.emacs.init.prelude = ''
    (set-face-attribute
     'default
     nil
     :height 105
     :family "DejaVu Sans Mono")

    ;; Test characters:
    ;; ∘ ℕ ℤ ℚ ℝ ⊥ ⊤ ≡ ≈ ≟ ∀ ∃
  '';
}
