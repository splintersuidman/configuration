{ pkgs, ... }:
let
  sleep = "${pkgs.coreutils}/bin/sleep";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  setxkbmapCommand = "${setxkbmap} -option compose:ralt";
in {
  imports = [ ../../modules/home/kmonad.nix ];

  home.keyboard = null;

  # NOTE: I would like to swap () and [], but david-janssen/kmonad#111 gets in
  # the way: shift-9 gets translated into {.
  services.kmonad = {
    package = pkgs.kmonad;
    keyboards = {
      builtin = let device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      in {
        enable = true;
        inherit device;
        config = ''
          (defcfg
            input (device-file "${device}")
            output (uinput-sink "KMonad built-in"
              "${sleep} 1 && ${setxkbmapCommand}")

            fallthrough true)

          (defsrc
            esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  home end  ins  del
            grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
            caps a    s    d    f    g    h    j    k    l    ;    '    ret
            lsft      z    x    c    v    b    n    m    ,    .    /    rsft
            wkup lctl lmet lalt      spc                 ralt ssrq rctl pgup up   pgdn
                                                                        left down rght)

          (defalias
            ;; xcape-like key: escape when tapped, control when hold.
            xcp (tap-hold-next-release 400 esc ctl)
            ;; Return when tapped, control when hold.
            rct (tap-hold-next-release 400 ret ctl)
            lsf (around lsft (layer-toggle qwerty-shift))
            rsf (around rsft (layer-toggle qwerty-shift)))

          (deflayer qwerty
            esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  home end  ins  del
            grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
            @xcp a    s    d    f    g    h    j    k    l    ;    '    @rct
            lsft      z    x    c    v    b    n    m    ,    .    /    rsft
            wkup lctl lmet lalt      spc                 ralt ssrq rctl pgup up   pgdn
                                                                        left down rght)

          (deflayer qwerty-shift
            esc   f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  home end  ins  del
            ~     !    @    #    $    %    ^    &    *    \(   \)   \_   +    bspc
            S-tab Q    W    E    R    T    Y    U    I    O    P    {    }    |
            @xcp  A    S    D    F    G    H    J    K    L    :    "    @rct
            XX         Z    X    C    V    B    N    M    <    >    ?    XX
            wkup lctl lmet lalt      spc                 ralt ssrq rctl pgup up   pgdn
                                                                        left down rght)
        '';
      };

      leopold =
        let device = "/dev/input/by-id/usb-HID_Keyboard_HID_Keyboard-event-kbd";
        in {
          enable = true;
          inherit device;
          config = ''
            (defcfg
              input (device-file "${device}")
              output (uinput-sink "KMonad Leopold"
                "${sleep} 1 && ${setxkbmapCommand}")

              fallthrough true)

            (defsrc
              esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12       ssrq slck pause
              grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc ins  home pgup
              tab  q    w    e    r    t    y    u    i    o    p    [    ]    \    del  end  pgdn
              caps a    s    d    f    g    h    j    k    l    ;    '    ret
              lsft      z    x    c    v    b    n    m    ,    .    /    rsft           up
              lctl lmet lalt           spc                 ralt      menu rctl      left down rght)

            (defalias
              ;; xcape-like key: escape when tapped, control when hold.
              xcp (tap-hold-next-release 400 esc ctl)
              ;; Return when tapped, control when hold.
              rct (tap-hold-next-release 400 ret ctl)
              lsf (around lsft (layer-toggle qwerty-shift))
              rsf (around rsft (layer-toggle qwerty-shift)))

            (deflayer qwerty
              esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12       ssrq slck pause
              grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc ins  home pgup
              tab  q    w    e    r    t    y    u    i    o    p    [    ]    \    del  end  pgdn
              @xcp a    s    d    f    g    h    j    k    l    ;    '    @rct
              lsft      z    x    c    v    b    n    m    ,    .    /    rsft           up
              lctl lmet lalt           spc                 ralt      menu rctl      left down rght)

            (deflayer qwerty-shift
              esc   f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12       ssrq slck pause
              ~     !    @    #    $    %    ^    &    *    \(   \)   \_   +    bspc ins  home pgup
              S-tab Q    W    E    R    T    Y    U    I    O    P    {    }    |    del  end  pgdn
              @xcp  A    S    D    F    G    H    J    K    L    :    "    @rct
              XX         Z    X    C    V    B    N    M    <    >    ?    XX             up
              lctl  lmet lalt           spc                 ralt      menu rctl      left down rght)
          '';
        };
    };
  };

  home.file.".XCompose".text = ''
    # Include the defaults
    include "%L"

    # Mathematical symbols
    <Multi_key> <a> <l> <l>                        : "∀"
    <Multi_key> <e> <x>                            : "∃"
    <Multi_key> <exclam> <e> <x>                   : "∄"
    <Multi_key> <exclam> <equal> <period>          : "≠"
    <Multi_key> <colon> <equal>                    : "≔"
    <Multi_key> <equal> <equal>                    : "≡"
    <Multi_key> <exclam> <equal> <equal>           : "≢"
    <Multi_key> <asciitilde> <equal>               : "≅"
    <Multi_key> <exclam> <asciitilde> <equal>      : "≇"
    <Multi_key> <asciitilde> <asciitilde>          : "≈"
    <Multi_key> <exclam> <asciitilde> <asciitilde> : "≉"
    <Multi_key> <equal> <greater>                  : "⇒"
    <Multi_key> <less> <equal>                     : "≤"
    <Multi_key> <greater> <equal>                  : "≥"
    <Multi_key> <e> <l> <e> <m>                    : "∈"
    <Multi_key> <exclam> <e> <l> <e> <m>           : "∉"
    <Multi_key> <o> <asterisk>                     : "∘"
    <Multi_key> <o> <period>                       : "·"
    <Multi_key> <slash> <backslash>                : "∧"
    <Multi_key> <backslash> <slash>                : "∨"
    <Multi_key> <parenleft> <parenleft>            : "⦅"
    <Multi_key> <parenright> <parenright>          : "⦆"
    <Multi_key> <bracketleft> <bracketleft>        : "⟦"
    <Multi_key> <bracketright> <bracketright>      : "⟧"
    <Multi_key> <braceleft> <braceleft>            : "⦃"
    <Multi_key> <braceright> <braceright>          : "⦄"
    <Multi_key> <colon> <colon>                    : "∷"
    <Multi_key> <bar> <minus>                      : "⊢"
    <Multi_key> <minus> <bar>                      : "⊣"
    <Multi_key> <i> <n> <t                         : "∫"
    <Multi_key> <B> <o> <t>                        : "⊥"
    <Multi_key> <T> <o> <p>                        : "⊤"
    <Multi_key> <C> <a> <p>                        : "∩"
    <Multi_key> <C> <u> <p>                        : "∪"
    # Blackboard bold for number sets
    <Multi_key> <b> <N> : "ℕ"
    <Multi_key> <b> <Z> : "ℤ"
    <Multi_key> <b> <Q> : "ℚ"
    <Multi_key> <b> <R> : "ℝ"
    <Multi_key> <b> <C> : "ℂ"
    <Multi_key> <b> <H> : "ℍ"
    # Greek letters
    <Multi_key> <g> <A> : "Α"
    <Multi_key> <g> <B> : "Β"
    <Multi_key> <g> <C> : "Ψ"
    <Multi_key> <g> <D> : "Δ"
    <Multi_key> <g> <E> : "Ε"
    <Multi_key> <g> <F> : "Φ"
    <Multi_key> <g> <G> : "Γ"
    <Multi_key> <g> <H> : "Η"
    <Multi_key> <g> <I> : "Ι"
    <Multi_key> <g> <J> : "Ξ"
    <Multi_key> <g> <K> : "Κ"
    <Multi_key> <g> <L> : "Λ"
    <Multi_key> <g> <M> : "Μ"
    <Multi_key> <g> <N> : "Ν"
    <Multi_key> <g> <O> : "Ο"
    <Multi_key> <g> <P> : "Π"
    <Multi_key> <g> <R> : "Ρ"
    <Multi_key> <g> <S> : "Σ"
    <Multi_key> <g> <T> : "Τ"
    <Multi_key> <g> <U> : "Υ"
    <Multi_key> <g> <V> : "Ω"
    <Multi_key> <g> <W> : "Σ"
    <Multi_key> <g> <X> : "Χ"
    <Multi_key> <g> <Y> : "Θ"
    <Multi_key> <g> <Z> : "Ζ"
    <Multi_key> <g> <a> : "α"
    <Multi_key> <g> <b> : "β"
    <Multi_key> <g> <c> : "ψ"
    <Multi_key> <g> <d> : "δ"
    <Multi_key> <g> <e> : "ε"
    <Multi_key> <g> <f> : "ϕ"
    <Multi_key> <g> <g> : "γ"
    <Multi_key> <g> <h> : "η"
    <Multi_key> <g> <i> : "ι"
    <Multi_key> <g> <j> : "ξ"
    <Multi_key> <g> <k> : "κ"
    <Multi_key> <g> <l> : "λ"
    <Multi_key> <g> <m> : "μ"
    <Multi_key> <g> <n> : "ν"
    <Multi_key> <g> <o> : "ο"
    <Multi_key> <g> <p> : "π"
    <Multi_key> <g> <q> : ";"
    <Multi_key> <g> <r> : "ρ"
    <Multi_key> <g> <s> : "σ"
    <Multi_key> <g> <t> : "τ"
    <Multi_key> <g> <u> : "υ"
    <Multi_key> <g> <v> : "ω"
    <Multi_key> <g> <w> : "ς"
    <Multi_key> <g> <x> : "χ"
    <Multi_key> <g> <y> : "θ"
    <Multi_key> <g> <z> : "ζ"
    # Superscript
    <Multi_key> <asciicircum> <1>            : "¹"
    <Multi_key> <asciicircum> <2>            : "²"
    <Multi_key> <asciicircum> <3>            : "³"
    <Multi_key> <asciicircum> <4>            : "⁴"
    <Multi_key> <asciicircum> <5>            : "⁵"
    <Multi_key> <asciicircum> <6>            : "⁶"
    <Multi_key> <asciicircum> <7>            : "⁷"
    <Multi_key> <asciicircum> <8>            : "⁸"
    <Multi_key> <asciicircum> <9>            : "⁹"
    <Multi_key> <asciicircum> <0>            : "⁰"
    <Multi_key> <asciicircum> <asterisk> <a> : "ᵃ"
    <Multi_key> <asciicircum> <asterisk> <b> : "ᵇ"
    <Multi_key> <asciicircum> <asterisk> <c> : "ᶜ"
    <Multi_key> <asciicircum> <asterisk> <d> : "ᵈ"
    <Multi_key> <asciicircum> <asterisk> <e> : "ᵉ"
    <Multi_key> <asciicircum> <asterisk> <f> : "ᶠ"
    <Multi_key> <asciicircum> <asterisk> <g> : "ᵍ"
    <Multi_key> <asciicircum> <asterisk> <h> : "ʰ"
    <Multi_key> <asciicircum> <asterisk> <i> : "ⁱ"
    <Multi_key> <asciicircum> <asterisk> <j> : "ʲ"
    <Multi_key> <asciicircum> <asterisk> <k> : "ᵏ"
    <Multi_key> <asciicircum> <asterisk> <l> : "ˡ"
    <Multi_key> <asciicircum> <asterisk> <m> : "ᵐ"
    <Multi_key> <asciicircum> <asterisk> <n> : "ⁿ"
    <Multi_key> <asciicircum> <asterisk> <o> : "ᵒ"
    <Multi_key> <asciicircum> <asterisk> <n> : "ⁿ"
    <Multi_key> <asciicircum> <asterisk> <p> : "ᵖ"
    <Multi_key> <asciicircum> <asterisk> <r> : "ʳ"
    <Multi_key> <asciicircum> <asterisk> <s> : "ˢ"
    <Multi_key> <asciicircum> <asterisk> <t> : "ᵗ"
    <Multi_key> <asciicircum> <asterisk> <u> : "ᵘ"
    <Multi_key> <asciicircum> <asterisk> <v> : "ᵛ"
    <Multi_key> <asciicircum> <asterisk> <w> : "ʷ"
    <Multi_key> <asciicircum> <asterisk> <x> : "ˣ"
    <Multi_key> <asciicircum> <asterisk> <y> : "ʸ"
    <Multi_key> <asciicircum> <asterisk> <z> : "ᶻ"
    # Superscript
    <Multi_key> <underscore> <1>            : "₁"
    <Multi_key> <underscore> <2>            : "₂"
    <Multi_key> <underscore> <3>            : "₃"
    <Multi_key> <underscore> <4>            : "₄"
    <Multi_key> <underscore> <5>            : "₅"
    <Multi_key> <underscore> <6>            : "₆"
    <Multi_key> <underscore> <7>            : "₇"
    <Multi_key> <underscore> <8>            : "₈"
    <Multi_key> <underscore> <9>            : "₉"
    <Multi_key> <underscore> <0>            : "₀"
    <Multi_key> <underscore> <asterisk> <a> : "ₐ"
    <Multi_key> <underscore> <asterisk> <e> : "ₑ"
    <Multi_key> <underscore> <asterisk> <h> : "ₕ"
    <Multi_key> <underscore> <asterisk> <i> : "ᵢ"
    <Multi_key> <underscore> <asterisk> <j> : "ⱼ"
    <Multi_key> <underscore> <asterisk> <k> : "ₖ"
    <Multi_key> <underscore> <asterisk> <l> : "ₗ"
    <Multi_key> <underscore> <asterisk> <m> : "ₘ"
    <Multi_key> <underscore> <asterisk> <n> : "ₙ"
    <Multi_key> <underscore> <asterisk> <o> : "ₒ"
    <Multi_key> <underscore> <asterisk> <p> : "ₚ"
    <Multi_key> <underscore> <asterisk> <r> : "ᵣ"
    <Multi_key> <underscore> <asterisk> <s> : "ₛ"
    <Multi_key> <underscore> <asterisk> <t> : "ₜ"
    <Multi_key> <underscore> <asterisk> <u> : "ᵤ"
    <Multi_key> <underscore> <asterisk> <v> : "ᵥ"
    <Multi_key> <underscore> <asterisk> <x> : "ₓ"
  '';
}
