" let g:chromatica#libclang_path='/usr/local/opt/llvm/lib/'
" TODO: this does not work?
let g:chroamtica#libclang_path = system('echo $(nix-build -E "(import <nixpkgs> {}).llvmPackages.libclang")/lib/libclang.dylib')
let g:chromatica#dotclangfile_seach_path = './'
" let g:chromatica#enable_at_startup=1
