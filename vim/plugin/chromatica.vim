" let g:chromatica#libclang_path='/usr/local/opt/llvm/lib/'
" TODO: this does not work?
" let g:chroamtica#libclang_path = system('echo $(nix-build -E "(import <nixpkgs> {}).llvmPackages.libclang")/lib/libclang.dylib')
" let g:chromatica#libclang_path = system("nix-instantiate --eval -E '\"${(import <nixpkgs> {}).llvmPackages.libclang}/lib/libclang.dylib\"' | sed 's/\"//g'")
" let g:chromatica#libclang_path = '/nix/store/h01iqphxm1p4jmxmwsy8zrlrhskja7v5-clang-5.0.2-lib/lib/libclang.dylib'
let g:chromatica#libclang_path = '/usr/local/Cellar/llvm/7.0.1/lib/libclang.dylib'
let g:chromatica#dotclangfile_seach_path = './'
" let g:chromatica#enable_at_startup=1
