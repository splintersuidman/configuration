# Local Nix packages

This folder contains local packages for Nix.
The reasons for not using the global `nixpkgs`:

- `ranger`: this is a copy of the `ranger` in `nixpkgs`, but because of an error with the locale I could not fix, a substitution was added.

## Using this

```sh
ln -s $local ~/.nix-defexpr/local
```

Where `$local` is the path to this folder.

To install a package (with name `$package`):

```sh
nix-env -iA local.$package
```
