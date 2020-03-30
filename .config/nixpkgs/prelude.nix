with builtins;
builtins // rec {
  # Return whether the argument is the empty list.
  isEmpty = l: l == [];

  # Zip two lists with a function. The result will have the same
  # length as the shortest of the two lists.
  #
  # Example:
  #      zipWith (x: y: x + y) [ 1 2 3 4 ] [ 5 6 7 ]
  #   == [ 6 8 10 ]
  zipWith = f: xs: ys:
    if isEmpty xs || isEmpty ys
    then []
    else [(f (head xs) (head ys))] ++ zipWith f (tail xs) (tail ys);

  # Return a list of all name-value pairs in an attrs. Each pair has
  # the form { name = <name>; value = <value>; }.
  #
  # Example:
  #      attrsToList { a = 1; b = 2; c = { d = 3; }; }
  #   == [ { name = "a"; value = 1; }
  #        { name = "b"; value = 2; }
  #        { name = "c"; value = { d = 3; }; }
  #      ]
  attrsToList = attrs:
    zipWith
      (name: value: { inherit name value; })
      (builtins.attrNames attrs)
      (attrValues attrs);

  # Concatenates all lists in a nested attrs, ignoring the names.
  #
  # Example:
  #      flattenAttrs { a = [ 1 2 3 ]; b = { c = [ 4 5 6 ]; }; }
  #   == [ 1 2 3 4 5 6 ]
  flattenAttrs = attrs:
    builtins.concatMap
      ({ name, value }:
        let type = builtins.typeOf value; in
        if type == builtins.typeOf []
        then value
        else
          if type == typeOf { }
          then flattenAttrs value
          else abort (
            "flattenAttrs: expected list or set, found " +
            type + " in set under name \"" + name "\""
          )
      )
      (attrsToList attrs);

  # Return a list of files of the files with a .nix extension in the
  # directory.
  nixFilesOfDir = dirPath:
    let dir = attrsToList (readDir dirPath);
        files = map
          ({ name, ... }: dirPath + name)
          (filter
            ({ value, ... }: value == "regular" || value == "symlink")
          dir);
    in
      filter
        (file:
          let fileStr = toString file;
          in stringLength fileStr > 4
             && substring (stringLength fileStr - 4) 4 fileStr == "nix")
        files;
}
