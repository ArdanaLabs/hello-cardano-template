l: haskellNix: cabalFiles:
let
  b = builtins;
  f = cabalFile:
    let
      lines = b.filter b.isString (b.split "\n" (b.readFile cabalFile));
      trimPrefixAndWhitespace = prefix: str: l.pipe str
        [
          (l.removePrefix prefix)
          l.stringToCharacters
          (b.filter (a: a != " "))
          (l.concatStringsSep "")
        ];
      getNamesOfType = t: b.map (trimPrefixAndWhitespace t) (b.filter (l.hasPrefix t) lines);
      name = trimPrefixAndWhitespace "name:" (b.head (b.filter (l.hasPrefix "name:") lines));
      library = b.length (b.filter (l.hasPrefix "library") lines) == 1;
      makeSet = attr: names:
        b.listToAttrs
          (b.map (name: { inherit name; value = haskellNix.${attr}.${name}; }) names);

      checks =
        makeSet "checks"
          (map (test: "${name}:test:${test}") (getNamesOfType "test-suite"));
    in
    {
      packages =
        makeSet "packages"
          ((if library then [ "${name}:lib:${name}" ] else [ ])
          ++ map (exe: "${name}:exe:${exe}") (getNamesOfType "executable")
          )
        // checks;

      inherit checks;
    };
in
haskellNix
  // b.foldl'
  (acc: file:
    l.recursiveUpdate acc (f file)
  )
  { }
  cabalFiles
