with import <nixpkgs> { };
mkShell {
  buildInputs = [ pandoc ] ++ [
    (haskellPackages.ghcWithPackages
      (p: with p; [ frontmatter feed xml-conduit pandoc ]))
    haskell-language-server
    ghcid
    ormolu
    hlint
    nixfmt
  ];
}
