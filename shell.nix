let
  packages = import ./.;
  inherit (packages) pkgs plutus-starter cardano-node;
  inherit (plutus-starter) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with plutus-starter; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      cardano-node.cardano-cli
      cardano-node.cardano-node
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
