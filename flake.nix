{
  description = "Colyseus.js wrapper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, easy-purescript-nix, flake-utils, nixpkgs, ... }@inputs:
    let
      name = "colyseus-client";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

    in flake-utils.lib.eachSystem supportedSystems (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        easy-ps = import easy-purescript-nix { inherit pkgs; };
      in
        {
          devShell = pkgs.mkShell {
            inherit name; 
            buildInputs = with pkgs; [ git nodejs nodePackages.typescript ] 
              ++ (with easy-ps; [ purs spago ]);
             
            shellHook = ''
              PS1="\[\e[33m\][\[\e[m\]\[\e[34;40m\]${name}:\[\e[m\]\[\e[36m\]\w\[\e[m\]\[\e[33m\]]\[\e[m\]\[\e[32m\]\\$\[\e[m\] "
            '';
          };
        }
    );
}
