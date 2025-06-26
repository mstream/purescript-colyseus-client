{
  pkgs,
  name,
  lib,
  system ? builtins.currentSystem,
}:

let
  appConfig = import ./app-config.nix {
    inherit name;
  };

  psConfig = appConfig.purescript;
  viteConfig = appConfig.vite;

  frontendModule = import ./frontend.nix {
    inherit pkgs lib name;
    frontend = {
      inherit (viteConfig) viteport settings;
      inherit (psConfig) codeDirs spagoFile;
    };
  };

  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      inherit name;
      frontendPath = ".";
      psDirs = psConfig.codeDirs;
    };
  };

  fileToolsModule = import ./file-tools.nix {
    inherit pkgs name lib;
    frontendPath = ".";
    psDirs = psConfig.codeDirs;
  };

  deployModule = import ./deploy.nix {
    inherit pkgs name lib;
  };

  # Common build inputs for PureScript & Vite development
  commonBuildInputs = with pkgs; [
    # PureScript tools
    esbuild
    nodejs_20
    nixpkgs-fmt
    purs
    purs-tidy
    purs-backend-es
    purescript-language-server
    spago-unstable
    entr
    concurrently

    # Development scripts
    #frontendModule.redis-start
    #frontendModule.vite
    #frontendModule.vite-cleanup
    #frontendModule.spago-watch
    #frontendModule.concurrent
    #frontendModule.dev
    #frontendModule.get-ip

    # File tools
    #fileToolsModule.compile-manifest
    #fileToolsModule.compile-archive
    #manifestModule.generateScript

    # Deployment tools
    #deployModule.build
    #deployModule.stop
    #deployModule.serve
    #deployModule.preview

    # Core utilities
    coreutils
    bash
    gnused
    gnugrep
    jq
    perl
    findutils
    toilet
    lsof
    python3 # For preview server
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
  ];

  darwinInputs =
    if (system == "aarch64-darwin" || system == "x86_64-darwin") then
      (with pkgs.darwin.apple_sdk.frameworks; [
        Cocoa
        CoreServices
      ])
    else
      [ ];

  devShell = pkgs.mkShell {
    inherit name;

    inherit nativeBuildInputs;

    buildInputs = commonBuildInputs ++ darwinInputs;

    shellHook = ''
      # Create necessary directories for project
      mkdir -p output
      mkdir -p "$(pwd)/script/concat_archive/output" "$(pwd)/script/concat_archive/archive" "$(pwd)/script/concat_archive/.hashes"

      echo "Welcome to the ${lib.toSentenceCase name} dev environment!"

      echo "Available commands:"
      echo "  Frontend:"
      echo "    vite                   - Start Vite development server"
      echo "    vite-cleanup           - Clean frontend build artifacts"
      echo "    spago-watch            - Watch PureScript files for changes"
      echo "    concurrent             - Run concurrent development tasks"
      echo "    dev                    - Start all development services"
      echo "    get-ip                 - Display your network IP address"
      echo ""
      echo "  Deployment:"
      echo "    serve                  - Run Vite server directly (no tmux)"
      echo "    build                  - Build for production (--preview to test)"
      echo "    preview                - Create a simple bundled preview" 
      echo "    stop                   - Kill all development processes"
      echo ""
      toilet ${lib.toSentenceCase name} -t --metal
    '';
  };

in
{
  inherit devShell;
}
