{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgsSrc ? import haskellNix.sources.nixpkgs-2105
, pkgs ? pkgsSrc (haskellNix.nixpkgsArgs // { inherit system; })
, nix-filter ? import sources.nix-filter
, system ? builtins.currentSystem
, prod ? false
, ghcjs-optimizer ? import sources.ghcjs-optimizer {
    inherit pkgs;
    fileExtentionsToZopfli = [ ];
    jsToOptimize = { "all.js" = "bin/*/all.js"; };
    filesToCopy = {
      "index.html" = sources.ghcjs-optimizer + "/index.html";
    };
    copyInsteadOfOptimizeJs = !prod;
  }
}:
let
  insaneOptimizationFlags = [
    "-O2"
    "-fexpose-all-unfoldings"
    "-fspecialise-aggressively"
  ];
  addLocalOptions = x:
    if prod then x // {
      ghcOptions = [ "-Werror" ] ++ insaneOptimizationFlags;
    }
    else x // {
      ghcOptions = [ "-O0" ];
    };

  hsPkgs = pkgs.haskell-nix.cabalProject {
    src = nix-filter {
      root = ./.;
      name = "reflex-route-simple";
      include = [
        ./reflex-route-simple.cabal
        ./cabal.project
      ];
    };

    modules = [
      {
        ghcOptions = insaneOptimizationFlags;
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        packages.reflex-route-simple = addLocalOptions {
          src = nix-filter {
            root = ./.;
            name = "reflex-route-simple";
            include = [
              (nix-filter.inDirectory ./src)
              (nix-filter.inDirectory ./app)
              ./reflex-route-simple.cabal
              ./cabal.project
            ];
          };
        };
      }
    ];

    index-state = "2021-10-08T23:58:38Z";
    compiler-nix-name = "ghc8107";
  };
  example = ghcjs-optimizer
    hsPkgs.projectCross.ghcjs.hsPkgs.reflex-route-simple.components.exes.reflex-route-simple-example;
  caddyfile = pkgs.writeText "Caddyfile" ''
    {
      admin off
    }
    http://:8000 {
      route {
        header Cache-Control no-cache
        file_server {
          root ${example}
        }
    }
  '';
in
hsPkgs // {
  run-example = pkgs.writeScript "run-caddy.sh" ''
    #!${pkgs.bash}/bin/bash
    ${pkgs.caddy}/bin/caddy run --config ${caddyfile} --adapter caddyfile
  '';
}
