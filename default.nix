let
  haskellNix = import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/a8961d94a52f0cd654ab1844419f6ac00171d112.tar.gz")
    { };
  pkgs = import haskellNix.sources.nixpkgs-2411 haskellNix.nixpkgsArgs;

  haskellProject = pkgs.haskell-nix.cabalProject {
    compiler-nix-name = "ghc966";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "todo";
      src = ./.;
    };
  };

  elmProject = pkgs.stdenv.mkDerivation {
    name = "fe";
    src = ./fe;
    buildInputs = [
      pkgs.elmPackages.elm
      pkgs.nodePackages.uglify-js
      haskellProject.be.components.exes.generate-elm-api
    ];
    buildPhase = pkgs.elmPackages.fetchElmDeps {
      elmPackages = import ./fe/elm-srcs.nix;
      elmVersion = "0.19.1";
      registryDat = ./fe/registry.dat;
    };
    installPhase = ''
      generate-elm-api
      elm make src/Main.elm --output elm.input.js
      mkdir -p $out
      uglifyjs elm.input.js \
        --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        | uglifyjs --mangle --output $out/elm.js
    '';
  };

  backend = haskellProject.be.components.exes.be;

  frontend = pkgs.runCommandLocal "frontend" { } ''
    mkdir -p $out/var/www
    cp -r ${./fe/assets}/* $out/var/www
    cp -f ${elmProject}/elm.js $out/var/www/elm.js
  '';

  caddyfile = pkgs.writeTextFile {
    name = "Caddyfile";
    text = ''
      :80 {
          handle /api/* {
              reverse_proxy localhost:8080
          }
          handle {
            root /var/www
            file_server
          }
      }
    '';
  };

in pkgs.dockerTools.buildImage {
  name = "todo-docker";
  tag = "latest";
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "caddy";
    imageDigest =
      "sha256:cf1bd22f2415bc99b785ece1df2c49d32985ac4ac42d84df335be17350593693";
    sha256 = "sha256-ybjZhDP3ySyk5lskib1Sa1NrasGCLWDUtFCPot1LnP4=";
    finalImageTag = "2.9.1-alpine";
    finalImageName = "caddy";
  };
  copyToRoot = [ frontend ];
  config = {
    Cmd = [
      "sh"
      "-c"
      "caddy run --adapter caddyfile --config ${caddyfile} & ${backend}/bin/be"
    ];
  };
}
