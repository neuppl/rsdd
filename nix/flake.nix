{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    ...
  } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
        ];
      };
      naersk = pkgs.callPackage inputs.naersk {};
      rsdd = pkgs.callPackage ./. {};
      py = pkgs.python3.withPackages (p: [p.graphviz]);
      render-graphviz = pkgs.writeScriptBin "render-graphviz" ''
        ${py}/bin/python ${../bin/render_graphviz.py}
      '';
    in {
      packages = {
        inherit rsdd render-graphviz;
        default = rsdd;
      };
      apps = rec {
        render-graphviz = {
          type = "app";
          program = "${render-graphviz}/bin/render-graphviz";
        };
        bayesian_network_compiler = {
          type = "app";
          program = "${rsdd}/bin/bayesian_network_compiler";
        };
        compare_canonicalize = {
          type = "app";
          program = "${rsdd}/bin/compare_canonicalize";
        };
        one_shot_benchmark = {
          type = "app";
          program = "${rsdd}/bin/one_shot_benchmark";
        };
      };

      devShell = inputs.devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [
          rec {
            pre-commit.hooks = {
              shellcheck.enable = true;
              clippy.enable = true;
              hunspell.enable = true;
              alejandra.enable = true;
              rustfmt.enable = true;
              typos.enable = true;
            };
            languages.rust.enable = true;
            # add a rust-repl
            scripts.repl.exec = "${pkgs.evcxr}/bin/evcxr";
            env.DEVSHELL = "devshell+flake.nix";
            enterShell = pkgs.lib.strings.concatStringsSep "\n" ([
                ''
                  echo ""
                  echo "Hello from $DEVSHELL!"
                  echo "Some tools this environment is equipped with:"
                  echo ""
                ''
              ]
              ++ (builtins.map (
                  p: "echo \"${p.pname}\t\t-- ${p.meta.description}\""
                )
                packages)
              ++ ["echo \"\""]);

            packages =
              with pkgs;
                [
                  lldb
                  cargo
                  rustc
                  rustfmt
                  rust-analyzer
                  clippy
                  cargo-watch
                  cargo-nextest
                  cargo-expand # expand macros and inspect the output
                  cargo-llvm-lines # count number of lines of LLVM IR of a generic function
                  cargo-inspect
                  cargo-criterion
                  evcxr # make sure repl is in a gc-root
                  cargo-play # quickly run a rust file that has a maint function
                ]
                ++ lib.optionals stdenv.isDarwin []
                ++ lib.optionals stdenv.isLinux [
                  cargo-rr
                  rr-unstable
                ]
              # ++ builtins.attrValues self.checks
              ;
          }
        ];
      };
    });
}
