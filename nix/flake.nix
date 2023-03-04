{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    naersk.url = "github:nix-community/naersk";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    ...
  } @ inputs: inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
        ];
      };
      naersk = pkgs.callPackage inputs.naersk {};
      rsdd = (naersk.buildPackage { src = ../.; })
        .overrideAttrs (prev: {
          fixupPhase =
            ''
              # FIXME move to the .lib output
              mkdir $out/lib
              mv target/release/librsdd.a $out/lib/
            ''
            + (
              if pkgs.stdenv.isDarwin
              then "mv target/release/librsdd.dylib $out/lib/" # untested
              else "mv target/release/librsdd.so $out/lib/"
            );
        });
      py = pkgs.python3.withPackages (p: [p.graphviz]);
      render-graphviz = pkgs.writeScriptBin "render-graphviz" ''
        ${py}/bin/python ${../bin/render_graphviz.py}
      '';

      # visualize = ;
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
    };

    devShell = devenv.lib.mkShell {
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
      enterShell = ''
        echo "hello from $DEVSHELL!"
        echo "Some tools this environment is equipped with:"
        echo ""
        echo "lldb                -- ${pkgs.lldb.meta.description}"
        echo "cargo               -- ${pkgs.cargo.meta.description}"
        echo "rustc               -- ${pkgs.rustc.meta.description}"
        echo "rustfmt             -- ${pkgs.rustfmt.meta.description}"
        echo "rust-analyzer       -- ${pkgs.rust-analyzer.meta.description}"
        echo "clippy              -- ${pkgs.clippy.meta.description}"
        echo "cargo-watch         -- ${pkgs.cargo-watch.meta.description}"
        echo "cargo-nextest       -- ${pkgs.cargo-nextest.meta.description}"
        echo "cargo-expand        -- ${pkgs.cargo-expand.meta.description}"
        echo "cargo-llvm-lines    -- ${pkgs.cargo-llvm-lines.meta.description}"
        echo "cargo-inspect       -- ${pkgs.cargo-inspect.meta.description}"
        echo "cargo-criterion     -- ${pkgs.cargo-criterion.meta.description}"
        echo "cargo-play          -- ${pkgs.cargo-play.meta.description}"
        echo "evcxr (alias: repl) -- ${pkgs.evcxr.meta.description}"
      '';

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
    };
  });
}
