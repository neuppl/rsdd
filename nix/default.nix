{ lib
, rustPlatform
, fetchFromGitHub
}:

rustPlatform.buildRustPackage rec {
  pname = "rsdd";
  version = "unstable-2023-03-03";

  src = ../.;

  cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

  meta = with lib; {
    description = "Rust decision diagrams";
    homepage = "https://github.com/pmall-neu/rsdd";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
