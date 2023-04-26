{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "rsdd";
  version = "unstable-2023-03-03";

  src = ../.;

  cargoHash = "sha256-mi27RanxrNy6ncpFUAvR62gSj+JnvLc1BPZD4NI56yg=";
  cargoPatches = [./0001-Cargo.lock.patch];

  meta = with lib; {
    description = "Rust decision diagrams";
    homepage = "https://github.com/neuppl/rsdd";
    license = licenses.mit;
    maintainers = with maintainers; [];
  };
}
