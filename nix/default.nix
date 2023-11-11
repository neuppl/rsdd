{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "rsdd";
  version = "unstable-2023-11-11";

  src = ../.;

  cargoPatches = [./0001-Cargo.lock.patch];
  cargoHash = "sha256-yIGvJRSW1qCf4giVZkj2rW7g0CG69nd5Ystle6cG5nU=";
  buildFeatures = [ "ffi" ];

  meta = with lib; {
    description = "Rust decision diagrams";
    homepage = "https://github.com/neuppl/rsdd";
    license = licenses.mit;
    maintainers = with maintainers; [];
  };
}
