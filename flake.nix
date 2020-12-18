{
  description = "beam-mysql";

  inputs = {
    beam.inputs.euler-build.follows = "euler-build";
  };

  outputs = flakeInputs@{ self, euler-build, ... }:
    euler-build.mkEulerFlake {
      overlayPath = ./nix/overlay.nix;
      mkConfig = { nixpkgs }: {
        flakeName = "beam-mysql";
        defaultPackageName = "beam-mysql";
        exportPackages = [
          "beam-mysql"
        ];
        shellTools = with nixpkgs; [
          # haskellPackages.cabal-fmt
        ];
        # shellAttrs = {
        # };
      };
      inputs = flakeInputs;
    };
}
