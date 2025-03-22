{
  description = "A simple NixOS flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # ghostty = {
    #   url = "github:ghostty-org/ghostty";
    # };
  };

  outputs = { 
    self, 
    nixpkgs,
    # ghostty,
    ... }@inputs: {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
	  # {
	  #          environment.systemPackages = [
	  #            ghostty.packages.x86_64-linux.default
	  #          ];
	  # }
	];
      };
    };
}
