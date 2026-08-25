{
  description = "Main config for my NixOS system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

  };

  outputs = { 
    self, 
    nixpkgs,
    unstable,
    ... }@inputs: {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
	  {
	    _module.args.unstablePkgs = inputs.unstable.legacyPackages.x86_64-linux;
	  }
	  ./configuration.nix
	];
      };
    };
}
