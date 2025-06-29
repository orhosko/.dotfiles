{
  description = "Main config for my NixOS system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # ghostty = {
    #   url = "github:ghostty-org/ghostty";
    # };
  };

  outputs = { 
    self, 
    nixpkgs,
    unstable,
    # ghostty,
    ... }@inputs: {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
	  {
	    _module.args.unstablePkgs = inputs.unstable.legacyPackages.x86_64-linux;
	  }
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
