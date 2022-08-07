{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
	name="dev-environment";
	buildInputs = [
		pkgs.ocaml
		pkgs.rlwrap
		pkgs.ocamlPackages.ocp-indent
	];
	shellHook = ''
		echo "Ocaml dev environment..."
	'';
}

