with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        (with pkgsMusl.ocaml-ng.ocamlPackages_4_10; [
            ocaml
            ocp-indent
        ])
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
