with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_10; [
            ocp-indent
            pkgsMusl.ocaml
        ])
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
