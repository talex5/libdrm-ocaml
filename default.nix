{ pkgconf, libdrm, ocamlPackages }:

ocamlPackages.buildDunePackage {
  pname = "libdrm";
  version = "0.1";

  src = ./.;

  nativeBuildInputs = [ pkgconf ];

  buildInputs = [ ocamlPackages.dune-configurator ];

  propagatedBuildInputs = [ libdrm ] ++ (with ocamlPackages; [ ctypes-foreign ctypes fmt ]);
}
