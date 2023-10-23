Ensure Ocaml environment
------------------------------
INSTALL OPAM
Mac:
brew install opam
MacPorts:
sudo port install opam
Windows:
sudo apt install opam

INITIALIZE OPAM
Linux, Mac, and WSL2:
opam init --bare -a -y
WSL1:
opam init --bare -a -y --disable-sandboxing

CREATE OPAM SWITCH
opam switch create cs3110-final-project ocaml-base-compiler.5.0.0
eval $(opam env)

ENSURE OPAM SWITCH
opam switch list
CHECK IF SWITCH HAS BEEM CREATED
------------------------------

Install Necessary Packages
------------------------------
GENERAL PACKAGES
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
PACKAGES NECESSARY FOR STOCKS-SIMULATOR
opam install core
opam install yojson
opam install dune
------------------------------

To run stock-simulator cli
------------------------------
dune build
dune exec bin/main.exe
------------------------------

