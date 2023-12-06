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
opam install lwt_ssl tls-lwt
------------------------------

To run stock-simulator cli
------------------------------
dune build
dune exec bin/main.exe
------------------------------

Or use make command
------------------------------
make build
make trade

Additional Make Commands
------------------------------
make test (Runes OUnits test suite)
make bisect (Bisect report on testing code coverage)
make build (Builds files for running)
make trade (Opens CLI forr running program features)
make doc (Builds documentation)
make opendoc (Opens documentation directory)
make clean (Cleans build files)
