#!/bin/sh

set -euo pipefail

rm -rf foo/*.cm{i,o,ti}

../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma \
  custom_ocamlc/custom_misc.mli custom_ocamlc/custom_misc.ml \
  custom_ocamlc/custom_load_path.mli custom_ocamlc/custom_load_path.ml \
  custom_ocamlc/effect_handler.mli custom_ocamlc/effect_handler.ml \
  custom_ocamlc/parallelism_helper.mli custom_ocamlc/parallelism_helper.ml \
  custom_ocamlc/parallelism.mli custom_ocamlc/parallelism.ml \
  custom_ocamlc/custom_maindriver.mli custom_ocamlc/custom_maindriver.ml \
  custom_ocamlc/main.ml \
  -I custom_ocamlc -I +compiler-libs -g -o custom-ocamlc

OCAMLRUNPARAM=b ./custom-ocamlc -g -c foo/A.ml foo/B.ml foo/C.ml foo/D.ml -I stdlib -I foo &> out

../v1/bin/ocamlc foo/D.cmo foo/B.cmo foo/C.cmo foo/A.cmo -o foo/prog

../v1/bin/ocamlrun foo/prog
