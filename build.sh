
#!/bin/sh

# temporary
git clean -dfX

# Bring up the tree and initialise boot/
./configure
make -j coldstart

# create utils/config.ml
cat utils/config.generated.ml utils/config.common.ml > utils/config_main.ml
cp utils/config_main.ml utils/config.ml

# create utils/domainstate.ml{,i}
gcc -E -P -I runtime/caml utils/domainstate.ml.c > utils/domainstate.ml
gcc -E -P -I runtime/caml utils/domainstate.mli.c > utils/domainstate.mli

# create parsing/parser.ml{,i}
sed "s/MenhirLib/CamlinternalMenhirLib/g" boot/menhir/parser.mli > parsing/parser.mli
sed "s/MenhirLib/CamlinternalMenhirLib/g" boot/menhir/parser.ml > parsing/parser.ml

# create parsing/camlinternalMenhirLib.ml{,i}
echo '[@@@ocaml.warning "-67"]' > parsing/camlinternalMenhirLib.mli && \
cat boot/menhir/menhirLib.mli >> parsing/camlinternalMenhirLib.mli
cp boot/menhir/menhirLib.ml parsing/camlinternalMenhirLib.ml

# ./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I utils -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c utils/domainstate.mli

# ./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I utils -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c utils/domainstate.ml

# domainstate
# make V=1 compilerlibs/ocamlcommon.cma
./boot/ocamlrun ./boot/ocamlc -nostdlib -linkall -a -use-prims runtime/primitives \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -I stdlib \
  utils/config.ml utils/build_path_prefix_map.ml utils/format_doc.ml utils/misc.ml utils/identifiable.ml utils/numbers.ml utils/arg_helper.ml utils/local_store.ml utils/load_path.ml utils/clflags.ml utils/profile.ml utils/terminfo.ml utils/ccomp.ml utils/warnings.ml utils/consistbl.ml utils/linkdeps.ml utils/strongly_connected_components.ml utils/targetint.ml utils/int_replace_polymorphic_compare.ml utils/domainstate.ml utils/binutils.ml utils/lazy_backtrack.ml utils/diffing.ml utils/diffing_with_keys.ml utils/compression.ml parsing/location.ml parsing/unit_info.ml parsing/asttypes.ml parsing/longident.ml parsing/docstrings.ml parsing/syntaxerr.ml parsing/ast_helper.ml parsing/ast_iterator.ml parsing/builtin_attributes.ml parsing/camlinternalMenhirLib.ml parsing/parser.ml parsing/lexer.ml parsing/pprintast.ml parsing/parse.ml parsing/printast.ml parsing/ast_mapper.ml parsing/attr_helper.ml parsing/ast_invariants.ml parsing/depend.ml typing/ident.ml typing/path.ml typing/primitive.ml typing/type_immediacy.ml typing/shape.ml typing/types.ml typing/data_types.ml typing/rawprinttyp.ml typing/gprinttyp.ml typing/btype.ml typing/oprint.ml typing/subst.ml typing/predef.ml typing/datarepr.ml file_formats/cmi_format.ml typing/persistent_env.ml typing/env.ml typing/errortrace.ml typing/typedtree.ml typing/signature_group.ml typing/printtyped.ml typing/ctype.ml typing/out_type.ml typing/printtyp.ml typing/errortrace_report.ml typing/includeclass.ml typing/mtype.ml typing/envaux.ml typing/includecore.ml typing/tast_iterator.ml typing/tast_mapper.ml typing/stypes.ml typing/shape_reduce.ml file_formats/cmt_format.ml typing/cmt2annot.ml typing/untypeast.ml typing/includemod.ml typing/includemod_errorprinter.ml typing/typetexp.ml typing/printpat.ml typing/patterns.ml typing/parmatch.ml typing/typedecl_properties.ml typing/typedecl_variance.ml typing/typedecl_unboxed.ml typing/typedecl_immediacy.ml typing/typedecl_separability.ml typing/typeopt.ml typing/typedecl.ml typing/value_rec_check.ml typing/typecore.ml typing/typeclass.ml typing/typemod.ml lambda/debuginfo.ml lambda/lambda.ml lambda/printlambda.ml lambda/switch.ml lambda/matching.ml lambda/value_rec_compiler.ml lambda/translobj.ml lambda/translattribute.ml lambda/translprim.ml lambda/translcore.ml lambda/translclass.ml lambda/translmod.ml lambda/tmc.ml lambda/simplif.ml lambda/runtimedef.ml bytecomp/meta.ml bytecomp/opcodes.ml bytecomp/bytesections.ml bytecomp/dll.ml bytecomp/symtable.ml driver/pparse.ml driver/compenv.ml driver/main_args.ml driver/compmisc.ml driver/makedepend.ml driver/compile_common.ml driver/handler_common.ml \
  -o compilerlibs/ocamlcommon.cma

# make V=1 compilerlibs/ocamlbytecomp.cma
./boot/ocamlrun ./boot/ocamlc -nostdlib -a -use-prims runtime/primitives \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events \
  bytecomp/instruct.ml bytecomp/bytegen.ml bytecomp/printinstr.ml bytecomp/emitcode.ml bytecomp/bytelink.ml bytecomp/bytelibrarian.ml bytecomp/bytepackager.ml driver/errors.ml driver/compile.ml driver/maindriver.ml \
  -o compilerlibs/ocamlbytecomp.cma

# make V=1 ocamlc
./boot/ocamlrun ./boot/ocamlc -nostdlib -use-prims runtime/primitives -compat-32 -g \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events \
  compilerlibs/ocamlbytecomp.cma driver/main.cmo \
  -o ocamlc compilerlibs/ocamlcommon.cma
