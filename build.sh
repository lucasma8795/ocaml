
#!/bin/sh

# temporary
git clean -dfX

# Empty install directory
rm -rf $PWD/install
mkdir install

# Bring up the tree and initialise boot/
./configure --prefix $PWD/install --disable-native-compiler --disable-ocamldoc --disable-debugger
make -j coldstart

# Manually create various .ml{,i} files that would otherwise be done by Makefile dependencies
# make V=1 utils/config.ml
# make V=1 utils/config_main.mli
# make V=1 utils/config_boot.ml utils/config_boot.mli
# make V=1 utils/domainstate.mli utils/domainstate.ml
# make V=1 parsing/parser.mli parsing/parser.ml
# make V=1 parsing/lexer.ml
# make V=1 parsing/camlinternalMenhirLib.mli parsing/camlinternalMenhirLib.ml
# make V=1 toplevel/byte/topeval.mli
# make V=1 bytecomp/opcodes.ml bytecomp/opcodes.mli
# make V=1 lambda/runtimedef.ml

make V=1 ocamllex
make V=1 beforedepend

# make V=1 compilerlibs/ocamlcommon.cma
./boot/ocamlrun ./boot/ocamlc \
  -nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -linkall \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -I stdlib \
  utils/config.ml utils/build_path_prefix_map.ml utils/format_doc.ml utils/misc.ml utils/identifiable.ml utils/numbers.ml utils/arg_helper.ml utils/local_store.ml utils/load_path.ml utils/clflags.ml utils/profile.ml utils/terminfo.ml utils/ccomp.ml utils/warnings.ml utils/consistbl.ml utils/linkdeps.ml utils/strongly_connected_components.ml utils/targetint.ml utils/int_replace_polymorphic_compare.ml utils/domainstate.ml utils/binutils.ml utils/lazy_backtrack.ml utils/diffing.ml utils/diffing_with_keys.ml utils/compression.ml parsing/location.ml parsing/unit_info.ml parsing/asttypes.ml parsing/longident.ml parsing/docstrings.ml parsing/syntaxerr.ml parsing/ast_helper.ml parsing/ast_iterator.ml parsing/builtin_attributes.ml parsing/camlinternalMenhirLib.ml parsing/parser.ml parsing/lexer.ml parsing/pprintast.ml parsing/parse.ml parsing/printast.ml parsing/ast_mapper.ml parsing/attr_helper.ml parsing/ast_invariants.ml parsing/depend.ml typing/ident.ml typing/path.ml typing/primitive.ml typing/type_immediacy.ml typing/shape.ml typing/types.ml typing/data_types.ml typing/rawprinttyp.ml typing/gprinttyp.ml typing/btype.ml typing/oprint.ml typing/subst.ml typing/predef.ml typing/datarepr.ml file_formats/cmi_format.ml typing/persistent_env.ml typing/env.ml typing/errortrace.ml typing/typedtree.ml typing/signature_group.ml typing/printtyped.ml typing/ctype.ml typing/out_type.ml typing/printtyp.ml typing/errortrace_report.ml typing/includeclass.ml typing/mtype.ml typing/envaux.ml typing/includecore.ml typing/tast_iterator.ml typing/tast_mapper.ml typing/stypes.ml typing/shape_reduce.ml file_formats/cmt_format.ml typing/cmt2annot.ml typing/untypeast.ml typing/includemod.ml typing/includemod_errorprinter.ml typing/typetexp.ml typing/printpat.ml typing/patterns.ml typing/parmatch.ml typing/typedecl_properties.ml typing/typedecl_variance.ml typing/typedecl_unboxed.ml typing/typedecl_immediacy.ml typing/typedecl_separability.ml typing/typeopt.ml typing/typedecl.ml typing/value_rec_check.ml typing/typecore.ml typing/typeclass.ml typing/typemod.ml lambda/debuginfo.ml lambda/lambda.ml lambda/printlambda.ml lambda/switch.ml lambda/matching.ml lambda/value_rec_compiler.ml lambda/translobj.ml lambda/translattribute.ml lambda/translprim.ml lambda/translcore.ml lambda/translclass.ml lambda/translmod.ml lambda/tmc.ml lambda/simplif.ml lambda/runtimedef.ml bytecomp/meta.ml bytecomp/opcodes.ml bytecomp/bytesections.ml bytecomp/dll.ml bytecomp/symtable.ml driver/pparse.ml driver/compenv.ml driver/main_args.ml driver/compmisc.ml driver/makedepend.ml driver/compile_common.ml driver/handler_common.ml \
  -a -o compilerlibs/ocamlcommon.cma

# make V=1 compilerlibs/ocamlbytecomp.cma
./boot/ocamlrun ./boot/ocamlc \
  -nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events \
  bytecomp/instruct.ml bytecomp/bytegen.ml bytecomp/printinstr.ml bytecomp/emitcode.ml bytecomp/bytelink.ml bytecomp/bytelibrarian.ml bytecomp/bytepackager.ml driver/errors.ml driver/compile.ml driver/maindriver.ml \
  -a -o compilerlibs/ocamlbytecomp.cma

# make V=1 driver/main.cmo driver/optmain.cmo
./boot/ocamlrun ./boot/ocamlc \
  -nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats \
  -c driver/main.ml -I stdlib -I driver
# ./boot/ocamlrun ./boot/ocamlc -c driver/main.ml driver/optmain.ml -I stdlib -I driver

# make V=1 ocamlc
./boot/ocamlrun ./boot/ocamlc \
  -nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -compat-32 \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events \
  compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma driver/main.cmo \
  -o ocamlc

# make V=1 ocaml
./boot/ocamlrun ./boot/ocamlc \
  -nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats \
  -I ./boot -I toplevel -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -I toplevel/byte \
  -c utils/config_main.ml utils/config_boot.ml toplevel/expunge.ml toplevel/genprintval.ml toplevel/topcommon.ml toplevel/byte/topeval.ml toplevel/byte/trace.ml toplevel/toploop.ml toplevel/topprinters.ml toplevel/topdirs.ml toplevel/byte/topmain.ml toplevel/topstart.ml

make V=1 ocamlmklib
cd otherlibs/runtime_events
make V=1 all
cd ../..

cd otherlibs/unix
make V=1 all
cd ../..

cd otherlibs/str
make V=1 all
cd ../..

cd otherlibs/systhreads
make V=1 all
cd ../..

# ./boot/ocamlrun ./ocamlc -nostdlib -I ./stdlib -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -pack -c debugger/ocamldebug.ml debugger/int64ops.ml debugger/primitives.ml debugger/unix_tools.ml debugger/debugger_config.ml debugger/parameters.ml debugger/debugger_lexer.ml debugger/input_handling.ml debugger/question.ml debugger/debugcom.ml debugger/exec.ml debugger/source.ml debugger/pos.ml debugger/checkpoints.ml debugger/events.ml debugger/program_loading.ml debugger/symbols.ml debugger/breakpoints.ml debugger/trap_barrier.ml debugger/history.ml debugger/printval.ml debugger/show_source.ml debugger/time_travel.ml debugger/program_management.ml debugger/frames.ml debugger/eval.ml debugger/show_information.ml debugger/loadprinter.ml debugger/debugger_parser.ml debugger/command_line.ml debugger/main.ml

# ./boot/ocamlrun ./ocamlc -nostdlib -I ./stdlib -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -pack -o debugger/ocamldebug.cmo debugger/int64ops.cmo debugger/primitives.cmo debugger/unix_tools.cmo debugger/debugger_config.cmo debugger/parameters.cmo debugger/debugger_lexer.cmo debugger/input_handling.cmo debugger/question.cmo debugger/debugcom.cmo debugger/exec.cmo debugger/source.cmo debugger/pos.cmo debugger/checkpoints.cmo debugger/events.cmo debugger/program_loading.cmo debugger/symbols.cmo debugger/breakpoints.cmo debugger/trap_barrier.cmo debugger/history.cmo debugger/printval.cmo debugger/show_source.cmo debugger/time_travel.cmo debugger/program_management.cmo debugger/frames.cmo debugger/eval.cmo debugger/show_information.cmo debugger/loadprinter.cmo debugger/debugger_parser.cmo debugger/command_line.cmo debugger/main.cmo

./boot/ocamlrun ./boot/ocamlc \
  -linkall -g -a -nostdlib -use-prims runtime/primitives \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -I toplevel/byte -I toplevel/byte   toplevel/genprintval.cmo toplevel/topcommon.cmo toplevel/byte/topeval.cmo toplevel/byte/trace.cmo toplevel/toploop.cmo toplevel/topprinters.cmo toplevel/topdirs.cmo toplevel/byte/topmain.cmo \
  -o compilerlibs/ocamltoplevel.cma

./boot/ocamlrun ./boot/ocamlc \
  -linkall -g -nostdlib -use-prims runtime/primitives \
  -I ./boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -I toplevel/byte \
  compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma compilerlibs/ocamltoplevel.cma toplevel/topstart.cmo \
  -o ocaml.tmp

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -o expunge compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma toplevel/expunge.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/stripdebug.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/stripdebug.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -o tools/stripdebug compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma tools/stripdebug.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcmt.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcmt.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -o tools/ocamlcmt compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma tools/ocamlcmt.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamldep.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamldep.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -compat-32   -o tools/ocamldep compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma  tools/ocamldep.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events  -a -o compilerlibs/ocamlmiddleend.cma  middle_end/internal_variable_names.cmo middle_end/linkage_name.cmo middle_end/compilation_unit.cmo middle_end/variable.cmo middle_end/flambda/base_types/closure_element.cmo middle_end/flambda/base_types/closure_id.cmo middle_end/symbol.cmo middle_end/backend_var.cmo middle_end/clambda_primitives.cmo middle_end/printclambda_primitives.cmo middle_end/clambda.cmo middle_end/printclambda.cmo middle_end/semantics_of_primitives.cmo middle_end/convert_primitives.cmo middle_end/flambda/base_types/id_types.cmo middle_end/flambda/base_types/export_id.cmo middle_end/flambda/base_types/tag.cmo middle_end/flambda/base_types/mutable_variable.cmo middle_end/flambda/base_types/set_of_closures_id.cmo middle_end/flambda/base_types/set_of_closures_origin.cmo middle_end/flambda/base_types/closure_origin.cmo middle_end/flambda/base_types/var_within_closure.cmo middle_end/flambda/base_types/static_exception.cmo middle_end/flambda/pass_wrapper.cmo middle_end/flambda/allocated_const.cmo middle_end/flambda/parameter.cmo middle_end/flambda/projection.cmo middle_end/flambda/flambda.cmo middle_end/flambda/flambda_iterators.cmo middle_end/flambda/flambda_utils.cmo middle_end/flambda/freshening.cmo middle_end/flambda/effect_analysis.cmo middle_end/flambda/inlining_cost.cmo middle_end/flambda/simple_value_approx.cmo middle_end/flambda/export_info.cmo middle_end/flambda/export_info_for_pack.cmo middle_end/compilenv.cmo middle_end/closure/closure.cmo middle_end/closure/closure_middle_end.cmo middle_end/flambda/import_approx.cmo middle_end/flambda/lift_code.cmo middle_end/flambda/closure_conversion_aux.cmo middle_end/flambda/closure_conversion.cmo middle_end/flambda/initialize_symbol_to_let_symbol.cmo middle_end/flambda/lift_let_to_initialize_symbol.cmo middle_end/flambda/find_recursive_functions.cmo middle_end/flambda/invariant_params.cmo middle_end/flambda/inconstant_idents.cmo middle_end/flambda/alias_analysis.cmo middle_end/flambda/lift_constants.cmo middle_end/flambda/share_constants.cmo middle_end/flambda/simplify_common.cmo middle_end/flambda/remove_unused_arguments.cmo middle_end/flambda/remove_unused_closure_vars.cmo middle_end/flambda/remove_unused_program_constructs.cmo middle_end/flambda/simplify_boxed_integer_ops.cmo middle_end/flambda/simplify_primitives.cmo middle_end/flambda/inlining_stats_types.cmo middle_end/flambda/inlining_stats.cmo middle_end/flambda/inline_and_simplify_aux.cmo middle_end/flambda/remove_free_vars_equal_to_args.cmo middle_end/flambda/extract_projections.cmo middle_end/flambda/augment_specialised_args.cmo middle_end/flambda/unbox_free_vars_of_closures.cmo middle_end/flambda/unbox_specialised_args.cmo middle_end/flambda/unbox_closures.cmo middle_end/flambda/inlining_transforms.cmo middle_end/flambda/inlining_decision.cmo middle_end/flambda/inline_and_simplify.cmo middle_end/flambda/ref_to_variables.cmo middle_end/flambda/flambda_invariants.cmo middle_end/flambda/traverse_for_exported_symbols.cmo middle_end/flambda/build_export_info.cmo middle_end/flambda/closure_offsets.cmo middle_end/flambda/un_anf.cmo middle_end/flambda/flambda_to_clambda.cmo middle_end/flambda/flambda_middle_end.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/objinfo.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/objinfo.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events    -o tools/ocamlobjinfo compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma compilerlibs/ocamlmiddleend.cma  tools/objinfo.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlprof.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlprof.ml

# ./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcp_common.mli
# ./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcp_common.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcp.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlcp.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events    -o tools/ocamlcp   config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo profile.cmo warnings.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo terminfo.cmo location.cmo ccomp.cmo compenv.cmo main_args.cmo ocamlcp_common.cmo ocamlcp.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/profiling.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/profiling.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlmklib.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlmklib.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events    -o tools/ocamlmklib   config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo ocamlmklib.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlmktop.mli

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error +a -bin-annot -strict-formats -I tools -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events -c tools/ocamlmktop.ml

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events    -o tools/ocamlmktop   config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo profile.cmo ccomp.cmo ocamlmktop.cmo

./boot/ocamlrun ./boot/ocamlc -nostdlib -I ./boot -use-prims runtime/primitives -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events    -o tools/ocamlprof   config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo terminfo.cmo warnings.cmo location.cmo longident.cmo docstrings.cmo syntaxerr.cmo ast_helper.cmo ast_iterator.cmo builtin_attributes.cmo camlinternalMenhirLib.cmo parser.cmo lexer.cmo pprintast.cmo parse.cmo ocamlprof.cmo

make V=1 otherlibs/dynlink/dynlink.cma

./boot/ocamlrun expunge ocaml.tmp ocaml camlinternalFormatBasics stdlib stdlib__Either stdlib__Sys stdlib__Obj stdlib__Type stdlib__Atomic camlinternalLazy stdlib__Lazy stdlib__Seq stdlib__Option stdlib__Pair stdlib__Result stdlib__Bool stdlib__Char stdlib__Uchar stdlib__List stdlib__Int stdlib__Array stdlib__Iarray stdlib__Bytes stdlib__String stdlib__Unit stdlib__Marshal stdlib__Float stdlib__Int32 stdlib__Int64 stdlib__Nativeint stdlib__Lexing stdlib__Parsing stdlib__Repr stdlib__Set stdlib__Map stdlib__Stack stdlib__Queue stdlib__Buffer stdlib__Mutex stdlib__Condition stdlib__Semaphore stdlib__Domain camlinternalFormat stdlib__Printf stdlib__Arg stdlib__Printexc stdlib__Fun stdlib__Gc stdlib__In_channel stdlib__Out_channel stdlib__Digest stdlib__Bigarray stdlib__Random stdlib__Hashtbl stdlib__Weak stdlib__Format stdlib__Scanf stdlib__Callback camlinternalOO stdlib__Oo camlinternalMod stdlib__Dynarray stdlib__Pqueue stdlib__Ephemeron stdlib__Filename stdlib__Complex stdlib__ArrayLabels stdlib__ListLabels stdlib__BytesLabels stdlib__StringLabels stdlib__MoreLabels stdlib__StdLabels stdlib__Effect outcometree topprinters topdirs toploop

rm -f ocaml.tmp

make V=1 toplevel/byte/topeval.cmi toplevel/byte/topmain.cmi toplevel/byte/trace.cmi

# make various other targets
make ld.conf ocamlrund ocamlruni libcamlrun_pic.a libcamlrun_shared.so

# install to $PWD/install
make install
