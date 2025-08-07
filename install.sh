#!/bin/sh

# This script builds and installs OCaml to ./install
# as an alternative to "make world && make install".

# We make use of the fact that the compiler is able to
# discover and compile missing dependencies on the fly,
# so there is no dependency order required (unlike in Makefile).

# I didn't modify the stdlib compilation (yet!) since it seems to have a lot of special build rules...

set -euo pipefail # "strict mode", i.e. quit asap if any errors arise
# set -x # echo commands to stdout

# Absolute path that contains this script
readonly BUILD_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly INSTALL_DIR="${BUILD_ROOT}/install"
readonly BOOT_DIR="${BUILD_ROOT}/boot"

# todo: re-enable ocamldoc, debugger, native compiler
readonly OCAML_BUILD_CONFIG="--prefix=${INSTALL_DIR} --disable-native-compiler --disable-ocamldoc --disable-debugger"

# compiler args that are common to every invocation (?)
readonly INCLUDE="-I boot -I utils -I parsing -I typing -I bytecomp -I file_formats -I lambda -I middle_end -I middle_end/closure -I middle_end/flambda -I middle_end/flambda/base_types -I asmcomp -I driver -I toplevel -I tools -I runtime -I otherlibs/dynlink -I otherlibs/str -I otherlibs/systhreads -I otherlibs/unix -I otherlibs/runtime_events"

readonly OCAMLC_FLAGS="-nostdlib -use-prims runtime/primitives -g -strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48 -warn-error -a -bin-annot -strict-formats"
# warn-error +a

# Logging configuration
readonly LOG_FILE="${INSTALL_DIR}/install.log"
readonly VERBOSE=${VERBOSE:-0}

addprefix() {
  local prefix=$1; shift
  for f in "$@"; do
    printf "%s%s " "$prefix" "$f"
  done
}

# We don't need to include .mli files, since the compiler will attempt
# to find it in the load path and my handler will then catch an effect
# and compile the file (if it hasn't already been compiled)

# todo: this seems to be listed in link order, can we make this independent?
# todo2: do we want to list .cmo instead of .ml?

readonly utils_SOURCES=$(addprefix utils/ \
  config.ml build_path_prefix_map.ml format_doc.ml misc.ml identifiable.ml \
  numbers.ml arg_helper.ml local_store.ml load_path.ml clflags.ml profile.ml \
  terminfo.ml ccomp.ml warnings.ml consistbl.ml linkdeps.ml \
  strongly_connected_components.ml targetint.ml \
  int_replace_polymorphic_compare.ml domainstate.ml binutils.ml \
  lazy_backtrack.ml diffing.ml diffing_with_keys.ml compression.ml)

readonly parsing_SOURCES=$(addprefix parsing/ \
  location.ml unit_info.ml asttypes.ml longident.ml docstrings.ml syntaxerr.ml \
  ast_helper.ml ast_iterator.ml builtin_attributes.ml camlinternalMenhirLib.ml \
  parser.ml lexer.ml pprintast.ml parse.ml printast.ml ast_mapper.ml \
  attr_helper.ml ast_invariants.ml depend.ml)

readonly typing_SOURCES="\
  typing/ident.ml typing/path.ml typing/primitive.ml typing/type_immediacy.ml \
  typing/shape.ml typing/types.ml typing/data_types.ml typing/rawprinttyp.ml \
  typing/gprinttyp.ml typing/btype.ml typing/oprint.ml typing/subst.ml \
  typing/predef.ml typing/datarepr.ml file_formats/cmi_format.ml \
  typing/persistent_env.ml typing/env.ml typing/errortrace.ml \
  typing/typedtree.ml typing/signature_group.ml typing/printtyped.ml \
  typing/ctype.ml typing/out_type.ml typing/printtyp.ml \
  typing/errortrace_report.ml typing/includeclass.ml typing/mtype.ml \
  typing/envaux.ml typing/includecore.ml typing/tast_iterator.ml \
  typing/tast_mapper.ml typing/stypes.ml typing/shape_reduce.ml \
  file_formats/cmt_format.ml typing/cmt2annot.ml typing/untypeast.ml \
  typing/includemod.ml typing/includemod_errorprinter.ml typing/typetexp.ml \
  typing/printpat.ml typing/patterns.ml typing/parmatch.ml \
  typing/typedecl_properties.ml typing/typedecl_variance.ml \
  typing/typedecl_unboxed.ml typing/typedecl_immediacy.ml \
  typing/typedecl_separability.ml typing/typeopt.ml typing/typedecl.ml \
  typing/value_rec_check.ml typing/typecore.ml typing/typeclass.ml \
  typing/typemod.ml"

readonly lambda_SOURCES=$(addprefix lambda/ \
  debuginfo.ml lambda.ml printlambda.ml switch.ml matching.ml \
  value_rec_compiler.ml translobj.ml translattribute.ml translprim.ml \
  translcore.ml translclass.ml translmod.ml tmc.ml simplif.ml runtimedef.ml)

readonly comp_SOURCES="\
  bytecomp/meta.ml bytecomp/opcodes.ml bytecomp/bytesections.ml \
  bytecomp/dll.ml bytecomp/symtable.ml driver/pparse.ml \
  driver/handler_common.ml driver/compenv.ml driver/main_args.ml \
  driver/compmisc.ml driver/makedepend.ml driver/compile_common.ml"

readonly ocamlmiddleend_SOURCES="\
  middle_end/internal_variable_names.ml middle_end/linkage_name.ml \
  middle_end/compilation_unit.ml middle_end/variable.ml \
  middle_end/flambda/base_types/closure_element.ml \
  middle_end/flambda/base_types/closure_id.ml middle_end/symbol.ml \
  middle_end/backend_var.ml middle_end/clambda_primitives.ml \
  middle_end/printclambda_primitives.ml middle_end/clambda.ml \
  middle_end/printclambda.ml middle_end/semantics_of_primitives.ml \
  middle_end/convert_primitives.ml middle_end/flambda/base_types/id_types.ml \
  middle_end/flambda/base_types/export_id.ml \
  middle_end/flambda/base_types/tag.ml \
  middle_end/flambda/base_types/mutable_variable.ml \
  middle_end/flambda/base_types/set_of_closures_id.ml \
  middle_end/flambda/base_types/set_of_closures_origin.ml \
  middle_end/flambda/base_types/closure_origin.ml \
  middle_end/flambda/base_types/var_within_closure.ml \
  middle_end/flambda/base_types/static_exception.ml \
  middle_end/flambda/pass_wrapper.ml middle_end/flambda/allocated_const.ml \
  middle_end/flambda/parameter.ml middle_end/flambda/projection.ml \
  middle_end/flambda/flambda.ml middle_end/flambda/flambda_iterators.ml \
  middle_end/flambda/flambda_utils.ml middle_end/flambda/freshening.ml \
  middle_end/flambda/effect_analysis.ml middle_end/flambda/inlining_cost.ml \
  middle_end/flambda/simple_value_approx.ml \
  middle_end/flambda/export_info.ml \
  middle_end/flambda/export_info_for_pack.ml middle_end/compilenv.ml \
  middle_end/closure/closure.ml middle_end/closure/closure_middle_end.ml \
  middle_end/flambda/import_approx.ml middle_end/flambda/lift_code.ml \
  middle_end/flambda/closure_conversion_aux.ml \
  middle_end/flambda/closure_conversion.ml \
  middle_end/flambda/initialize_symbol_to_let_symbol.ml \
  middle_end/flambda/lift_let_to_initialize_symbol.ml \
  middle_end/flambda/find_recursive_functions.ml \
  middle_end/flambda/invariant_params.ml \
  middle_end/flambda/inconstant_idents.ml \
  middle_end/flambda/alias_analysis.ml middle_end/flambda/lift_constants.ml \
  middle_end/flambda/share_constants.ml \
  middle_end/flambda/simplify_common.ml \
  middle_end/flambda/remove_unused_arguments.ml \
  middle_end/flambda/remove_unused_closure_vars.ml \
  middle_end/flambda/remove_unused_program_constructs.ml \
  middle_end/flambda/simplify_boxed_integer_ops.ml \
  middle_end/flambda/simplify_primitives.ml \
  middle_end/flambda/inlining_stats_types.ml \
  middle_end/flambda/inlining_stats.ml \
  middle_end/flambda/inline_and_simplify_aux.ml \
  middle_end/flambda/remove_free_vars_equal_to_args.ml \
  middle_end/flambda/extract_projections.ml \
  middle_end/flambda/augment_specialised_args.ml \
  middle_end/flambda/unbox_free_vars_of_closures.ml \
  middle_end/flambda/unbox_specialised_args.ml \
  middle_end/flambda/unbox_closures.ml \
  middle_end/flambda/inlining_transforms.ml \
  middle_end/flambda/inlining_decision.ml \
  middle_end/flambda/inline_and_simplify.ml \
  middle_end/flambda/ref_to_variables.ml \
  middle_end/flambda/flambda_invariants.ml \
  middle_end/flambda/traverse_for_exported_symbols.ml \
  middle_end/flambda/build_export_info.ml \
  middle_end/flambda/closure_offsets.ml \
  middle_end/flambda/un_anf.ml middle_end/flambda/flambda_to_clambda.ml \
  middle_end/flambda/flambda_middle_end.ml"

readonly PERVASIVES="\
  camlinternalFormatBasics stdlib stdlib__Either stdlib__Sys stdlib__Obj stdlib__Type stdlib__Atomic camlinternalLazy stdlib__Lazy stdlib__Seq stdlib__Option stdlib__Pair stdlib__Result stdlib__Bool stdlib__Char stdlib__Uchar stdlib__List stdlib__Int stdlib__Array stdlib__Iarray stdlib__Bytes stdlib__String stdlib__Unit stdlib__Marshal stdlib__Float stdlib__Int32 stdlib__Int64 stdlib__Nativeint stdlib__Lexing stdlib__Parsing stdlib__Repr stdlib__Set stdlib__Map stdlib__Stack stdlib__Queue stdlib__Buffer stdlib__Mutex stdlib__Condition stdlib__Semaphore stdlib__Domain camlinternalFormat stdlib__Printf stdlib__Arg stdlib__Printexc stdlib__Fun stdlib__Gc stdlib__In_channel stdlib__Out_channel stdlib__Digest stdlib__Bigarray stdlib__Random stdlib__Hashtbl stdlib__Weak stdlib__Format stdlib__Scanf stdlib__Callback camlinternalOO stdlib__Oo camlinternalMod stdlib__Dynarray stdlib__Pqueue stdlib__Ephemeron stdlib__Filename stdlib__Complex stdlib__ArrayLabels stdlib__ListLabels stdlib__BytesLabels stdlib__StringLabels stdlib__MoreLabels stdlib__StdLabels stdlib__Effect"

log()
{
  local level="$1"
  shift
  [ -d "$INSTALL_DIR" ] || mkdir "$INSTALL_DIR"
  [ -n "${LOG_FILE:-}" ] || touch "$LOG_FILE"
  echo "[$(date '+%H:%M:%S')] [$level] $@" | tee -a "$LOG_FILE"
}

info() { log "INFO" "$@"; }
warn() { log "WARN" "$@"; }
error() { log "ERROR" "$@"; }

# Remove untracked files & dirs that are ignored by .gitignore
clean_workspace()
{
  info "Cleaning workspace..."
  git clean -dfX >> /dev/null
}

# Suppress outputs (stdout, stderr) and redirect to logfile
redirect_to_logfile() {
  $@ >> "$LOG_FILE" 2>&1
}

# Invoke the OCaml compiler
parallel_ocamlc()
{
  redirect_to_logfile "./custom-ocamlc" "$@"
}

boot_ocamlc()
{
  redirect_to_logfile "boot/ocamlrun" "boot/ocamlc" "$@"
}

# Invoke the OCaml compiler
ocamlrun()
{
  redirect_to_logfile "boot/ocamlrun" "$@"
}

# Initialize the core part of the build system
init_build()
{
  export CAML_LD_LIBRARY_PATH=
  export OCAMLRUNPARAM="b"

  # Check that boot/ocamlc exists
  if [ ! -x "${BOOT_DIR}/ocamlc" ]; then
    error "Missing distribution compiler! (boot/ocamlc)"
    exit 1
  fi

  # Set up log files
  mkdir "$INSTALL_DIR"
  cat /dev/null > "$LOG_FILE"

  # Build custom compiler
  redirect_to_logfile \
    ../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma \
    custom_ocamlc/custom_load_path.mli custom_ocamlc/custom_load_path.ml \
    custom_ocamlc/effect_handler.mli custom_ocamlc/effect_handler.ml \
    custom_ocamlc/parallelism_helper.mli custom_ocamlc/parallelism_helper.ml \
    custom_ocamlc/parallelism.mli custom_ocamlc/parallelism.ml \
    custom_ocamlc/custom_maindriver.mli custom_ocamlc/custom_maindriver.ml \
    custom_ocamlc/main.ml \
    -I custom_ocamlc -I +compiler-libs -g -o custom-ocamlc

  # Configure the OCaml build system (to use our install dir, for example)
  redirect_to_logfile ./configure $OCAML_BUILD_CONFIG

  # Start up the system from the distribution compiler
  # This produces, most importantly, boot/ocamlrun, the bytecode interpreter
  info "make -j coldstart"
  redirect_to_logfile make -j coldstart

  # Also build the lexer (Do we want to remove the dependency on Make here?)
  info "make -j ocamllex"
  redirect_to_logfile make -j ocamllex

  # This generates some .ml{,i} files that are required for compilation later?
  info "make -j beforedepend"
  redirect_to_logfile make -j beforedepend
}

build_ocamlc()
{
  local ocamlcommon_SOURCES="$utils_SOURCES $parsing_SOURCES $typing_SOURCES $lambda_SOURCES $comp_SOURCES"

  local ocamlbytecomp_SOURCES="\
    bytecomp/instruct.ml bytecomp/bytegen.ml bytecomp/printinstr.ml \
    bytecomp/emitcode.ml bytecomp/bytelink.ml bytecomp/bytelibrarian.ml \
    bytecomp/bytepackager.ml driver/errors.ml driver/compile.ml \
    driver/maindriver.ml"

  info "Building compilerlibs/ocamlcommon.cma... (may take a while)"
  parallel_ocamlc \
    $OCAMLC_FLAGS -a -linkall $INCLUDE -I stdlib \
    $ocamlcommon_SOURCES \
    -o compilerlibs/ocamlcommon.cma

  info "Building compilerlibs/ocamlbytecomp.cma..."
  parallel_ocamlc \
    $OCAMLC_FLAGS -a $INCLUDE \
    $ocamlbytecomp_SOURCES \
    -o compilerlibs/ocamlbytecomp.cma

  info "Building driver/main.cmo..."
  parallel_ocamlc \
    $OCAMLC_FLAGS -I stdlib -I driver \
    -c driver/main.ml

  info "Building ocamlc..."
  boot_ocamlc \
    $OCAMLC_FLAGS -compat-32 $INCLUDE \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma driver/main.cmo \
    -o ocamlc
}

build_toplevel()
{
  # todo: move somewhere else / eliminate?
  info "Building other toplevel .ml sources..."
  parallel_ocamlc \
    $OCAMLC_FLAGS $INCLUDE -I toplevel/byte \
    -c utils/config_main.ml utils/config_boot.ml toplevel/expunge.ml toplevel/topstart.ml

  cp toplevel/topmain.cmi toplevel/topmain.mli toplevel/byte

  info "Building ocamltoplevel.cma..."
  boot_ocamlc \
    $OCAMLC_FLAGS -linkall -a $INCLUDE -I toplevel/byte \
    toplevel/genprintval.ml toplevel/topcommon.ml toplevel/byte/topeval.ml toplevel/byte/trace.ml toplevel/toploop.ml toplevel/topprinters.ml toplevel/topdirs.ml toplevel/byte/topmain.ml \
    -o compilerlibs/ocamltoplevel.cma

  info "Building expunge..."
  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma toplevel/expunge.cmo \
    -o expunge

  info "Building ocaml.tmp..."
  boot_ocamlc \
    $OCAMLC_FLAGS -linkall $INCLUDE -I toplevel/byte \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma compilerlibs/ocamltoplevel.cma toplevel/topstart.cmo \
    -o ocaml.tmp

  info "Building ocaml..."
  ocamlrun expunge ocaml.tmp ocaml $PERVASIVES outcometree topprinters topdirs toploop

  rm -f ocaml.tmp

  boot_ocamlc $OCAMLC_FLAGS $INCLUDE -c toplevel/topeval.mli

  cp toplevel/topeval.cmi toplevel/topeval.mli toplevel/byte
  # cp toplevel/byte/trace.cmti toplevel/trace.cmti
}

build_otherlibs()
{
  # todo: figure out what this does
  redirect_to_logfile make -j ocamlmklib

  for subdir in runtime_events unix str systhreads; do
    cd otherlibs/$subdir
    info "Building otherlibs/$subdir..."
    redirect_to_logfile make -j all
    cd ../..
  done

  boot_ocamlc $OCAMLC_FLAGS $INCLUDE -I otherlibs/dynlink/byte -c otherlibs/dynlink/dynlink.mli

  cp otherlibs/dynlink/dynlink.cmi otherlibs/dynlink/dynlink.mli otherlibs/dynlink/byte/

  parallel_ocamlc $OCAMLC_FLAGS -a $INCLUDE -I otherlibs/dynlink/byte \
    otherlibs/dynlink/dynlink_config.ml otherlibs/dynlink/dynlink_types.ml otherlibs/dynlink/dynlink_platform_intf.ml otherlibs/dynlink/dynlink_common.ml otherlibs/dynlink/byte/dynlink_symtable.ml otherlibs/dynlink/byte/dynlink.ml \
    -o otherlibs/dynlink/dynlink.cma
}

build_middle_end()
{
  info "Building compilerlibs/ocamlmiddleend.cma..."
  parallel_ocamlc $OCAMLC_FLAGS $INCLUDE -a $ocamlmiddleend_SOURCES \
    -o compilerlibs/ocamlmiddleend.cma
}

# Build various tools that come with the OCaml installation
build_tools()
{
  info "Building ocamldep..."
  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE -compat-32 \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma tools/ocamldep.ml \
    -o tools/ocamldep

  info "Building stripdebug..."
  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma tools/stripdebug.ml \
    -o tools/stripdebug

  info "Building ocamlcmt..."
  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma tools/ocamlcmt.ml \
    -o tools/ocamlcmt

  info "Building ocamlobjinfo..."
  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma compilerlibs/ocamlmiddleend.cma tools/objinfo.ml \
    -o tools/ocamlobjinfo

  info "Building ocamlcp..."
  parallel_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    -c build_path_prefix_map.ml format_doc.ml misc.ml profile.ml warnings.ml identifiable.ml numbers.ml arg_helper.ml local_store.ml load_path.ml clflags.ml terminfo.ml location.ml ccomp.ml compenv.ml main_args.ml ocamlcp_common.ml ocamlcp.ml

  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo profile.cmo warnings.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo terminfo.cmo location.cmo ccomp.cmo compenv.cmo main_args.cmo ocamlcp_common.cmo ocamlcp.cmo \
    -o tools/ocamlcp

  info "Building ocamlprof..."
  parallel_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    -c build_path_prefix_map.ml format_doc.ml misc.ml identifiable.ml numbers.ml arg_helper.ml local_store.ml load_path.ml clflags.ml terminfo.ml warnings.ml location.ml longident.ml docstrings.ml syntaxerr.ml ast_helper.ml ast_iterator.ml builtin_attributes.ml camlinternalMenhirLib.ml parser.ml lexer.ml pprintast.ml parse.ml ocamlprof.ml \

  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo terminfo.cmo warnings.cmo location.cmo longident.cmo docstrings.cmo syntaxerr.cmo ast_helper.cmo ast_iterator.cmo builtin_attributes.cmo camlinternalMenhirLib.cmo parser.cmo lexer.cmo pprintast.cmo parse.cmo ocamlprof.cmo \
    -o tools/ocamlprof

  info "Building ocamlmklib..."
  parallel_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    -c build_path_prefix_map.ml format_doc.ml misc.ml ocamlmklib.ml

  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo ocamlmklib.cmo \
    -o tools/ocamlmklib

  info "Building ocamlmktop..."
  parallel_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    -c build_path_prefix_map.ml format_doc.ml misc.ml identifiable.ml numbers.ml arg_helper.ml local_store.ml load_path.ml clflags.ml profile.ml ccomp.ml ocamlmktop.ml

  boot_ocamlc \
    $OCAMLC_FLAGS $INCLUDE \
    config.cmo build_path_prefix_map.cmo format_doc.cmo misc.cmo identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo profile.cmo ccomp.cmo ocamlmktop.cmo \
    -o tools/ocamlmktop

  boot_ocamlc $OCAMLC_FLAGS $INCLUDE -c tools/profiling.ml
}

main()
{
  clean_workspace

  # Build the compiler and other targets
  init_build
  build_ocamlc
  build_middle_end
  build_toplevel
  build_otherlibs
  build_tools

  # make various other targets; these don't invoke the compiler (i think)
  redirect_to_logfile make -j ld.conf ocamlrund ocamlruni libcamlrun_pic.a libcamlrun_shared.so

  # install to $PWD/install
  info "Installing to $INSTALL_DIR..."
  redirect_to_logfile make install
}

main "$@"

