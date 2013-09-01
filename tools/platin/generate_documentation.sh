#!/bin/bash
#
# Currently developer only (not installed)
#
if [ -z "${RUBY}" ] ; then RUBY=ruby ; fi
if [ -z "${RDOC}" ] ; then RDOC=rdoc ; fi
SRC_DIR=$(dirname "${0}")
OUTDIR="${SRC_DIR}"/rdoc
mkdir -p "${OUTDIR}"
"${RUBY}" "${SRC_DIR}/ext/generate_pml_doc.rb" "${SRC_DIR}/lib/core/pml.yml" > "${OUTDIR}"/pmldoc.rb
"${RDOC}" "${OUTDIR}"/pmldoc.rb -o "${OUTDIR}"/pml
"${RDOC}" "${SRC_DIR}"/lib -o "${OUTDIR}"/platin
