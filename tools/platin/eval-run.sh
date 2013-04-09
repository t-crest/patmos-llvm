#!/bin/bash
# Customizable Variables
###############################################################################
: ${BENCH:=sweet}                   # benchmark script, one out of {trace,sweet}
: ${SETS:=basic}                 # IFS-separated list of benchmark sets {basic,mrtc_ext}
: ${OPTS:=0 1 2}                    # IFS-separated list of optimization levels
: ${CLANG:=patmos-clang}            # patmos-clang tool
: ${PLATIN:=./platin}               # PLATIN tool
: ${BENCHMARKS_ROOT:=examples}      # benchmarks are in $BENCHMARKS_ROOT/$SET/*.bc
: ${OUTPUT_ROOT:=examples/${BENCH}} # generated files go to ${OUTPUT_ROOT}-${SET}/*
###############################################################################

ARCH=patmos
for SET in ${SETS} ; do
    if [ -e "${BENCHMARKS_ROOT}/${SET}/Makefile" ] ; then
        make -C "${BENCHMARKS_ROOT}/${SET}"
    fi
    mkdir -p "${OUTPUT_ROOT}-${SET}"
    HTMLFILE=${OUTPUT_ROOT}-${SET}/results.html
    YMLFILE=${OUTPUT_ROOT}-${SET}/results.yml
    rm -f ${HTMLFILE} ${YMLFILE}
    ruby -Ilib eval-summarize.rb --html=$HTMLFILE --html-prolog
    for OPT in ${OPTS}; do
        FILEBASE=${OUTPUT_ROOT}-${SET}/"${ARCH}"-O${OPT}
        OPT_FLAGS="-O${OPT}"
        mkdir -p ${FILEBASE}.{bin,out}
        for BCFILE in ${BENCHMARKS_ROOT}/${SET}/*.bc ; do
            LOGFILE=${FILEBASE}.out/$(basename ${BCFILE} .bc).log
            echo '* '"Evaluating ${SET}: $(basename $BCFILE .bc) -O${OPT} [$LOGFILE]" >&2
            echo '=====' ${BCFILE} '=====' | tee ${LOGFILE}

            M=$(basename "${BCFILE}" .bc)
            ELF_FILE=${FILEBASE}.bin/${M}.elf
            ELFBC_FILE=${FILEBASE}.bin/${M}.elf.bc
            INPML_FILE=${FILEBASE}.bin/${M}.elf.pml
            OUTPML_FILE=${FILEBASE}.out/${M}.pml

            ${CLANG} ${OPT_FLAGS} -o "${ELF_FILE}" -mpatmos-preemit-bitcode="${ELFBC_FILE}" \
                -mserialize="${INPML_FILE}" "${BCFILE}"  2>&1 | tee ${LOGFILE}
            ${PLATIN} bench-${BENCH} --bitcode "${ELFBC_FILE}" --outdir "${FILEBASE}.out" \
                --binary ${ELF_FILE} -o ${OUTPML_FILE} ${INPML_FILE} 2>&1 | tee ${LOGFILE}
        done >/dev/null # > ${FILEBASE}.log

        ruby -Ilib eval-summarize.rb "${FILEBASE}".out/*.pml --verbose --logdir="${ARCH}-O${OPT}.out" \
             --html="${HTMLFILE}" --name="${BENCH}-${SET}-O${OPT}" >> $YMLFILE
    done
    ruby -Ilib eval-summarize.rb --html-epilog --html=${HTMLFILE}
done

for SET in ${SETS} ; do
    echo "* ${SET}"
    echo " HTML report: ${OUTPUT_ROOT}-${SET}/results.html"
    YMLFILE=${OUTPUT_ROOT}-${SET}/results.yml
    echo " YML raw data: ${YMLFILE}"
    if [ -e "${YMLFILE}".expect ] ; then
        if ! diff -q "${YMLFILE}".expect "${YMLFILE}" ; then
            echo " WARNING: Results changed (compared to ${YMLFILE}.expect)"
        fi
    fi
done

