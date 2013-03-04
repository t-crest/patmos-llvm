#!/bin/bash

# >>> customize below <<<
#########################
BENCHMARKS_ROOT=examples

BENCH=trace
# BENCH=sweet

SETS="basic"
# SETS="basic mrtc"

OPTS="0 1 2"

EVAL_OUTPUT_ROOT=${BENCHMARKS_ROOT}/${BENCH}

#########################
# ^^^ customize above ^^^

ARCH=patmos
for SET in ${SETS} ; do
    mkdir -p "${EVAL_OUTPUT_ROOT}-${SET}"
    HTMLFILE=${EVAL_OUTPUT_ROOT}-${SET}/results.html
    YMLFILE=${EVAL_OUTPUT_ROOT}-${SET}/results.yml
    rm -f ${HTMLFILE} ${YMLFILE}
    ruby -Ilib eval-summarize.rb --html=$HTMLFILE --html-prolog
    for OPT in ${OPTS}; do
        FILEBASE=${EVAL_OUTPUT_ROOT}-${SET}/"${ARCH}"-O${OPT}
        mkdir -p ${FILEBASE}.{bin,out}
        for BCFILE in ${BENCHMARKS_ROOT}/${SET}/*.bc ; do
            LOGFILE=${FILEBASE}.out/$(basename ${BCFILE} .bc).log
            echo '* '"Evaluating ${SET}: $(basename $BCFILE .bc) -O${OPT} [$LOGFILE]" >&2
            echo '=====' ${BCFILE} '=====' | tee ${LOGFILE}
            PATMOS_CFLAGS="-O${OPT}" STATS=1 ./run-benchmark ${FILEBASE}.{bin,out} ${BCFILE} bench-${BENCH} 2>&1 | tee ${LOGFILE}
        done >/dev/null # > ${FILEBASE}.log
        ruby -Ilib eval-summarize.rb "${FILEBASE}".out/*.pml --verbose --logdir="${ARCH}-O${OPT}.out" \
             --html="${HTMLFILE}" --name="${BENCH}-${SET}-O${OPT}" >> $YMLFILE
    done
    ruby -Ilib eval-summarize.rb --html-epilog --html=${HTMLFILE}
done

for SET in ${SETS} ; do
    echo "* ${SET}"
    echo " HTML report: ${EVAL_OUTPUT_ROOT}-${SET}/results.html"
    echo " YML raw data: ${EVAL_OUTPUT_ROOT}-${SET}/results.yml"
done
