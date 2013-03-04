#!/bin/bash

# >>> customize below <<<
SETS="mrtc_ext"
OPTS="0 1 2"
EVAL_ROOT=examples/trace
# ^^^ customize above ^^^


ARCH=patmos
rm -f results-trace.yaml
for SET in ${SETS} ; do
    HTMLFILE=${EVAL_ROOT}-${SET}/results.html
    YMLFILE=${EVAL_ROOT}-${SET}/results.yml
    rm -f ${HTMLFILE} ${YMLFILE}
    ruby -Ilib eval-summarize.rb --html=$HTMLFILE --html-prolog
    for OPT in ${OPTS}; do
        FILEBASE=${EVAL_ROOT}-${SET}/"${ARCH}"-O${OPT}
        mkdir -p ${FILEBASE}.{bin,out}
        for BCFILE in examples/${SET}/*.bc ; do
            LOGFILE=${FILEBASE}.out/$(basename ${BCFILE} .bc).log
            echo '* '"Evaluating ${SET}: $(basename $BCFILE .bc) -O${OPT} [$LOGFILE]" >&2
            echo '=====' ${BCFILE} '=====' | tee ${LOGFILE}
            PATMOS_CFLAGS="-O${OPT}" STATS=1 ./run-benchmark ${FILEBASE}.{bin,out} ${BCFILE} bench-trace 2>&1 | tee ${LOGFILE}
        done >/dev/null # > ${FILEBASE}.log
        ruby -Ilib eval-summarize.rb "${FILEBASE}".out/*.pml --verbose --logdir="${ARCH}-O${OPT}.out" \
             --html="${HTMLFILE}" --name="trace-${SET}-O${OPT}" >> $YMLFILE
    done
    ruby -Ilib eval-summarize.rb --html-epilog --html=${HTMLFILE}
done

for SET in ${SETS} ; do
    echo "* ${SET}"
    echo " HTML report: ${EVAL_ROOT}-${SET}/results.html"
    echo " YML raw data: ${EVAL_ROOT}-${SET}/results.yml"
done
