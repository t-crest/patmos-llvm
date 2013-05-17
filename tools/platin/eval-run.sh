#!/bin/bash
# Customizable Variables
#################################################################################
: ${BENCH:=rg}                     # benchmark script, one out of {trace,sweet}
: ${SET:=mrtc_ext}                  # benchmark set
# : ${LD_FLAGS:=-lm}                  # Linker flags
: ${ARCH:=armv7a}                   # architecture, one out of {patmos,armv4t,armv7a}
: ${OPTS:=0 2 2-if-in}              # IFS-separated list of optimization levels {0,1,2}_suffix
                                    # Optimization suffixes: -if => disable ifcvt
                                    #                        -in => disable inlining
: ${CLANG:=patmos-clang}            # clang tool
: ${PLATIN:=./platin}               # PLATIN tool
: ${BENCHMARKS_ROOT:=examples}      # benchmarks are in     ${BENCHMARKS_ROOT}/${SET}/${ARCH}/*.bc
: ${OUTPUT_ROOT:=results}           # generated files go to ${OUTPUT_ROOT}/${SET}-${BENCH}/${ARCH}

: ${GEM5_HOME:=/home/benedikt/gem5-stable}
##################################################################################
# set -x
set -e

# reads: CLANG, OPT_FLAGS, BINPREFIX, BCFILE, LOGFILE
function compile() {
    case "${OPT}" in
        0*) OPT_FLAGS="-O0" ;;
        1*) OPT_FLAGS="-O1" ;;
        2*) OPT_FLAGS="-O2" ;;
        *) echo "Bad optimization setting ${OPT}" >&2; exit 1 ;;
    esac
    if [[ "${OPT}" =~ '-in' ]]; then
        OPT_FLAGS="-Wl,-disable-inlining ${OPT_FLAGS}"
    fi
    case "${ARCH}" in
        patmos)
            if [[ "${OPT}" =~ '-if' ]]; then
                OPT_FLAGS="-mpatmos-disable-ifcvt ${OPT_FLAGS}"
            fi
            ${CLANG} ${OPT_FLAGS} -o "${BINPREFIX}.elf" -mpreemit-bitcode="${BINPREFIX}.elf.bc" \
                -mserialize="${BINPREFIX}.elf.pml" "${BCFILE}" "${LD_FLAGS}" 2>&1 | tee -a "${LOGFILE}" ;;
        arm*)
            if [[ "${OPT}" =~ '-if' ]]; then
                OPT_FLAGS="-Xclang -backend-option -Xclang -ifcvt-limit=0 ${OPT_FLAGS}"
            fi
            ${CLANG} ${OPT_FLAGS} -march="${ARCH}" -target arm-linux-gnueabi \
                -Xclang -backend-option -Xclang -mserialize="${BINPREFIX}.elf.pml" \
                -Xclang -backend-option -Xclang -mpreemit-bitcode="${BINPREFIX}.elf.bc" \
                "${LD_FLAGS}" -c -o "${BINPREFIX}".o  "${BCFILE}" ;
              arm-linux-gnueabi-gcc -static "${BINPREFIX}".o -o "${BINPREFIX}".elf ;;
        *) echo "Bad architecture ${ARCH}" >&2; exit 1 ;;
    esac
}

function analyze() {
    rm -f "${OUTPREFIX}.pml"
    case "${ARCH}" in
        patmos)  ${PLATIN} bench-${BENCH} --bitcode "${BINPREFIX}.elf.bc" --outdir "${OUTDIR}" \
                --binary "${BINPREFIX}.elf" -o "${OUTPREFIX}.pml" "${BINPREFIX}.elf.pml" 2>&1 | tee -a ${LOGFILE} ;;
        arm*) $GEM5_HOME/build/ARM/gem5.opt --debug-flags=Exec,-ExecMicro,ExecMacro --trace-file=trace $GEM5_HOME/configs/example/se.py -c "${BINPREFIX}.elf" ;
              cp m5out/trace "${OUTPREFIX}.mtf"
              ${PLATIN} bench-${BENCH} --disable-ait  --bitcode "${BINPREFIX}.elf.bc" --trace-file="${OUTPREFIX}.mtf"  --outdir "${OUTDIR}" \
                --binary "${BINPREFIX}.elf" -o "${OUTPREFIX}.pml" "${BINPREFIX}.elf.pml" 2>&1 | tee -a ${LOGFILE} ;;
        *) echo "Bad architecture ${ARCH}" >&2; exit 1 ;;
    esac
    # if something failed, at least copy the files, so we see there is data missing
    if [ ! -e "${OUTPREFIX}.pml" ] ; then
        cp "${BINPREFIX}.elf.pml" "${OUTPREFIX}.pml"
        touch "${OUTPREFIX}.pml"
    fi
}

SRCDIR="${BENCHMARKS_ROOT}/${SET}/${ARCH}"
RESULTDIR="${OUTPUT_ROOT}/${SET}-${BENCH}/${ARCH}"
# Recompile bitcode if necessary
if [ -e "${SRCDIR}/Makefile" ] ; then
    make -C "${SRCDIR}"
fi

# Initialize RESULTDIR
mkdir -p "${RESULTDIR}"
HTMLFILE="${RESULTDIR}"/results.html
YMLFILE="${RESULTDIR}"/results.yml
rm -f ${HTMLFILE} ${YMLFILE}
ruby -Ilib eval-summarize.rb --html=${HTMLFILE} --html-prolog

# For all optimization settings
for OPT in ${OPTS}; do
    FILEBASE="${RESULTDIR}/OPT-${OPT}"
    BINDIR="${FILEBASE}/bin"
    OUTDIR="${FILEBASE}/out"
    mkdir -p ${BINDIR} ${OUTDIR}
    # For all benchmarks
    for BCFILE in ${SRCDIR}/*.bc ; do
        # set logfile
        M=$(basename "${BCFILE}" .bc)
        LOGFILE=${OUTDIR}/${M}.log
        echo '* '"Evaluating ${SET}: $(basename $BCFILE .bc) -O${OPT} [$LOGFILE]" >&2
        echo '=====' ${BCFILE} '=====' | tee ${LOGFILE}

        BINPREFIX=${BINDIR}/${M}
        compile
        OUTPREFIX=${OUTDIR}/${M}
        analyze
    done >/dev/null # > ${FILEBASE}.log
    ruby -Ilib eval-summarize.rb "${OUTDIR}"/*.pml --verbose --logdir="${OUTDIR}" \
        --html="${HTMLFILE}" --name="${BENCH}-${SET}-O${OPT}" >> $YMLFILE
done

ruby -Ilib eval-summarize.rb --html-epilog --html=${HTMLFILE}
YMLFILE=${RESULTDIR}/results.yml
echo "* ${SET}"
echo " HTML report: ${RESULTDIR}/results.html"
echo " YML raw data: ${YMLFILE}"
if [ -e "${YMLFILE}".expect ] ; then
    if ! diff -q "${YMLFILE}".expect "${YMLFILE}" ; then
        echo " WARNING: Results changed (compared to ${YMLFILE}.expect)"
    fi
fi
