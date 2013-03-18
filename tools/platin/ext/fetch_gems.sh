#!/bin/bash
#
# fetch and build gems
#
detect_rake19() {
    RAKE19s="rake1.9.1 rake1.9 rake"
    for g19 in ${RAKE19s} ; do
        if [ ! -z "`which ${g19} 2>/dev/null`" ] ; then
            RAKE19="$g19"
            return
        fi
    done
    echo "rake / rake1.9.1 not found" >&2
    exit 1
}
detect_gem19() {
    GEM19s="gem1.9.1 gem1.9 gem"
    for g19 in ${GEM19s} ; do
        if [ ! -z "`which ${g19}  2>/dev/null`" ] ; then
            GEM19="$g19"
            return
        fi
    done
    echo "gem / gem1.9.1 not found" >&2
    exit 1
}

detect_rake19
detect_gem19

pushd $(dirname $0)

# Fetch ruby gems
${GEM19} fetch rsec --version 0.4
${GEM19} fetch ruby-graphviz --version 1.0.8

# Build additional ruby gems from my github account
if [ ! -e "rb-lpsolve" ] ; then
    git clone https://github.com/visq/rb-lpsolve.git
fi
(cd rb-lpsolve && ${RAKE19} gem)
cp rb-lpsolve/pkg/*.gem .
popd
