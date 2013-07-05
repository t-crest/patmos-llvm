#!/bin/bash
#
# Install necessary ruby gems
#
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

if [ -z $1 ] ; then
    echo "Error: No install directory specified." >&2
    echo "Usage: $0 <path-to-patmos-install>/lib/gems/1.9" >&2
    exit 1
fi

detect_gem19
INSTALL_DIR="${1}"

pushd $(dirname $0)
HAS_GEMS=1
for gem in $(cat gems.txt) ; do
    if [ ! -e "${gem}" ] ; then
	HAS_GEMS=0
    fi
done
if [ "${HAS_GEMS}" -eq 0 ] ; then
    echo "Fetching Gems"
    ./fetch_gems.sh
fi
FLAGS="--install-dir ${1}"
for gem in $(cat gems.txt) ; do
    ${GEM19} install --install-dir "${INSTALL_DIR}" "${gem}"
done
popd
