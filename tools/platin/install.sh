#!/bin/bash
#
SRC_DIR=$(dirname "${0}")

INSTALL_DIR=$1
if [ -z "${INSTALL_DIR}" ] ; then
echo "usage: ${0} <prefix>" >&2
echo >&2
echo "Install the platin toolchain relative to the given prefix" >&2
echo "If INSTALL_GEMS is set, the ruby libraries are installed as well" >&2
echo "Installed Files: " >&2
echo "  <prefix>/bin/platin  ... Binary" >&2
echo "  <prefix>/lib/platin/ ... Library Directory" >&2
exit 1
fi

BINARY="${INSTALL_DIR}/bin/platin"
GEM_DIR="${INSTALL_DIR}/lib/gems/1.9"
echo "${BINARY}"
mkdir -p "$(dirname "${BINARY}")"
cat <<EOF >"${BINARY}"
#!/bin/bash

# Installer
RELATIVE_LIBDIR="../lib/platin"

# Local Gems
export GEM_PATH="${GEM_DIR}:${GEM_PATH}"

# Executable
EOF
cat  "${SRC_DIR}/platin"  >> "${BINARY}"
chmod uga+x "${BINARY}"

for libfile in $(cd "${SRC_DIR}/lib" ; find "." -name '*.rb') ; do
    SRC="${SRC_DIR}/lib/${libfile}"
    DST="${INSTALL_DIR}/lib/platin/${libfile}"
    echo "${DST}"
    mkdir -p "$(dirname "${DST}")"
    cp "${SRC}" "${DST}"
done

if [ ! -z "${INSTALL_GEMS}" ] ; then
    echo "Installing ruby gems"
    "${SRC_DIR}"/ext/install_gems.sh "${GEM_DIR}"
fi