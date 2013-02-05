#!/bin/bash

SRC_DIR=$(dirname "${0}")

INSTALL_DIR=$1
if [ -z "${INSTALL_DIR}" ] ; then
echo "usage: ${0} <prefix>" >&2
echo >&2
echo "Install the psk toolchain relative to the given prefix" >&2
echo
echo "Installed Files: " >&2
echo "  <prefix>/bin/psk  ... Binary" >&2
echo "  <prefix>/lib/psk/ ... Library Directory" >&2
exit 1
fi

BINARY="${INSTALL_DIR}/bin/psk"
echo "${BINARY}"
mkdir -p "$(dirname "${BINARY}")"
cat <<EOF >"${BINARY}"
#!/bin/bash

# Installer
RELATIVE_LIBDIR="../lib/psk"

# Executable
EOF
cat  "${SRC_DIR}/psk"  >> "${BINARY}"
chmod uga+x "${BINARY}"

for libfile in $(cd "${SRC_DIR}/lib" ; find "." -name '*.rb') ; do
    SRC="${SRC_DIR}/lib/${libfile}"
    DST="${INSTALL_DIR}/lib/psk/${libfile}"
    echo "${DST}"
    mkdir -p "$(dirname "${DST}")"
    cp "${SRC}" "${DST}"
done
