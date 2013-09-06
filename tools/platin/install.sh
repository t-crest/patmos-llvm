#!/bin/bash
#
# platin installation script
#

SRC_DIR=$(dirname "${0}")

# find ruby,gem,rdoc commands
source "${SRC_DIR}/ext/detect_ruby_commands"
detect_ruby
detect_gem_command

function usage() {
    echo "usage: ${0} -i INSTALL_DIR [-d]" >&2
    echo >&2
    echo "Install the platin toolchain relative to the given prefix" >&2
    echo "Installed Files: " >&2
    echo "  <prefix>/bin/platin      ... Binary" >&2
    echo "  <prefix>/lib/platin/     ... Library Directory" >&2
    echo "  <prefix>/lib/platin/gems ... Ruby Libraries (if necessary)" >&2
    echo "OPTIONS" >&2
    echo "  -d     dry run" >&2
    echo "  -v     verbose" >&2
    exit 1

}

while getopts "hi:dvx" opt; do
  case $opt in
    h) usage; exit 0 ;;
    i) INSTALL_DIR="${OPTARG}" ;;
    d) DRYRUN=true; VERBOSE=true ;;
    v) VERBOSE=true ;;
    x) set -x ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ -z "${INSTALL_DIR}" ] ; then
    usage
fi

BINARY="${INSTALL_DIR}/bin/platin"
COMPILER="${INSTALL_DIR}/bin/patmos-clang-wcet"
LIB_DIR="${INSTALL_DIR}/lib/platin"
GEM_DIR="${LIB_DIR}/gems"

function info() {
    if [ ! -z "${VERBOSE}" ] ; then
	echo 'INFO' "${@}"
    else
	echo "${@}"
    fi
}

function install() {
    DST="${1}"
    SRC="${2}"
    info "Installing ${SRC} -> ${DST}"
    run mkdir -p "$(dirname "${DST}")"
    run cp "${SRC}" "${DST}"
}

function verbose() {
    if [ ! -z "${VERBOSE}" ] ; then
	echo "${@}"
    fi
}
function run() {
    CMD="${1}"
    shift
    verbose "${CMD}" "${@}"
    if [ -z "${DRYRUN}" ] ; then
	"${CMD}" "${@}"
    fi
}

# A note on GEM_PATH (as of ruby 1.9.3 / gem 1.8.11):
# * GEM_PATH should be a colon separated list of directories
#   caveat: if there is a *trailing* colon, the GEM_PATH environment
#   variable is *ignored*
# * if GEM_PATH is set, the lookup path for gems is global repo and
#   all directories listed in GEM_PATH
# * if GEM_PATH is not set, the lookup path for gems is global repo
#   *and* the user local repository
#
# Therefore, if we need gems from the user local repository and from
# the platin-specific gem repository, we need to include the
# user local repositories in GEM_PATH. As we do not know about them
# we simply read 'gem env gempath' and add all directories to
# GEM_PATH.
function ruby_wrapper_header() {
    echo '#!/bin/bash'
    cat "${SRC_DIR}/ext/detect_ruby_commands"
    cat <<EOF
detect_ruby
detect_gem_command

if [ -z "\${GEM}" ] ; then
  export GEM_PATH="${GEM_DIR}"
else
  CURRENT_GEM_PATH=\$(\${GEM} env gempath)
  export GEM_PATH="${GEM_DIR}:\${CURRENT_GEM_PATH}"
fi

EOF
}

function install_binary() {
    install "${BINARY}" "${SRC_DIR}/platin"
    run chmod uga+x "${BINARY}"
    if [ -z "${DRYRUN}" ] ; then
        ruby_wrapper_header                    >"${BINARY}"
	echo 'RELATIVE_LIBDIR="../lib/platin"' >>"${BINARY}"
	cat "${SRC_DIR}/platin"                >> "${BINARY}"
    fi
}
function install_wcet_compiler() {
    install "${COMPILER}" "${SRC_DIR}/platin"
    run chmod uga+x "${BINARY}"
    if [ -z "${DRYRUN}" ] ; then
        ruby_wrapper_header  > "${COMPILER}"
        cat <<EOF           >>"${COMPILER}"
LIBDIR=\$(dirname \${0})/../lib/platin
COMPILER_SCRIPT="\${LIBDIR}"/ext/patmos-clang-wcet.rb
exec \${RUBY} -I "\${LIBDIR}" "\${COMPILER_SCRIPT}" "\${@}"
EOF
    fi
}
function install_late_bypass() {
  local dir="${SRC_DIR}/ext/patch_loads"
  if make -C $dir 2>&1 > /dev/null ; then
    DST="${INSTALL_DIR}/lib/platin/ext/patch_loads"
    install "${DST}" "${dir}/patch_loads"
  else
    info "Warning: could not build patch_loads tool." \
         "platin late-bypass will not work."
  fi
}


install_binary
install_wcet_compiler
install_late_bypass

for libfile in $(cd "${SRC_DIR}/lib" ; find "." -name '*.rb' -o -name '*.yml') ; do
    SRC="${SRC_DIR}/lib/${libfile}"
    DST="${INSTALL_DIR}/lib/platin/${libfile}"
    install "${DST}" "${SRC}"
done

if [ ! -z "${GEM}" ] ; then
    if [ -z "${GEM_PATH}" ] ; then
	export GEM_PATH="${GEM_DIR}"
    else
	export GEM_PATH="${GEM_DIR}:${GEM_PATH}"
    fi
    for gemqname in rsec:0.4 ruby-graphviz:1.0.8 kwalify:0.7.2 lpsolve:5.5.10.j ; do
	gemname=$(echo "${gemqname}" | cut -d':' -f1)
	gemversion=$(echo "${gemqname}" | cut -d':' -f2)
	verbose "INFO checking for gem ${gemqname}"
	if [ "$(${GEM} list ${gemname} --version ${gemversion} -i)" != "true" ] ; then
	    info "Installing gem ${gemname} --version ${gemversion} (missing)"
	    local_gemfile="${SRC_DIR}/ext/${gemname}-${gemversion}.gem"
	    if [ -z "${DRYRUN}" ] ; then
		if [ -e "${local_gemfile}" ] ; then
		    gem_args="${local_gemfile}"
		else
		    gem_args="${gemname} --version ${gemversion}"
		fi
		"${GEM}" install --install-dir "${GEM_DIR}" ${gem_args}  -q 2>&1 | sed 's/^/[GEM] /'
		if [ "${PIPESTATUS[0]}" -ne 0 ] ; then
		    echo "WARNING: failed to install gem ${gem_args}. platin will not work." >&2
		fi
	    fi
	fi
    done
else
    echo "WARNING: did not find 'gem' command; skipping ruby library checks" >&2
fi
