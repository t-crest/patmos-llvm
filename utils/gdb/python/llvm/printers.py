# Pretty-printers for llvm.

# Copyright (C) 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
#               2012 Stefan Hepp

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This GDB pretty printing script has been partially based on 
# the python libstdc++ pretty printers available at
# sourceware.org/gdb/wiki/STLSupport


import gdb
import itertools
import re

# Try to use the new-style pretty-printing if available.
_use_gdb_pp = True
try:
    import gdb.printing
except ImportError:
    _use_gdb_pp = False

# Starting with the type ORIG, search for the member type NAME.  This
# handles searching upward through superclasses.  This is needed to
# work around http://sourceware.org/bugzilla/show_bug.cgi?id=13615.
def find_type(orig, name):
    typ = orig.strip_typedefs()
    while True:
        search = str(typ) + '::' + name
        try:
            return gdb.lookup_type(search)
        except RuntimeError:
            pass
        # The type was not found, so try the superclass.  We only need
        # to check the first superclass, so we don't bother with
        # anything fancier here.
        field = typ.fields()[0]
        if not field.is_base_class:
            raise ValueError, "Cannot find type %s::%s" % (str(orig), name)
        typ = field.type


############################################################################

class SmallVectorPrinter:
    "Print a llvm::SmallVector"

    class _iter:
        def __init__(self, start, end, elttype, eltsize):
            self.p = start
            self.end = end
	    self.elttype = elttype
            self.eltsize = eltsize
            self.count = 0

        def __iter__(self):
            return self

        def next(self):
            if self.p == self.end:
                raise StopIteration
	    
	    elt = self.p.cast(self.elttype.pointer())

            result = ('[%d]' % self.count, elt)

            self.count = self.count + 1
            self.p = elt + 1

            return result

    def __init__(self, typename, val):
        self.typename = typename
        self.val = val
        self.elttype = val.type.template_argument(0)
        self.eltsize = self.elttype.sizeof

    def children(self):
        start = self.val['BeginX']
        end = self.val['EndX']
        return self._iter(start, end, self.elttype, self.eltsize)

    def to_string(self):
        start = self.val['BeginX']
        end = self.val['EndX']
	capptr = self.val['CapacityX']

        size = (end - start) / self.eltsize
        capacity = (capptr - start) / self.eltsize

        return '%s of length %d, capacity %d' % (self.typename, long (size), long (capacity))

    def display_hint (self):
        return 'array'


class StringRefPrinter:
    "Print a llvm::StringRef of some kind"

    def __init__(self, typename, val):
        self.val = val

    def to_string(self):
        # Make sure &string works, too.
        type = self.val.type
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target ()

        ptr = self.val ['Data']
        len = self.val['Length']
        return ptr.string (length = len)

    def display_hint (self):
        return 'string'

############################################################################

# A "regular expression" printer which conforms to the
# "SubPrettyPrinter" protocol from gdb.printing.
class RxPrinter(object):
    def __init__(self, name, function):
        super(RxPrinter, self).__init__()
        self.name = name
        self.function = function
        self.enabled = True

    def invoke(self, value):
        if not self.enabled:
            return None
        return self.function(self.name, value)

# A pretty-printer that conforms to the "PrettyPrinter" protocol from
# gdb.printing.  It can also be used directly as an old-style printer.
class Printer(object):
    def __init__(self, name):
        super(Printer, self).__init__()
        self.name = name
        self.subprinters = []
        self.lookup = {}
        self.enabled = True
        self.compiled_rx = re.compile('^([a-zA-Z0-9_:]+)<.*>$')

    def add(self, name, function):
	name = "llvm::" + name
        # A small sanity check.
        if not self.compiled_rx.match(name + '<>'):
            raise ValueError, 'llvm programming error: "%s" does not match' % name
        printer = RxPrinter(name, function)
        self.subprinters.append(printer)
        self.lookup[name] = printer

    @staticmethod
    def get_basic_type(type):
        # If it points to a reference, get the reference.
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target ()

        # Get the unqualified type, stripped of typedefs.
        type = type.unqualified ().strip_typedefs ()

        return type.tag

    def __call__(self, val):
        typename = self.get_basic_type(val.type)
        if not typename:
            return None

	# Check for non-template type first
	if typename in self.lookup:
	    return self.lookup[typename].invoke(val)

        # Check template types.
        match = self.compiled_rx.match(typename)
        if not match:
            return None

        basename = match.group(1)
        if basename in self.lookup:
            return self.lookup[basename].invoke(val)

        # Cannot find a pretty printer.  Return None.
        return None


llvm_printer = None

def register_llvm_printers (obj):
    "Register LLVM pretty-printers with objfile Obj."

    global _use_gdb_pp
    global llvm_printer

    if _use_gdb_pp:
        gdb.printing.register_pretty_printer(obj, llvm_printer)
    else:
        if obj is None:
            obj = gdb
        obj.pretty_printers.append(llvm_printer)

def build_llvm_dictionary ():
    global llvm_printer

    llvm_printer = Printer("llvm")

    # llvm objects requiring pretty-printing.
    #llvm_printer.add('ArrayRef', StdStringPrinter)
    #llvm_printer.add('TinyPtrVector', StdStringPrinter)
    llvm_printer.add('SmallVector', SmallVectorPrinter)
    #llvm_printer.add('ilist', StdStringPrinter)
    #llvm_printer.add('PackedVector', StdStringPrinter)

    llvm_printer.add('StringRef', StringRefPrinter)
    #llvm_printer.add('Twine', StdStringPrinter)
    #llvm_printer.add('SmallString', StdStringPrinter)

    #llvm_printer.add('SmallSet', StdStringPrinter)
    #llvm_printer.add('SmallPtrSet', StdStringPrinter)
    #llvm_printer.add('DenseSet', StdStringPrinter)
    #llvm_printer.add('SparseSet', StdStringPrinter)
    #llvm_printer.add('FoldingSet', StdStringPrinter)
    #llvm_printer.add('SetVector', StdStringPrinter)
    #llvm_printer.add('UniqueVector', StdStringPrinter)
    #llvm_printer.add('ImmutableSet', StdStringPrinter)

    #llvm_printer.add('StringMap', StdStringPrinter)
    #llvm_printer.add('IndexedMap', StdStringPrinter)
    #llvm_printer.add('DenseMap', StdStringPrinter)
    #llvm_printer.add('ValueMap', StdStringPrinter)
    #llvm_printer.add('MultiImplMap', StdStringPrinter)
    #llvm_printer.add('FlatArrayMap', StdStringPrinter)
    #llvm_printer.add('SmallMap', StdStringPrinter)
    #llvm_printer.add('IntervalMap', StdStringPrinter)
    #llvm_printer.add('IntEqClasses', StdStringPrinter)
    #llvm_printer.add('ImmutableMap', StdStringPrinter)

    #llvm_printer.add('BitVector', StdStringPrinter)
    #llvm_printer.add('SmallBitVector', StdStringPrinter)
    #llvm_printer.add('SparseBitVector', StdStringPrinter)

    #if True:
        # These shouldn't be necessary, if GDB "print *i" worked.
        # But it often doesn't, so here they are.
        #llvm_printer.add_container('std::', '_List_iterator', StdListIteratorPrinter)


build_llvm_dictionary ()
