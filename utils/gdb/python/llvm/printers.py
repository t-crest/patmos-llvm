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

def gdb_eval(command, default):
    try:
	return gdb.parse_and_eval(command)
    except:
	return default


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

class BucketIterator:
    "Iterate over an array of elements that has Empty and Tombstone markers"

    def __init__(self, start, size, numbuckets, empty, tombstone, eltype):
	self.eltype = eltype
	self.p = start.cast(eltype)
	self.size = size
	self.end = self.p + numbuckets
	self.tombstone = tombstone
	self.empty = empty
	self.count = 0

	self.advancePastEmpty()

    def __iter__(self):
	return self

    def next(self):
	if self.count >= self.size:
	    raise StopIteration
	
	el = self.p.cast(self.eltype)

	result = ('[%d]' % self.count, el)

	self.count = self.count + 1
	self.p = el + 1

	self.advancePastEmpty()

	return result

    def advancePastEmpty(self):
	while self.p != self.end:
	    key = self.getKey()
	    if key != self.empty and key != self.tombstone:
		break
	    b = self.p.cast(self.eltype)
	    self.p = b + 1

    def getKey(self):
	return self.p


class SmallPtrSetPrinter:
    "Print a llvm::SmallPtrSet"

    def __init__(self, typename, val):
        self.typename = typename
        self.val = val
        self.keytype = val.type.template_argument(0)

    def children(self):
        start = self.val['CurArray']
        size = self.val['NumElements']
	buckets = self.val['CurArraySize']
	array = self.val['SmallArray']

	empty = -1
	tombstone = -2
	eltype = self.keytype.pointer()

        return BucketIterator(start, size, buckets, empty, tombstone, eltype)

    def to_string(self):
        size = self.val['NumElements']
	buckets = self.val['CurArraySize']
	start = self.val['CurArray']
	array = self.val['SmallArray']

	small = "small" if start == array else "not small"
        return '%s of length %d, buckets %d, %s' % (self.typename, long (size), long(buckets), small)

    def display_hint (self):
        return 'array'

class DenseMapPrinter:
    "Print a llvm::DenseMap"

    class _iter(BucketIterator):
	def getKey(self):
	    return self.p['first']

    def __init__(self, typename, val):
        self.typename = typename
        self.val = val

    def children(self):
        start = self.val['Buckets']
        size = self.val['NumEntries']
	buckets = self.val['NumBuckets']

	# This is a bit of a hack: by using try-catch we remove a 'not found' warning message, but it works anyway
	# Defaults to -4 if eval is not working (this should check for custom Infos, raise error if custom info and default is used)
	empty = gdb_eval(str(self.val.type.template_argument(2))+'::getEmptyKey()', -4)
	tombstone = gdb_eval(str(self.val.type.template_argument(2))+'::getTombstoneKey()', -4)

        return self._iter(start, size, buckets, empty, tombstone, start.type)

    def to_string(self):
        size = self.val['NumEntries']
	buckets = self.val['NumBuckets']

        return '%s of length %d, buckets %d' % (self.typename, long (size), long(buckets))

    def display_hint (self):
        return 'map'


class StringRefPrinter:
    "Print a llvm::StringRef of some kind"

    def __init__(self, typename, val):
        self.val = val

    def to_string(self):
        #type = self.val.type
        #if type.code == gdb.TYPE_CODE_REF:
        #    type = type.target ()
	
        ptr = self.val['Data']
        len = self.val['Length']
	try:
	    return ptr.string (encoding = "utf-8", length = len)
	except:
	    return ptr.string (encoding = "iso-8859-1", length = len)

    def display_hint (self):
        return 'string'

class InitPrinter:
    "Print a TableGen Init class nicely"

    def __init__(self, typename, val):
	self.val = val
	self.typename = typename

    def to_string(self):
	if self.val.type.code == gdb.TYPE_CODE_PTR:
	    val = self.val.dereference()
	else:
	    val = self.val

	dyn = str(val.dynamic_type)
	dval = val.cast(val.dynamic_type)
	if dyn == "llvm::StringInit":
	    return str(dval['Value'])
	    
	if dyn == "llvm::BitInit":
	    return "BitInit: " + str(dval['Value'])
	    
	if dyn == "llvm::IntInit":
	    return dval['Value']
	
	return str(val.dynamic_type)
	
    def display_hint(self):
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
        self.compiled_rx = re.compile('^([a-zA-Z0-9_:]+)<.*>( \*)?$')

    def add(self, name, function, pointer = False):
	name = "llvm::" + name
        # A small sanity check.
        if not self.compiled_rx.match(name + '<>'):
            raise ValueError, 'llvm programming error: "%s" does not match' % name
        printer = RxPrinter(name, function)
        self.subprinters.append(printer)
        self.lookup[name] = printer
	if pointer:
	    self.lookup[name+" *"] = printer

    @staticmethod
    def get_basic_type(type):
        # If it points to a reference, get the reference.
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target ()

        # Get the unqualified type, stripped of typedefs.
        type = type.unqualified ().strip_typedefs ()

	if type.code == gdb.TYPE_CODE_PTR:
	    return str(type)

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
    #llvm_printer.add('ArrayRef', ArrayRefPrinter)
    #llvm_printer.add('TinyPtrVector', TinyPtrVectorPrinter)
    llvm_printer.add('SmallVector', SmallVectorPrinter)
    llvm_printer.add('SmallVectorImpl', SmallVectorPrinter)
    #llvm_printer.add('ilist', IlistPrinter)
    #llvm_printer.add('PackedVector', PackedVectorPrinter)

    #llvm_printer.add('SmallSet', SmallSetPrinter)
    # TODO needs testing with nonempty set
    #llvm_printer.add('SmallPtrSet', SmallPtrSetPrinter)
    #llvm_printer.add('DenseSet', DenseSetPrinter)
    #llvm_printer.add('SparseSet', SparseSetPrinter)
    #llvm_printer.add('FoldingSet', FoldingSetPrinter)
    #llvm_printer.add('SetVector', SetVectorPrinter)
    #llvm_printer.add('UniqueVector', UniqueVectorPrinter)
    #llvm_printer.add('ImmutableSet', ImmutableSetPrinter)

    #llvm_printer.add('StringMap', StringMapPrinter)
    #llvm_printer.add('IndexedMap', IndexedMapPrinter)
    llvm_printer.add('DenseMap', DenseMapPrinter)
    #llvm_printer.add('ValueMap', ValueMapPrinter)
    #llvm_printer.add('MultiImplMap', MultiImplMapPrinter)
    #llvm_printer.add('FlatArrayMap', FlatArrayMapPrinter)
    #llvm_printer.add('SmallMap', SmallMapPrinter)
    #llvm_printer.add('IntervalMap', IntervalMapPrinter)
    #llvm_printer.add('IntEqClasses', IntEqClassesPrinter)
    #llvm_printer.add('ImmutableMap', ImmutableMapPrinter)

    #llvm_printer.add('BitVector', BitVectorPrinter)
    #llvm_printer.add('SmallBitVector', SmallBitVectorPrinter)
    #llvm_printer.add('SparseBitVector', SparseBitVectorPrinter)

    llvm_printer.add('StringRef', StringRefPrinter)
    #llvm_printer.add('Twine', TwinePrinter)
    #llvm_printer.add('SmallString', SmallStringPrinter)

    llvm_printer.add('Init', InitPrinter, True)

    #if True:
        # These shouldn't be necessary, if GDB "print *i" worked.
        # But it often doesn't, so here they are.
        #llvm_printer.add_container('std::', '_List_iterator', StdListIteratorPrinter)


build_llvm_dictionary ()
