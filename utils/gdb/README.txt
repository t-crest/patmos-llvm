GDB Pretty Printing for LLVM
----------------------------

Those scripts provide basic pretty printing support for common LLVM containers.
The code is partially derived from the libstdc++ pretty printer from
http://sourceware.org/gdb/wiki/STLSupport

To use this, modify the provided gdbinit file to your paths and either tell gdb to use 
this gdbinit file, or create a copy of it it as ~/.gdbinit.
If you want to have pretty printing for STL containers too, check out the libstdcxx printers
from the svn repository that can be found at the wiki page given above and place the
'llvm/' directory in 'python/' into the 'python/' directory of the svn checkout.

To use pretty printing with eclipse, make sure you use at least Eclipse 3.7 with CDT 8.0.
Generate Eclipse project files using cmake (preferably keep your build dir separate from 
your source tree), and import your build directory as existing eclipse project.
Check that pretty printing is enabled in the C++ -> Debug settings in Properties.
Use the GDB (DSF) launcher in your Debug Configuration. 

You may need to change the 'Source lookup paths' in your Debug Configuration to the following
  - Absolute File Paths
  - File System path to your source tree (including subfolders)
  - File System path to your build tree (including subfolders)
Remove any Project or Project Relative Source file paths, they break breakpoints.

