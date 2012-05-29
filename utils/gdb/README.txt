GDB Pretty Printing for LLVM
----------------------------

This script provides basic pretty printing support for common LLVM containers.
The code is partially derived from the libstdc++ pretty printer from
http://sourceware.org/gdb/wiki/STLSupport

Currently only a small set of LLVM containers are supported, more will follow when I need them...

--- Usage ---

Optionally copy the python directory to ~/gdb_printers/ or any other path.

If you want to have pretty printing for STL containers too, check out the libstdcxx printers
from this svn repository (from the STLSupport wiki), e.g. into ~/gdb_printers/:
  
  svn co svn://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python

Place the 'python/llvm/' directory into the 'python/' directory of the svn checkout and 
remove the comments from the gdbinit file to enable loading of the libstdcxx printers.


In any case, modify the provided 'gdbinit' file to match your paths. Either tell gdb to use 
this gdbinit file with 'gdb -x <gdbinit>', or copy it to ~/.gdbinit.


--- Debugging with Eclipse ---

o) Setup

To use pretty printing with eclipse, make sure you use at least Eclipse 3.7 with CDT 8.0.
Generate Eclipse project files using cmake (preferably keep your build dir separate from your source tree).
Use File->Import->General->Import Existing Project to import the generated project (leave Copy Sources unchecked).

Check that pretty printing is enabled in the Window->Preferences->C++->Debug->GDB settings.

o) Debug Configuration

Use the GDB (DSF) launcher in your Debug Configurations (Run->Debug Configurations; you can enable 
pretty printing in ~/.gdbinit with the Standard Process Launcher, but this launcher does not show formatted 
results in the variable browser).

In the Debugger Tab, make sure the command file points to your modified gdbinit file (when calling gdb on the command line
or using the Standard Launcher, gdb will always load ~/.gdbinit. When using GDB DSF ~/.gdbinit will *not* be loaded 
automatically).

You may need to change the 'Source lookup paths' in the Source tab to the following (in this order?):
  - Absolute File Path (required by Eclipse to show the source files in the editor on breakpoints)
  - File System path to your source checkout (including subfolders)
  - File System path to your build directory (including subfolders)
Remove any 'Project' or 'Project - Relative to Source' file paths if you get 'File not found' errors on the gdb console when
setting breakpoints.

