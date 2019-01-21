#!/bin/bash
# Ensures that a program, when compiled as using singlepath code, 
# executes in the same way regardless of input, and that its output is correct.
#
# The program is expected to be in LLVM IR. The script will compile it,
# run it using 'pasim' once for each execution argument, and check
# that 'pasim's execution statistics are equivalent for all runs.
#
# usage:
# It takes >= 6 arguments:
#	1. The path to LLVM's binary folder. E.g. '$t-crest-home/llvm/build/bin'.
#		May contain a '.'. If so, everything after (and including) the '.' is ignored.
#		This allows the use of llvm-lit's substition, where 'llc' will give the correct path.
#		I.e. llvm-lit will substitue 'llc' for '$t-crest-home/llvm/build/bin/./llc' which will work.
#		The llvm binary folder must be exactly 3 levels below '$t-crest-home', otherwise the script
#		will fail.
#	2. The path to the source program to Test.
#	3. Additional build arguments for llc. E.g. '-O2' for a specific optimization flag.
#		Must be exactly 1 argument to the script, so if you want to pass multiple arguments to llc
#		they should be wrapped in quotes. E.g. "-O2 -v".
#		Also, if no arguments are needed, "" must be used.
#	4. A singlepath function to compile as singlepath and run statistics on
#	>5. a list of execution arguments
#		Each execution argument has the input to send to the program through stdin.
#		The argument also includes the expected output of the program (on stdout).
#		The two values are separated by '='.
#		E.g '1=2' will run the program, send it '1' through the stdin. when the program finishes
#		the script ensures that the program output '2' on stdout.
#		Giving '1=2 2=3' will run the program twice, ensuring the first time gets '1' and outputs '2'
#		and the rest_stats time gets '2' and outputs '3'.
#		At least two execution arguments must be given.
#
# Additionally, the script ensures that all runs of the program produce equivalent pasim statistics.
# This means the same number of instructions (and type of instruction) is fetched (but not retired/discarded),
# the same number of cycles are spent in the function, and the same number of operations are executed.
# This ensures that the code is singlepath. 
#
# Requirements:
#	The design of this script assumes that setup of t-crest on the machine was done by
#	the 'build.sh' scipt in the patmos-misc repository (github.com/t-crest/patmos-misc).
#	Specifically, this scripts uses the 'local' directory created by the script, assuming
#	it is in the same directory as LLVM. 
#	Additionally, it requires 'pasim' and 'patmos-ld' are discoverable on the path.
#	Lastly, this script uses python code, therefore, 'python3' must also be on the path.
#
# Notes:
#	This script will create files as part of its build but deletes before terminating.
#	Some messages printed by the script may refer to files that have been deleted.
#
# TODO: Currently only supports running 'pasim' statistics on 1 function. Should support multiple.
#

# This is imbedded python code. Can be accessed through the $python_pasim_stat_clean
# variable and run using 'python3 -c "$python_pasim_stat_clean"'.
#
# It reads 'pasim's statistics from stdin and cleans them
# leaving only the stats needed to ensure two run of a singlepath
# program are identical (execution-wise).
# We embed the python code here instead of having it in its own
# file to ensure that its always present and callable, even if this
# script is moved.
# The cleaning could have been done in pure bash script, but that
# increases the execution time significantly (x7 in the tested cases).
read -r -d '' python_pasim_stat_clean << EndOfPython
import fileinput

input = fileinput.input()

#Find the instruction statistics
for line in input:
	if line.strip() == "Instruction Statistics:" :
		input.readline() #Discard next line
		break;
		
	if line.strip() == "Pasim options:" :
		raise ValueError("No pasim statistics given.")
		break;

#output cleaned instruction statistics
for inst in input:
	if inst.strip().startswith("all:"):
		break
		
	split_line = inst.split()
	name = split_line[0]
	fetch_count = int(split_line[1]) + int(split_line[4])
	print(name + " " + str(fetch_count))

#Find and output operation count
for line in input:
	if line.strip().startswith("Operations:"):
		split = line.split()
		print(split[0] + " " + split[1])
		break

#Find profiling information
for line in input:
	if line.strip().startswith("Profiling information:"):
		#Discard the next 3 lines, which are just table headers
		input.readline()
		input.readline()
		input.readline()
		break;
		
#Output how many times each function is called
while True:
	line = input.readline().strip()
	if line.startswith("<"):
		input.readline() #Discard next line
		count = input.readline().strip().split()[0]
		print(line[1:].split(">")[0] + "(): " + count)
	else:
		#Not part of the profiling
		break;

EndOfPython

# Executes the given program (arg 1), running statistics on the given function (arg 2).
# Argument 3 is the execution arguments (see top of file for description).
# Tests that the output of the program match the expected output. If not, reports an error.
# Returns the cleaned statistics.
execute_and_stat(){
	# Split the execution argument into input and expected output.
	split=(${3//=/ })
	input=${split[0]}
	expected_out=${split[1]}
	
	ret_code=0
	
	# The next line runs the program ($1) on 'pasim' with the input. 
	# It then pipes the stdout of the program to the variable 'actual_out' 
	# and the 'pasim' stats (which are printed to stderr) to 'pasim_stats'.
	# An explanation of the line can be found at https://stackoverflow.com/a/26827443/8171453
	. <({ pasim_stats=$({ actual_out=$(echo "$input" | pasim "$1" --print-stats "$2" -V); } 2>&1; declare -p actual_out >&2); declare -p pasim_stats; } 2>&1)
	
	# Test the the stdout of the program is as expected
	if ! diff <(echo "$expected_out") <(echo "$actual_out") &> /dev/null ; then
		(>&2 echo "The execution of '$1' for input argument '$input' gave the wrong output through stdout.")
		(>&2 echo "-------------------- Expected --------------------")
		(>&2 echo "$expected_out")
		(>&2 echo "--------------------- Actual ---------------------")
		(>&2 echo "$actual_out")
		(>&2 echo "--------------------------------------------------")
		# '(>&2 ...)' outputs '...' on stderr
		ret_code=1
	fi
	
	# Clean 'pasim's statistics
	cleaned_stats=$(echo "$pasim_stats" | python3 -c "$python_pasim_stat_clean" 2> /dev/null)
	if [ $? -ne 0 ]; then
		(>&2 echo "Failed to clean pasim statistics from run of '$1' with functions '$2' and execution argument '$3'")
		(>&2 echo "Pasim statistics were:")
		(>&2 echo "$pasim_stats")
		ret_code=1
	fi
	
	# Return the cleaned stats to caller.
	echo "$cleaned_stats"
	
	# Whether anything failed.
	return $ret_code
}

#------------------------------------ Start of script execution -----------------------------------

# Ensure that at least 2 execution arguments were given,
# such that we can compare at least 2 executions
if [ $# -lt 6 ]; then
	echo "Must have at least 2 execution arguments but was: ${@:4}"
	exit 1
fi

# Takes the path to LLVM's build binaries and removes 
# everything (and including) the first '.'
bin_dir=(${1//./ })

# Find the path to the standard library to link with
link_libs_dir=$bin_dir../../../local/patmos-unknown-unknown-elf/lib

# The source file to test
bitcode="$3"

linked="$bitcode.link"

# The LLVM-linked object file, still missing final linked
compiled="$linked.o"

# Final executable
exec="$compiled.o"

# Link the source LLVM IR with the standard library and then compile to assembly
$bin_dir/llvm-link -nostdlib -L$link_libs_dir/ $link_libs_dir/crt0.o $link_libs_dir/crtbegin.o $bitcode $link_libs_dir/libcsyms.o -lc -lpatmos $link_libs_dir/librtsfsyms.o -lrtsf $link_libs_dir/librtsyms.o -lrt $link_libs_dir/crtend.o -o $linked
if [ $? -ne 0 ]; then 
	echo "Failed to link '$bitcode'."
	exit 1
fi

$bin_dir/llc $linked $2 -mforce-block-labels -disable-separate-nested-loops -filetype=obj -o $compiled -mpatmos-singlepath="$4"
rm $linked #cleanup
if [ $? -ne 0 ]; then 
	echo "Failed to compile '$linked'."
	exit 1
fi

# Final assembly linking and generation of an executable patmos file
patmos-ld -nostdlib -static --defsym __heap_start=end --defsym __heap_end=0x100000 --defsym _shadow_stack_base=0x1f8000 --defsym _stack_cache_base=0x200000 -o $exec $compiled 
rm $compiled #cleanup
if [ $? -ne 0 ]; then
	echo "Failed to generate executable from '$compiled'."
	exit 1
fi

ret_code=0

# Run the first execution argument on its own,
# such that its stats result can be compared to
# all other executions
first_stats=$(execute_and_stat "$exec" "$4" "$5")
if [ $? -ne 0 ]; then
	ret_code=1
fi

# Run the rest of the execution arguments.
# For each one, compare to the first. If they all
# are equal to the first, they must also be equal to each other,
# so we don't need to compare them to each other.
for i in "${@:6}" 
do
	rest_stats=$(execute_and_stat "$exec" "$4" "$i")
	if [ $? -ne 0 ]; then
		# There was an error in executing the program or cleaning the stats
		ret_code=1 
	fi
	if ! diff <(echo "$first_stats") <(echo "$rest_stats") ; then
		echo "The execution of '$exec' for execution arguments '$5' and '$i' weren't equivalent."
		ret_code=1
	fi
done

rm $exec #cleanup

exit $ret_code