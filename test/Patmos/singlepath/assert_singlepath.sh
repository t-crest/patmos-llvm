#!/bin/bash
# Ensures that a program, when compiled using singlepath code, 
# executes in the same way regardless of input, and that its output is correct.
#
# The program is expected to be in LLVM IR and have a main function
# that takes one i32 input. The script will compile it,
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
#	2. The path to the source program to test.
#	3. The path to the temporary file available to the test.
#	4. The path to the _start.ll to link with the test program
#	5. Additional build arguments for llc. E.g. '-O2' for a specific optimization flag.
#		Must be exactly 1 argument to the script, so if you want to pass multiple arguments to llc
#		they should be wrapped in quotes. E.g. "-O2 -v".
#		I no arguments are needed, "" must be used.
#	>6. a list of execution arguments.
#		Each execution argument has the input to send to the program through stdin
#		and the expected output of the program (on stdout). The two values are separated by '='.
#		E.g '1=2' will run the program, send it '1' through the stdin. when the program finishes
#		the script ensures that the program output '2' on stdout.
#		Giving '1=2 2=3' will run the program twice, ensuring the first run gets '1' and outputs '2'
#		and the second run gets '2' and outputs '3'.
#		At least two execution arguments must be given.
#		If input or output need to have spaces, this is possible too using quotes.
#		E.g. the argument '"1 2=3 4"' will run the program with input '1 2' and
#		expect the output '3 4'.
#
# Additionally, the script ensures that all runs of the program produce equivalent pasim statistics.
# This means the same number of instructions (and type of instruction) are fetched 
# (but not retired/discarded), the same number of cycles are spent in the function, 
# and the same number of operations are executed. This ensures that the code is singlepath. 
#
# Requirements:
#   It requires 'pasim' and 'patmos-ld' are discoverable on the path,
#	and that all LLVM tools have been built and are in 'llvm/build/bin'.
#	Lastly, this script uses python code, therefore, 'python' must also be on the path.
#

# This is imbedded python code. Can be accessed through the $python_pasim_stat_clean
# variable and run using 'python3 -c "$python_pasim_stat_clean"'.
#
# It reads pasim's statistics from stdin and cleans them
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

#Find and output cycle count
for line in input:
	if line.strip().startswith("Cycles:"):
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
	
	# We rename spaces such that they are not recognized as list separators when we split
	# the input from the expected output
	placeholder="<!!SPACE!!>"
	no_space=${2// /$placeholder}
	split=(${no_space//=/ })
	
	#We now reinsert the spaces
	input=${split[0]//$placeholder/ }
	expected_out=${split[1]//$placeholder/ }
	
	# The final name of the ELF to execute
	exec=$1_$input
	
	ret_code=0
	
	# Final generation of ELF with added input
	patmos-ld -nostdlib -static -o $exec $1 --defsym input=$input
	if [ $? -ne 0 ]; then
		echo "Failed to generate executable from '$1' for argument '$input'."
		return 1
	fi
	
	# The next line runs the program ($exec) on 'pasim'. 
	# It then pipes the stdout of the program to the variable 'actual_out',
	# the 'pasim' stats (which are printed to stderr) to 'pasim_stats',
	# and the return code to 'pasim_return_code'.
	# An explanation of the line can be found at https://stackoverflow.com/a/26827443/8171453
	# We configure pasim to use an "ideal" data cache, such that any variance in cycle count 
	# because of cache-misses are negated (an ideal cache never misses).
	. <({ pasim_stats=$({ actual_out=$(pasim "$exec" -V -D ideal); pasim_return_code=$?; } 2>&1; declare -p actual_out pasim_return_code>&2); declare -p pasim_stats; } 2>&1)

	# Test the the stdout of the program is as expected
	if ! diff <(echo "$expected_out") <(echo "$pasim_return_code") &> /dev/null ; then
		# Explanation: '(>&2 ...)' outputs the result of command '...' on stderr
		(>&2 echo "The execution of '$1' for input argument '$input' gave the wrong output through stdout.")
		(>&2 echo "-------------------- Expected --------------------")
		(>&2 echo "$expected_out")
		(>&2 echo "--------------------- Actual ---------------------")
		(>&2 echo "$pasim_return_code")
		(>&2 echo "--------------------- stdout ---------------------")
		(>&2 echo "$actual_out")
		(>&2 echo "--------------------------------------------------")
		ret_code=1
	fi
	
	# Clean 'pasim's statistics
	cleaned_stats=$(echo "$pasim_stats" | python -c "$python_pasim_stat_clean" 2> /dev/null)
	if [ $? -ne 0 ]; then
		# If cleaning failed it means the stderr is not statistics and some other
		# error was printed
		(>&2 echo "--------------------- stderr ---------------------")
		(>&2 echo "$pasim_stats")
		(>&2 echo "--------------------------------------------------")
		ret_code=1
	fi
	
	# Return the cleaned stats to caller.
	echo "$cleaned_stats"
	
	# Whether anything failed.
	return $ret_code
}

#------------------------------------ Start of script execution -----------------------------------

# Check required binaries are installed
if ! [ -x "$(command -v patmos-ld)" ] ; then
	echo "Patmos port of the Gold linker 'patmos-ld' could not be found."
	exit 1
fi
if ! [ -x "$(command -v pasim)" ] ; then
	echo "Patmos simulator 'pasim' could not be found."
	exit 1
fi

# Ensure that at least 2 execution arguments were given,
# such that we can compare at least 2 executions
if [ $# -lt 7 ]; then
	echo "Must have at least 2 execution arguments but was: ${@:6}"
	exit 1
fi

# Rename arguments for later use

# Takes the path to LLVM's build binaries and removes 
# everything after (and including) the first '.'
bin_dir=(${1//./ })

# The source file to test
bitcode="$2"

# The temporary file available to the test
temp="$3"

# The object file of the start function to be linked with the program
start_function="$4"

# Additional arguments to pass to LLC
llc_args="$5"

# The first execution argument
exec_arg="$6"

# The object file of the program
compiled="$temp"

# Try to compile the program to rule out compile errors. Throw out the result.
$bin_dir/llc $bitcode $llc_args -filetype=obj -mpatmos-singlepath=main -o $compiled
if [ $? -ne 0 ]; then 
	echo "Failed to compile '$bitcode'."
	exit 1
fi

# Link start function with program
$bin_dir/llvm-link -nostdlib -B=static $start_function $bitcode -o $compiled
if [ $? -ne 0 ]; then 
	echo "Failed to link '$bitcode' and '$start_function'."
	exit 1
fi

# Compile into object file (not ELF yet)
$bin_dir/llc $compiled $llc_args -filetype=obj -mpatmos-singlepath=main -o $compiled
if [ $? -ne 0 ]; then 
	echo "Failed to compile '$bitcode'."
	exit 1
fi

ret_code=0

# Run the first execution argument on its own,
# such that its stats result can be compared to
# all other executions
first_stats=$(execute_and_stat "$compiled" "$exec_arg")
if [ $? -ne 0 ]; then
	echo "$first_stats"
	ret_code=1
fi

# Run the rest of the execution arguments.
# For each one, compare to the first. If they all
# are equal to the first, they must also be equal to each other,
# so we don't need to compare them to each other.
for i in "${@:7}" 
do
	if [ $ret_code -ne 0 ] ; then
		# If an error has already been encountered, stop.
		continue
	fi
	rest_stats=$(execute_and_stat "$compiled" "$i")
	if [ $? -ne 0 ]; then
		# There was an error in executing the program or cleaning the stats
		ret_code=1 
	fi
	if ! diff <(echo "$first_stats") <(echo "$rest_stats") ; then
		echo "The execution of '$compiled' for execution arguments '$exec_arg' and '$i' weren't equivalent."
		ret_code=1 
	fi
done

exit $ret_code
