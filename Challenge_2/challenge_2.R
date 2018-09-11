# Implement a calculator for working with complex numbers. It should support addition, subtraction, multiplication, division and computing the n-th primitive root of unity.

# Define the following functions:

 # add        // adding complex numbers
 # multiply   // multiplying complex numbers
 # multiply   // multiplying complex numbers with real numbers
 # divide     // division of complex numbers
 # n_root     // computing the n-th primitive root of unity

# The calculator should support the commands:

# + (addition),
# - (subtraction),
# * (multiplication),
# / (division),
# w (printing the n-th primitive root and all its powers).
# in case of command +, -, *, / parse two complex numbers after the command. In case of the command w, parse an integer n. The execution of the program should stop when entering a blank line.
# The results should be rounded to 5 decimal places.
#####################################################################################################

# Define variable that stores the line entered by user (set initial value)
next_line <- " "

# Define vector of valid commands and name them.
commands = c("+", "-", "*", "/", "w")
names(commands) <- c("plus", "minus", "times", "div", "pru")

# execute_command: execute functionality specified by command argument.
execute_command <- function(command) {
	# Switch on command.
	switch(command,
		plus = return (addition())
		# minus = print("-"),
		# times = print("*"),
		# div = print("/"),
		# pru = print("w")
	)
}

# Complex Number Parsing ################################################################################################################

# parse_num: parse complex number from terminal and return parsed complex number
# THOROUGH INPUT CHECKING IS NOT IMPLEMENTED.
parse_num <- function() {
	# Parse raw user input.
	raw_in <- readLines("stdin", 1)

	# Remove all whitespace
	trimmed_in <- gsub(" ", "", raw_in, fixed = TRUE)

	# Define starting sign and assign initial value.
	start_sign <- ""

	# Check if number starts with sign.
	if(substr(trimmed_in, 1, 1) == "+" || substr(trimmed_in, 1, 1) == "-") {
		# The sign of real part is the first character in trimmed input.
		start_sign <- substr(trimmed_in, 1, 1)
		# Start parsing magnitude from second position (after sign).
		start_re <- 2
		# End parsing real part magnitude just before sign of imaginary part.
		end_re <- regexpr("[+-]", substr(trimmed_in, 2, nchar(trimmed_in)))[1]
		# Start parsing imaginary part from second sign.
		start_im <- end_re + 1
		# End parsing imaginary part magnitude just before 'i'.
		end_im <- nchar(trimmed_in) - 1

	} else {
	# Else number does not start with a sign (implicitly positive)
		start_sign <- "+"
		# Start parsing from beginning.
		start_re <- 1
		# Similar as in if bloc.
		end_re <- regexpr("[+-]", trimmed_in)[1] - 1
		start_im <- end_re + 1
		end_im <- nchar(trimmed_in) - 1
	}

	# Get components of parsed complex number and name them.
	components <- c(paste(start_sign, substr(trimmed_in, start_re, end_re), sep = ""), substr(trimmed_in, start_im, end_im))
	components <- as.numeric(components)
	names(components) <- c("real", "imaginary")

	# Return parsed complex number.
	return(complex(real = components["real"], imaginary = components["imaginary"]))
}

#########################################################################################################################################

# Calculator arithmetic functionality implementation ####################################################################################

# addition: add two complex numbers and return result.
addition <- function() {
	# Read and parse first number.
	num1 <- parse_num() # TODO
	# Read and parse second number.
	num2 <- parse_num()

	# Return sum of parsed numbers.
	return (num1 + num2)
}

#########################################################################################################################################

# The main loop #########################################################################################################################

# While user does not enter a blank line...
while (next_line != "") {

	# Prompt user to enter command and read entered value.
	print("Enter command (+, -, *, /, w):")
	next_line <- readLines("stdin", 1)
	# Remove leading and trailing whitespace
	next_line = trimws(next_line, "both")


	# Check for validity of command.
	if(next_line %in% commands) {
		# Get result.
		result <- execute_command(names(which(commands == next_line)))
		print(result)

	} else if (next_line == "") {
		# Do nothing - quit program
	} else {
		print("Unknown command")
	}
}

#########################################################################################################################################