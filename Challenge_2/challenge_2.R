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

# parse_num: parse complex number from terminal and return parsed complex number
# THOROUGH INPUT CHECKING IS NOT IMPLEMENTED.
parse_num <- function() {
	valid <- FALSE
	while(!valid) {
		# Read input and remove whitespace. Replace 
		raw_in <- readLines("stdin", 1)
		trimmed <- gsub(" ", "", raw_in, fixed = TRUE)
		trimmed <- gsub("+", ",", trimmed, fixed = TRUE)
		trimmed <- gsub("-", ",-", trimmed, fixed = TRUE)
		trimmed <- gsub("i", "", trimmed, fixed = TRUE)

		components <- strsplit(trimmed, split=",", fixed=TRUE)[[1]]
		components <- as.numeric(components)
		if(length(components) != 2) {
			print("Could not parse input. Please try again.")
		} else {
			names(components) <- c("real", "imaginary")


			if (!is.na(components["real"]) && !is.na(components["imaginary"])) {
				valid <- TRUE
				return (complex(real = components["real"], imaginary = components["imaginary"]))
			} else {
				print("Could not parse input. Please try again.")		
			}
		}
	}
}


# addition: add two complex numbers and return result.
addition <- function() {
	# Read and parse first number.
	num1 <- parse_num() # TODO
	# Read and parse second number.
	num2 <- parse_num()

	# Return sum of parsed numbers.
	return (num1 + num2)
}

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

