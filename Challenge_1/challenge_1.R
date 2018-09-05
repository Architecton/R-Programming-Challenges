# Instructions:
#
# Implement two algorithms that search for both the minimum and the maximum element in the list at the same time.
# Implement the search using:
#	- an iterative algorithm,
#	- a recursive algorithm (divide and conquer algorithm)
#
# Empirically compare run times for each algorithm for increasing list lengths.

# generate_vector: generate a random vector
generate_vector <- function(lower_bound, upper_bound, n) {
	return (c(sample(lower_bound:upper_bound, n, replace = TRUE, prob = NULL)))
}

# Implementation of the linear search for the minimum and maximum values.
linear_minmax <- function(vect) {

	# Assume minimum and maximum are first value in list.
	minimum <- vect[1]
	maximum <- vect[1]
	# Go over elements in vector and compare to current minimum/maximum.
	for (el in vect) {
		if (el < minimum) {
			minimum <- el
		}
		if (el > maximum) {
			maximum <- el
		}
	}
	# Make results vector.
	result <- c(minimum, maximum)
	# Name fileds in results vector.
	names(result) <- c("minimum", "maximum")
	# Return results vector.
	return(result)
}

dc_minmax <- function(vect) {
	if (length(vect) <= 2) {
		if(length(vect) == 1) { 										# If vector length is 1, both minimum and maximum are the same.
			result <- c(vect[1], vect[2]) 								# Make results named vector and return it.
			names(result) <- c("minimum", "maximum")
			return (result)
		} else {
			result <- c(min(vect[1], vect[2]), max(vect[1], vect[2])) 	# Else if vector length is 2, make a trivial comparison ad return named vector.
			names(result) <- c("minimum", "maximum")
			return (result)
		}
	# Recursive case: compute critical values in left and right list and compare.
	} else {
		midpoint <- (length(vector) %/% 2) + 1

		result_left <- dc_minmax(vect[1:midpoint])
		result_right <- dc_minmax(vect[midpoint + 1 : length(vect)])

		result <- (c( min(as.numeric(result_left["minimum"], as.numeric(result_left["minimum"]))), max(as.numeric(result_left["maximum"], as.numeric(result_left["maximum"])))))
		names(result) <- c("minimum", "maximum")
		return (result)
	}
}

# get_times: take starting vector length, incrementation step, number of steps to include and the search function to use
# and return matrix of measurements where the first column represents the vector lengths and the second column represents the
# run time. 
get_times <- function(starting_length, step, num_steps, num_reps, search_func) {

	# Preallocate data matrix
	measurements = matrix( , nrow = num_steps, ncol = 2)
	measurement_index = 1

	# Test algorithm performance over various vector lengths.
	for (n in seq(starting_length, starting_length + (step - 1) * num_steps, step)) {
		time_acc <- 0

		# Perform testing.
		for(rep in 1:num_reps) {
			# Generate test vector.
			test_vect <- generate_vector(-1e3, 1e3, n)
			# Start timer
			start_time <- as.numeric(format(Sys.time(), "%OS3")) * 1000
			# Run algorithm.
			search_func(test_vect)
			# End timer
			end_time <- as.numeric(format(Sys.time(), "%OS3")) * 1000
			# Add value to accumulator
			time_acc <- time_acc + (end_time - start_time)
		}

		# Make measurements vector.
		measurement <- c(n, time_acc / num_reps)
		# Add measurement to measurements matrix.
		measurements[measurement_index, ] <- measurement
		# Increment measurements index.
		measurement_index <- measurement_index + 1
	}

	# Return measurements matrix.
	return (measurements)
}

linear_results = get_times(1000, 1000, 100, 10, linear_minmax)
# dc_results = get_times(1000, 1000, 100, 10, dc_minmax)

# Plot data
plot(
	linear_results[,1],
	linear_results[,2],
	type='l',
	xlab='Vector Length',
	ylab='Run Time (ms)'
)