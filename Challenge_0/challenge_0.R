# Instructions:
#
# Write a program that empirically compares two search algorithms for
# sorted arrays:
#	- linear search
#	- binary search
#
# Run both algorithms multiple times for different array sizes and measure
# the average running time.

# Function that generates a vector of incrementing integers of length vector_length
generate_vector <- function(vector_length) {
	return (1:vector_length)
}

# Implementation of the linear search algorithm
linear_search <- function(el, vect) {
	for (i in 1:length(vect)) { 		# Go over elements in vector
		if (vect[i] == el) { 			# If element matches...
			return (i) 					# ...return its index.
		}
	}
	return (-1) 						# If element not found return signal value.
}

# Implementation of the binary search algorithms
binary_search <- function(el, vect) {
	# Auxiliary function that takes two additional parameters needed for recursion.
	binary_search_aux <- function(el, vect, l, r) {
		if (l <= r) {
			# Compute index of midpoint of vector.
			mid = l + (r - l) %/% 2 + 1
			# Compare midpoint element to sought element.
			if (vect[mid] == el) {
				return (mid) 		# If it matches, return its index.
			}

			# Search in appropriate half of the vector if midpoint does not match.
			if (vect[mid] > el) {
				return (binary_search_aux(el, vect, l, mid - 1))
			} else {
				return (binary_search_aux(el, vect, mid + 1, r))
			}
		}
		return (-1) 	# If element not found, return signal value -1.
	}
	# Make call of auxiliary function and return its result.
	return (binary_search_aux(el, vect, 0, length(vect)))
}

# get_times: take starting vector length, incrementation step, number of steps to include and the search function to use
# and return matrix of measurements where the first column represents the vector lengths and the second column represents the
# run time.
get_times <- function(starting_length, step, num_steps, num_reps, search_func) {
	
	# Preallocate data matrix
	measurements = matrix( , nrow = num_steps, ncol = 2)
	measurement_index = 1

	# Go over applicable vector lengths.
	for (n in seq(starting_length, starting_length + (step - 1) * num_steps, step)) {
		
		# Generate test vector
		test_vect = generate_vector(n)

		# Start timer
		start_time = as.numeric(format(Sys.time(), "%OS3")) * 1000

		# Run the algorithm num_reps times.
		for (rep in 1:num_reps) {
			to_find = sample(1:n, 1)
			search_func(to_find, test_vect)
		}

		# Stop timer.
		end_time = as.numeric(format(Sys.time(), "%OS3")) * 1000

		# Make a vector representing a measurement.
		measurement = c(n, (end_time - start_time) / num_reps)

		# Add measurement to matrix of measurements
		measurements[measurement_index, ] = measurement

		# Increment index of current measurement.
		measurement_index = measurement_index + 1
	}

	# Return matrix of measurements.
	return (measurements)
}

# Get measurements matrices for both algorithms.
linear_search_times <- get_times(1000, 1000, 100, 10, linear_search)
# binary_search_times <- get_times(1000, 1000, 10, 10, binary_search)


# Plot data
plot(
	linear_search_times[,1],
	linear_search_times[,2],
	type='l',
	xlab='Vector Length',
	ylab='Run Time'
)