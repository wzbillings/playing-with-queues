# Function to simulate a Poisson process
simulate_poisson_process <- function(lambda, end_time) {
	current_time <- 0
	event_times <- c()
	while (current_time < end_time) {
		time_to_next_event <- rexp(1, rate = lambda)
		current_time <- current_time + time_to_next_event
		if (current_time < end_time) {
			event_times <- c(event_times, current_time)
		}
	}
	return(event_times)
}

# Parameters
arrival_rate <- 0.5  # Arrival rate of the Poisson process (events per time unit)
service_rate <- 0.7  # Service rate of the Poisson process (events per time unit)
end_time <- 100  # Time limit for the simulation

# Simulate arrival times
arrival_times <- simulate_poisson_process(arrival_rate, end_time)

# Initialize variables for the simulation
departure_times <- c()
current_time <- 0
next_departure_time <- NA
queue_length <- 0
all_events <- c()
event_types <- c()

# Process the arrivals and departures
i <- 1
while (current_time < end_time && (i <= length(arrival_times) || queue_length > 0)) {
	if (i <= length(arrival_times) && (is.na(next_departure_time) || arrival_times[i] < next_departure_time)) {
		# Process arrival
		current_time <- arrival_times[i]
		queue_length <- queue_length + 1
		all_events <- c(all_events, current_time)
		event_types <- c(event_types, "arrival")
		
		if (queue_length == 1) {
			# Generate next departure time if the server is free
			next_departure_time <- current_time + rexp(1, rate = service_rate)
		}
		i <- i + 1
	} else {
		# Process departure
		current_time <- next_departure_time
		queue_length <- queue_length - 1
		departure_times <- c(departure_times, current_time)
		all_events <- c(all_events, current_time)
		event_types <- c(event_types, "departure")
		
		if (queue_length > 0) {
			# Generate next departure time if there are customers in the queue
			next_departure_time <- current_time + rexp(1, rate = service_rate)
		} else {
			next_departure_time <- NA
		}
	}
}

# Plot results
data <- data.frame(time = all_events, event = event_types)
ggplot(data, aes(x = time, y = event, color = event)) +
	geom_point() +
	labs(title = "Arrival and Departure Events in a Queue", x = "Time", y = "Event") +
	theme_minimal()

# Print statistics
cat(sprintf("Total number of arrivals: %d\n", length(arrival_times)))
cat(sprintf("Total number of departures: %d\n", length(departure_times)))
