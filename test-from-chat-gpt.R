# Load necessary libraries
library(ggplot2)
library(MASS)

# Function to generate batch size based on time of day
generate_batch_size <- function(time_of_day, max_batch_size, min_batch_size) {
	# Sinusoidal pattern peaking at midday (assumes time_of_day is in hours [0, 24))
	peak_time <- 12
	#batch_size <- round(min_batch_size + (max_batch_size - min_batch_size) * (1 + sin(pi * (time_of_day - peak_time) / 12)) / 2)
	batch_size <- round(min_batch_size + (max_batch_size - min_batch_size) * ifelse(time_of_day >= 6 & time_of_day <= 20, cos(1.16 * pi * (1.30 * time_of_day - 24)/24 + 1.04) - 0.09, 0))
	return(batch_size)
}

simulate_mx_md_ck_queue_with_cyclical_batches_and_correlated_service <- function(lambda_rate, service_time_meanlog, service_time_sdlog, service_correlation, num_batches, max_batch_size, min_batch_size, balking_time_meanlog, balking_time_sdlog, balking_correlation, num_servers, buffer_size) {
	# Generate batch interarrival times
	batch_interarrival_times <- rexp(num_batches, rate = lambda_rate)
	batch_arrival_times <- cumsum(batch_interarrival_times)
	
	# Generate batch sizes based on time of day
	batch_sizes <- sapply(batch_arrival_times %% 24, generate_batch_size, max_batch_size = max_batch_size, min_batch_size = min_batch_size)
	
	# Generate individual interarrival times within batches
	individual_arrival_times <- unlist(mapply(function(t, s) rep(t, s), batch_arrival_times, batch_sizes))
	
	# Total number of individuals
	total_individuals <- sum(batch_sizes)
	
	# Generate correlated log-normal service times for each server
	service_cov_matrix <- matrix(service_correlation * service_time_sdlog^2, nrow = num_servers, ncol = num_servers)
	diag(service_cov_matrix) <- service_time_sdlog^2
	log_service_times <- mvrnorm(total_individuals, mu = rep(service_time_meanlog, num_servers), Sigma = service_cov_matrix)
	service_times <- exp(log_service_times)
	
	# Generate correlated log-normal balking times for each batch
	log_balking_times <- unlist(lapply(batch_sizes, function(size) {
		balking_cov_matrix <- matrix(balking_correlation * balking_time_sdlog^2, nrow = size, ncol = size)
		diag(balking_cov_matrix) <- balking_time_sdlog^2
		exp(mvrnorm(1, mu = rep(balking_time_meanlog, size), Sigma = balking_cov_matrix))
	}))
	
	# Initialize vectors to store results
	start_service_times <- numeric(total_individuals)
	departure_times <- numeric(total_individuals)
	waiting_times <- numeric(total_individuals)
	balked_customers <- 0
	dropped_customers <- 0
	current_buffer <- 0
	
	# Initialize a vector to keep track of server availability
	server_end_times <- rep(0, num_servers)
	
	# Initialize statistics
	customers_served <- 0
	current_queue_length <- 0
	total_arrivals <- 0
	total_departures <- 0
	
	for (i in 1:total_individuals) {
		total_arrivals <- total_arrivals + 1
		
		# Check if the buffer is full
		if (current_buffer >= num_servers + buffer_size) {
			waiting_times[i] <- NA  # Customer is dropped, so we set waiting time to NA
			dropped_customers <- dropped_customers + 1
			next
		}
		
		# Assign the customer to the earliest available server
		next_available_server <- which.min(server_end_times)
		
		# Determine the start service time
		if (i == 1) {
			start_service_times[i] <- individual_arrival_times[i]
		} else {
			start_service_times[i] <- max(individual_arrival_times[i], server_end_times[next_available_server])
		}
		current_waiting_time <- start_service_times[i] - individual_arrival_times[i]
		
		# Check if the customer will balk
		if (current_waiting_time <= log_balking_times[i]) {
			service_time <- service_times[i, next_available_server]
			departure_times[i] <- start_service_times[i] + service_time
			waiting_times[i] <- current_waiting_time
			server_end_times[next_available_server] <- departure_times[i]
			customers_served <- customers_served + 1
			current_buffer <- max(0, current_buffer - 1)  # Reduce buffer as a customer starts service
			total_departures <- total_departures + 1
		} else {
			waiting_times[i] <- NA  # Customer balks, so we set waiting time to NA
			balked_customers <- balked_customers + 1
			next
		}
		
		# Increase buffer as a customer joins the queue
		current_buffer <- current_buffer + 1
		current_queue_length <- current_buffer
	}
	
	# Remove balked and dropped customers from the results
	valid_indices <- !is.na(waiting_times)
	waiting_times <- waiting_times[valid_indices]
	individual_arrival_times <- individual_arrival_times[valid_indices]
	departure_times <- departure_times[valid_indices]
	
	return(list(waiting_times = waiting_times, arrival_times = individual_arrival_times, departure_times = departure_times, balked_customers = balked_customers, dropped_customers = dropped_customers, customers_served = customers_served, current_queue_length = current_queue_length, total_arrivals = total_arrivals, total_departures = total_departures))
}

# Parameters
{
set.seed(12839123)
lambda_rate <- 0.3  # Arrival rate for batches (batches per time unit)
service_time_meanlog <- 0.5  # Mean of log of service time
service_time_sdlog <- 0.2  # Standard deviation of log of service time
service_correlation <- 0.5  # Correlation between service times for different servers
num_batches <- 500  # Number of batches to simulate
max_batch_size <- 25  # Maximum batch size
min_batch_size <- 1  # Minimum batch size
balking_time_meanlog <- 2  # Mean of log of balking time
balking_time_sdlog <- 0.5  # Standard deviation of log of balking time
balking_correlation <- 0.7  # Correlation between balking times within a batch
num_servers <- 1  # Number of servers
buffer_size <- 3  # Buffer size

# Run simulation
results <- simulate_mx_md_ck_queue_with_cyclical_batches_and_correlated_service(lambda_rate, service_time_meanlog, service_time_sdlog, service_correlation, num_batches, max_batch_size, min_batch_size, balking_time_meanlog, balking_time_sdlog, balking_correlation, num_servers, buffer_size)
waiting_times <- results$waiting_times
arrival_times <- results$arrival_times
departure_times <- results$departure_times
balked_customers <- results$balked_customers
dropped_customers <- results$dropped_customers
customers_served <- results$customers_served
current_queue_length <- results$current_queue_length
total_arrivals <- results$total_arrivals
total_departures <- results$total_departures

# Plot results
data <- data.frame(arrival_times = arrival_times, waiting_times = waiting_times)
ggplot(data, aes(x = arrival_times %% 24, y = waiting_times)) +
	geom_point() +
	labs(title = "Waiting Times in an M^X/D/c/K Queue with Cyclical Batch Sizes and Correlated Balking and Service Times", x = "Arrival Time", y = "Waiting Time") +
	theme_minimal()


ggplot(data, aes(x = waiting_times)) +
	geom_histogram(
		binwidth = 0.25,
		center = 0,
		aes(y = after_stat(density))
	) +
	labs(
		x = "Waiting time",
		y = "Density",
		title = "Waiting Times in an M^X/D/c/K Queue with Cyclical Batch Sizes and Correlated Balking and Service Times"
	) +
	theme_minimal()

# Print average waiting time and additional statistics
cat(sprintf("Average waiting time: %.4f\n", mean(waiting_times, na.rm = TRUE)))
cat(sprintf("Number of balked customers: %d\n", balked_customers))
cat(sprintf("Number of dropped customers: %d\n", dropped_customers))
cat(sprintf("Total number of customers served: %d\n", customers_served))
cat(sprintf("Current number of customers in the queue: %d\n", current_queue_length))
cat(sprintf("Total number of arrivals: %d\n", total_arrivals))
cat(sprintf("Total number of departures: %d\n", total_departures))
}
# Test lambda(t) function
