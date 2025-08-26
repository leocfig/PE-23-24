# Exercicio 4 do Projeto de PE

# Dúvidas - era a probabilidade condicionada que o exercício queria!!!

# Sets the asked values
reps <- 150
seed <- 2255
nr_circuts <- 9
circuit_signals <- vector(length = nr_circuts)
warning_sound <- 2 
system_off <- 1    
set.seed(seed)

signal_emitted <- function() {
  return(sample(1:10, 1, prob=(1:10)/55))
}


# Function that checks which circuits the warning sound was produced and the system was not turned off
system_simulation_intersection <- function() {
  
  # Emits a signal for each circuit
  for(i in 1:nr_circuts) {
    circuit_signals[i] <- signal_emitted()
  }
  
  # A warning sound was produced in a system that was not turned off
  if (warning_sound %in% circuit_signals & !(system_off %in% circuit_signals)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


# Function that checks which circuits the warning sound was produced and the system was not turned off
system_simulation_not_off <- function() {
  
  # Emits a signal for each circuit
  for(i in 1:nr_circuts) {
    circuit_signals[i] <- signal_emitted()
  }
  
  # A system was not turned off
  if (!(system_off %in% circuit_signals)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# Repeats the process the number of requested repetitions
results_intersection <- replicate(reps, system_simulation_intersection())

results_not_off <- replicate(reps, system_simulation_not_off())

# Calculates the number of "TRUE" values, indicating a warning sound was produced
# in a system that is not turned off, and divides it by the total number of simulations
intersection <- mean(results_intersection)

not_off <- mean(results_not_off)

proportion <- intersection / not_off

print(proportion)


