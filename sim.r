serverSim <- function(lambda, mu){
  
  # DEBUG
  debug   <- FALSE
  
  # time vars
  t.end   <- 10^4   # duration of simulation
  t.clock <- 0      # simulation time
  
  # simulation parameters
  # we will be using exponential distribution
  queue             <- 0      # query queue
  N                 <- 70    # query queue capacity
  nextArrival       <- 0      # time for the next query arrival (reception)
  nextDeparture     <- t.end  # time for the next query departure (service)
  
  totalArrivals     <- 0      # total number of received queries
  totalDepartures   <- 0      # total number of serviced queries
  totalDropped      <- 0      # total number of dropped queries
  
  while (t.clock < t.end){
    # Query arrival
    if (nextArrival < nextDeparture || queue == 0){
      if (queue < N){
        if (debug){
          print("[DEBUG] NEW QUERY ADDED [DEBUG]")
        }
        queue = queue + 1
        totalArrivals = totalArrivals + 1
        t.clock = nextArrival        
        nextArrival = t.clock + rexp(1, lambda)
        if (queue == 1){
          # Only query in queue is directly serviced
          if (debug){
            print("[DEBUG] SERVICING ONLY QUERY IN QUEUE [DEBUG]")
          }
          nextDeparture = t.clock + rexp(1, mu)
        }
      } else {
        # server queue has reached its maximum capacity
        print("/!\ MAXIMUM CAPACITY REACHED /!\\")
        if (debug){
          print("[DEBUG] QUERY DROPPED [DEBUG]")
        }
        totalDropped = totalDropped + 1
      }
    } else {
      # Queue departure
      queue = queue - 1
      totalDepartures = totalDepartures + 1
      if (queue > 0){
        if (debug){
          print("[DEBUG] SERVICING QUERY [DEBUG]")
        }
        nextDeparture = t.clock + rexp(1, mu)
      } else {
        nextDeparture = t.end
      }
    }
  }

  cat('Nombre de requêtes reçues: ', totalArrivals, "\n")
  cat('Nombre de requêtes traitées: ', totalDepartures, "\n")
  cat('Nombre de requêtes perdues: ', totalDropped, "\n")
  cat('Nombre de requêtes restantes à la fin de la simulation: ', queue, "\n")

}

cat("- STARTING SIMULATION -\n")
serverSim(lambda = 3, mu = 2)
cat("- SIMULATION ENDED -")


