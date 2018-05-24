serverSim <- function(duration, ns, lambda, mu, fP, nP){
  # lambda: Queries arrival rate
  # mu: Queries service/departure rate
  # fP: Fast queries queue proportion 
  # nP: Normal queries queue proportion
  # nS: Number of servers

  # proportion of slow queries is 1 - (fP + nP)
  
  # DEBUG
  debug   <- FALSE
  
  # time vars
  endTime   = duration   # duration of simulation
  currentTime <- 0      # simulation time
  
  # simulation parameters
  # we will be using an exponential distribution for incoming and departing queries
  queue             <- 0        # query queue
  fQ                <- 0        # fast queue
  nQ                <- 0        # normal queue
  sQ                <- 0        # slow queue

  N                 <- 100      # total query queue(s) capacity
  nextArrival       <- 0        # time for the next query arrival (reception)
  nextDeparture     <- endTime  # time for the next query departure (service)
  
  totalArrivals     <- 0        # total number of received queries
  totalFA           <- 0        # total number of fast received queries
  totalNA           <- 0        # total number of normal received queries
  totalSA           <- 0        # total number of slow received queries
  
  totalDepartures   <- 0        # total number of serviced queries
  totalFD           <- 0        # total number of fast serviced queries
  totalND           <- 0        # total number of normal serviced queries
  totalSD           <- 0        # total number of slow serviced queries
  
  busyTime          <- 0        # total time servicing queries

  if(nS < 1){
    return(cat("Le nombre de serveur ne peut pas être nul.\n"))
  }
  while (currentTime < endTime){
    # Query arrival
    if (queue < N && nextArrival < nextDeparture || queue == 0){
        if (debug){
          print("[DEBUG] NEW QUERY ADDED [DEBUG]")
        }
        # Determining query type using proportions
        queryType = runif(1)
        if (queryType < fP){
          fQ = fQ + 1
          totalFA = totalFA + 1
        } else if (queryType < fP + nP){
          nQ = nQ + 1
          totalNA = totalNA + 1
        } else {
          sQ = sQ + 1
          totalSA = totalSA + 1
        }
        # Adding a new query, regardless of its type, and calculating its arrival time
        queue = fQ + nQ + sQ
        totalArrivals = totalArrivals + 1
        currentTime = nextArrival    
        # Predermining the next query's arrival time    
        nextArrival = currentTime + rexp(1, 1/lambda)
        if (queue == 1){
          # Only query in queue is directly serviced
          if (debug){
            print("[DEBUG] SERVICING ONLY QUERY IN QUEUE [DEBUG]")
          }
          nextDeparture = currentTime + rexp(1, 1/mu)
          lastBusyTime = currentTime
        }
    } else {
      # Queue departure
      # As long as there are more prioritised queries, those will be serviced before moving to the slow queue
      servicedQueries = 1
      while (servicedQueries <= nS && queue > 0){
        # Servicing queries as long as there are any in queue and as many as the servers can handle (number of servers)
        if (fQ > 0){
          fQ = fQ - 1
        } else if (nQ > 0){
          nQ = nQ - 1
        } else {
          sQ = sQ - 1
        }
        queue = fQ + nQ + sQ
        currentTime = nextDeparture
        totalDepartures = totalDepartures + 1
        servicedQueries = servicedQueries + 1
      }
      if (queue > 0 || servicedQueries > 0){
        if (debug){
          print("[DEBUG] SERVICING QUERY [DEBUG]")
        }
        while (servicedQueries > 0 && queue > 0){
          nextDeparture = currentTime + rexp(1, 1/mu)
          servicedQueries = servicedQueries - 1
        }
      } 
      if (queue == 0) {
        # Queue is empty, no more queries to service, next service time is moved to the end of the simulation
        nextDeparture = endTime
        busyTime = busyTime + currentTime - lastBusyTime
      }
    }
    if(debug){
      cat("busyTime : ", busyTime, "\n")
      cat("lastBusyTime: ", lastBusyTime, "\n")
      cat("Queue status: ", queue, "\n")
    }
  }

  #if (busyTime == 0){
  #  busyTime = endTime
  #}
  cat('Nombre de requêtes reçues: ', totalArrivals, "\n\t", totalFA," prioritaires\n\t", totalNA," normales\n\t", totalSA," lentes\n")
  cat('Nombre de requêtes traitées: ', totalDepartures, "\n")
  cat('Nombre de requêtes perdues: ', totalArrivals - totalDepartures, "\n")
  # Remaining queries are considered lost queries since simulation ended too early
  cat('Nombre de requêtes restantes à la fin de la simulation: ', queue, "\n")
  cat('Taux d\'utilisation: ', round(busyTime/endTime, 4)*100, "%\n")
}

cat("- STARTING SIMULATION -\n")

# Arguments to edit to customize the server simulation
duration = 10^4
lambda = 1
mu = 0.8
fP = 0.1
nP = 0.3
nS = 3

serverSim(duration, ns, lambda, mu, fP, nP)

cat("- SIMULATION ENDED -")


