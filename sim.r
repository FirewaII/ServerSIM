serverSim <- function(lambda, mu, fP, nP){
  # lambda: Queries arrival rate
  # mu: Queries service/departure rate
  # fP: Fast queries queue proportion 
  # nP: Normal queries queue proportion
  # proportion of slow queries is the rest of 1 - (fP + nP)
  
  # DEBUG
  debug   <- FALSE
  
  # time vars
  endTime   <- 10^4   # duration of simulation
  currentTime <- 0      # simulation time
  
  # simulation parameters
  # we will be using exponential distribution
  queue             <- 0        # query queue
  fQ                <- 0        # fast queue
  nQ                <- 0        # normal queue
  sQ                <- 0        # slow queue

  N                 <- 100      # total query queue(s) capacity
  nextArrival       <- 0        # time for the next query arrival (reception)
  nextDeparture     <- endTime  # time for the next query departure (service)
  
  totalArrivals     <- 0      # total number of received queries
  totalFA           <- 0      # total number of fast received queries
  totalNA           <- 0      # total number of normal received queries
  totalSA           <- 0      # total number of slow received queries

  totalDepartures   <- 0      # total number of serviced queries
  totalFD           <- 0      # total number of fast serviced queries
  totalND           <- 0      # total number of normal serviced queries
  totalSD           <- 0      # total number of slow serviced queries

  busyTime          <- 0      # total time servicing queries

  while (currentTime < endTime){
    # Query arrival
    if (queue < N && nextArrival < nextDeparture || queue == 0){
        if (debug){
          print("[DEBUG] NEW QUERY ADDED [DEBUG]")
        }
        queryType = runif(1)
        print(queryType)
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
        queue = fQ + nQ + sQ
        totalArrivals = totalArrivals + 1
        currentTime = nextArrival        
        nextArrival = currentTime + rexp(1, lambda)
        if (queue == 1){
          # Only query in queue is directly serviced
          if (debug){
            print("[DEBUG] SERVICING ONLY QUERY IN QUEUE [DEBUG]")
          }
          nextDeparture = currentTime + rexp(1, mu)
          lastBusyTime = currentTime
        }
    } else {
      # Queue departure
      # queue = queue - 1
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
      if (queue > 0){
        if (debug){
          print("[DEBUG] SERVICING QUERY [DEBUG]")
        }
        nextDeparture = currentTime + rexp(1, mu)
      } else {
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

  cat('Nombre de requêtes reçues: ', totalArrivals, "\n\t", totalFA," prioritaires\n\t", totalNA," normales\n\t", totalSA," lentes\n")
  cat('Nombre de requêtes traitées: ', totalDepartures, "\n")
  cat('Nombre de requêtes perdues: ', totalArrivals - totalDepartures, "\n")
  cat('Nombre de requêtes restantes à la fin de la simulation: ', queue, "\n")
  cat('Taux d\'utilisation: ', round(busyTime/endTime, 4)*100, "%\n")
}

cat("- STARTING SIMULATION -\n")
serverSim(lambda = 0.4, mu = 0.4, fP = 0.2, nP = 0.3)
cat("- SIMULATION ENDED -")


