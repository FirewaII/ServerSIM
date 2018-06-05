serverSim <- function(duration, ns, lambda, mu, fP, nP, simType, showResults = FALSE){
  # duration: Duration of the simulation
  # lambda: Queries arrival rate
  # mu: Queries service/departure rate
  # fP: Fast queries queue proportion 
  # nP: Normal queries queue proportion
  # nS: Number of servers
  # simType : End of simulation return setting (busy time or lost queries)
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
  
  N                 <- 150      # total query queue(s) capacity
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
  
  timeStep          <- 1
  timeState         <- 0
  mQState           <- 0
  fQState           <- 0
  nQState           <- 0
  sQState           <- 0
  
  if(nS < 1){
    return(cat("Le nombre de serveur ne peut pas être nul.\n"))
  }
  while (currentTime < endTime){
    # Query arrival
    if (nextArrival < nextDeparture || queue == 0){
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
          furthestDeparture = currentTime + rexp(1, mu)
          if (furthestDeparture > nextDeparture){
            nextDeparture = furthestDeparture
          }
          servicedQueries = servicedQueries - 1
        }
      } 
      
    }
    if (queue == 0) {
      # Queue is empty, no more queries to service, next service time is moved to the end of the simulation
      nextDeparture = endTime
      busyTime = busyTime + currentTime - lastBusyTime
    }
    
    if(debug){
      cat("busyTime : ", busyTime, "\n")
      cat("lastBusyTime: ", lastBusyTime, "\n")
      cat("Queue status: ", queue, "\n")
    }
    
    if(simType == "queue") { # Si on veut plotter
      timeState[timeStep] = currentTime
      mQState[timeStep] = queue # On combine les vecteurs (total)
      fQState[timeStep] = fQ
      nQState[timeStep] = nQ
      sQState[timeStep] = sQ
      timeStep = timeStep + 1
    }
  }
  busyTime = busyTime + currentTime - lastBusyTime
  
  if (nextDeparture>endTime){
    if (debug){
      print("[DEBUG] SERVICE TIME IS BEYOND THE END [DEBUG]")
    }
    totalDepartures = totalDepartures - 1
  }
  if (busyTime > endTime){
    busyTime = endTime
  }
  busyRate = round(busyTime/endTime, 4)*100
  if (showResults){
    cat('Nombre de requêtes reçues: ', totalArrivals, "\n\t", totalFA," prioritaires\n\t", totalNA," normales\n\t", totalSA," lentes\n")
    cat('Nombre de requêtes traitées: ', totalDepartures, "\n")
    cat('Nombre de requêtes perdues: ', totalArrivals - totalDepartures, "\n")
    # Remaining queries are considered lost queries since simulation ended too early
    cat('Nombre de requêtes restantes à la fin de la simulation: ', queue, "\n")
    cat('Taux d\'utilisation: ', busyRate, "%\n")
  }
  
  if (simType == "busy"){
    return(busyRate)
  } else if (simType == "lostQueries") {
    return(totalArrivals - totalDepartures)
  } else if (simType == "queue"){
    plotSeq = seq(1, timeStep-1)
    plot(timeState, mQState, xlab="temps", ylab="Nombre de requetes",type="s", main="Nombre de requetes dans la file d'attente") # On plote
    points(timeState, fQState, col="red", pch=NA_integer_)
    lines(timeState, fQState, col="red")
    points(timeState, nQState, col="blue", pch=NA_integer_)
    lines(timeState, nQState, col="blue")
    points(timeState, sQState, col="green", pch=NA_integer_)
    lines(timeState, sQState, col="green")
    legend("topleft",legend=c("total","prioritaires","normales","lentes"), col=c("black", "red","blue","green"),lty=c(1,1,1,1), ncol=1)
  }

}

cat("- STARTING SIMULATION -\n")

# Arguments to edit to customize the server simulation
duration = 10^4   # Duration of the simulation
lambda = 1        # Queries arrival rate
mu = 1            # Queries service/departure rate
fP = 0.1          # Fast queries queue proportion 
nP = 0.3          # Normal queries queue proportion
nS = 1            # Number of servers
nSim = 1:100      #



# Lost queries simulation
cat("1 Serveur ; mu = 1\n")

lostQueries = 0
lambda = 0.1
mu = 1
nS = 1
plotSeq = seq(0.1, 10, 0.1)
for (n in nSim){
  lostQueries[n] = serverSim(duration, ns, lambda, mu, fP, nP, "lostQueries")
  lambda = lambda + 0.1
}

plot(plotSeq, lostQueries, main=expression(paste("Perte des requêtes en fonction du taux d'arrivée pour 100 simulations ( ", mu, "=1 )")), xlab=expression(lambda) , ylab = "Nombre de pertes", las=1)

# Busy time/rate simulation
cat("n Serveurs ; lambda = 2 ; mu = 1\n")

nSim = 1:50
busyRate = 0
lambda = 2
mu = 1
nS = 1
plotSeq = seq(1, 50)
for (n in nSim){
  busyRate[n] = serverSim(duration, ns, lambda, mu, fP, nP, "busy")
  nS = nS + 1
}

plot(plotSeq, busyRate, main=expression(paste("Taux d'utilisation en fonction du nombre de serveurs pour 50 simulations ( ",lambda,"=2, ", mu, "=1 )")), xlab="Nombre de serveurs" , ylab = "Taux d'utilisation", las=1)

# Queue proportions simulation
duration = 10^2
lambda = 1
mu = 1
nS = 2
serverSim(duration, ns, lambda, mu, fP, nP, "queue")


# Normal simulation
duration = 10^4
lambda = 1
mu = 1
nS = 2
serverSim(duration, nS, lambda, mu, fP, nP, simType = "", showResults = TRUE)

cat("- SIMULATION ENDED -")





