### SETUP STUFF -------------------------------------

# Load training data
load("./FitnessTrack.Rdata")

# Pre-computed column info
columns <- c(
  "time"=c(
    "units"="s",
    "units_full"="seconds",
    "title"="time (s)"
  ),
  "latitude"=c(
    "units"="degrees",
    "units_full"="degrees",
    "title"="latitude (degrees)"
  ),
  "longitude"=c(
    "units"="degrees",
    "units_full"="degrees",
    "title"="longitude (degrees)"
  ),
  "altitude"=c(
    "units"="m",
    "units_full"="metres",
    "title"="altitude (m)"
  ),
  "distance"=c(
    "units"="m",
    "units_full"="metres",
    "title"="distance (m)"
  ),
  "heart.rate"=c(
    "units"="bpm",
    "units_full"="beats per minute",
    "title"="heart.rate (bpm)"
  ),
  "speed"=c(
    "units"="m/s",
    "units_full"="metres per second",
    "title"="speed (m/s)"
  ),
  "cadence"=c(
    "units"="steps/min",
    "units_full"="steps per minute",
    "title"="cadence (steps/min)"
  ),
  "power"=c(
    "units"="W",
    "units_full"="watts",
    "title"="power (W)"
  ),
  "pace"=c(
    "units"="min/km",
    "units_full"="minutes per kilometre",
    "title"="pace (min/km)"
  )
)

# Put all sessions data into 'sessions' list
sessions <- list()
for(i in 1:27) {
  sessions[[i]] <- data.frame(time = attr(FitnessTrack[[i]],"index"))
  sessions[[i]]$latitude <- FitnessTrack[[i]][,"latitude"]
  sessions[[i]]$longitude <- FitnessTrack[[i]][,"longitude"]
  sessions[[i]]$altitude <- FitnessTrack[[i]][,"altitude"]
  sessions[[i]]$distance <- FitnessTrack[[i]][,"distance"]
  sessions[[i]]$heart.rate <- FitnessTrack[[i]][,"heart.rate"]
  sessions[[i]]$speed <- FitnessTrack[[i]][,"speed"]
  sessions[[i]]$cadence <- FitnessTrack[[i]][,"cadence"]
  sessions[[i]]$power <- FitnessTrack[[i]][,"power"]
  sessions[[i]]$pace <- FitnessTrack[[i]][,"pace"]
}

### EASY TO USE FUNCTIONS -------------------------------------

plot_session <- function(session_index, x_property, y_property, col="BLACK", ylim=NULL) {
  s  <- sessions[[session_index]]
  x  <- getElement(s, x_property)
  y  <- getElement(s, y_property)
  xl <- columns[paste(x_property, ".title", sep="")]
  yl <- columns[paste(y_property, ".title", sep="")]
  main <- paste("Session ", session_index, ": ", y_property, ", ", x_property, sep="")
  
  if (is.null(ylim)) { ylim = c(min(y), max(y)) }
  
  plot(xlab=xl, x=x, ylab=yl, y=y, ylim=ylim, col=col, main=main, type='l')
}

plot_session_a <- function(session_index, x_property, y_property, col="BLACK") {
  s  <- sessions[[session_index]]
  x  <- getElement(s, x_property)
  y  <- getElement(s, y_property)
  
  lines(x=x, y=y, col=col)
}

sessions_plot_two_properties <- function(property1, property2, against="time", col1="RED", col2="BLUE", range=c(1,3)) {
  op <- par(mfrow = c(range[2], 2), mar = .1 + c(2,2,3,1))
  for (i in range[1]:range[2]) {
    s <- sessions[[i]]
    s_against <- getElement(s, against)
    s_prop1   <- getElement(s, property1)
    s_prop2   <- getElement(s, property2)
    
    #xlim <- c( min(s_time,na.rm=TRUE), max(s_time,na.rm=TRUE) )
    #ylim <- c( min(min(s_latitude,na.rm=TRUE), min(s_longitude,na.rm=TRUE)),
    #           max(max(s_latitude,na.rm=TRUE), max(s_longitude,na.rm=TRUE)) )
    
    xlab <- columns[paste(against, ".title", sep="")]
    ylab1 <- columns[paste(property1, ".title", sep="")]
    ylab2 <- columns[paste(property2, ".title", sep="")]
    main1 <- paste("Session ", i, ": ", property1, " against ", against, sep="")
    main2 <- paste("Session ", i, ": ", property2, " against ", against, sep="")
    
    plot(x=s_against, y=s_prop1, main=main1, xlab=xlab, ylab=ylab1, type='l', col=col1)
    plot(x=s_against, y=s_prop2, main=main2, xlab=xlab, ylab=ylab2, type='l', col=col2)
  }
  par(op)
}

### INSTRUCTIONS ----------------------------

# The list 'sessions' holds the session data for every training session
# The ith training session is accessible by doing 'sessions[[INDEX]]' where index is from 1 to 27
# The training sessions have the following columns:
#     time     (this is the data index in the .rdata file)
#     latitude
#     longitude
#     altitude
#     distance
#     heart.rate
#     speed
#     cadence
#     power
#     pace
# Each row in a training session is a different point in time, i.e. the data is just many consecutive readings
#  of various properties (the columns listed above)
# A column is accessible 'sessions[[INDEX]]$COLUMN_NAME'
# You can access the units and a human-readable axis title of a column via 'column["COLUMN_NAME.units"]'
#  and 'column["COLUMN_NAME.title"]' respectively.


### FUNCTIONS ---


# plot_session(session_index, x_property, y_property, col="BLACK", ylim=NULL)
#   > Plots the x_property against y_proprty (both strings, e.g. "altitude") for specified session

# plot_session_a(session_index, x_property, y_property, col="BLACK")
#   > Adds to last plot the x_property against y_proprty for specified session

# sessions_plot_two_properties(property1, property2, against="time", col1="RED", col2="BLUE", range=c(1,3))
#   > Plots two properties against another for a range of training sessions


### YOUR CODE -------------------------------


###
### Plot latitude and longitude against time for the range of sessions specified as 'range' below
###

range <- c(1, 3)

sessions_plot_two_properties("latitude", "longitude", against="time", range=range)

###
### Plot speed and heart.rate against time for the range of sessions specified as 'range' below
###

# Notes:
# When they stop moving, the heart rate reading vanishes!

range <- c(1,3)  # Range of sessions you wish to draw: '1,3' will draw sessions 1, 2 and 3

sessions_plot_two_properties("speed", "heart.rate", against="time", range=range)

###



