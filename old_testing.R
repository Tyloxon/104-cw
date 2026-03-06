load("./FitnessTrack.Rdata")

# Get name of first column in data
attr(FitnessTrack,"units")[1,1]

# Get units of first column in data
attr(FitnessTrack,"units")[1,2]

# Get axis title for first column
paste(attr(FitnessTrack,"units")[1,1], " (", attr(FitnessTrack,"units")[1,2], ")", sep="")

# Get summaries of the individual data columns in the first training session
summary(FitnessTrack[[1]])

# Put first session's data into session
session <- data.frame(timestamp = attr(FitnessTrack[[1]],"index"))
session$latitude <- FitnessTrack[[1]][,"latitude"]
session$longitude <- FitnessTrack[[1]][,"longitude"]
session$altitude <- FitnessTrack[[1]][,"altitude"]
session$distance <- FitnessTrack[[1]][,"distance"]
session$heart.rate <- FitnessTrack[[1]][,"heart.rate"]
session$speed <- FitnessTrack[[1]][,"speed"]
session$cadence <- FitnessTrack[[1]][,"cadence"]
session$power <- FitnessTrack[[1]][,"power"]
session$pace <- FitnessTrack[[1]][,"pace"]

session

# Put all sessions data into 'sessions' list
sessions <- list()
for(i in 1:1) {
  sessions[[i]] <- data.frame(timestamp = attr(FitnessTrack[[i]],"index"))
  sessions[[i]]$latitude <- FitnessTrack[[i]][,"latitude"]
  sessions[[i]]$longitude <- FitnessTrack[[i]][,"longitude"]
  sessions[[i]]$altitude <- FitnessTrack[[i]][,"altitude"]
  sessions[[i]]$distance <- FitnessTrack[[i]][,"distance"]
  sessions[[i]]$heartrate <- FitnessTrack[[i]][,"heart.rate"]
  sessions[[i]]$speed <- FitnessTrack[[i]][,"speed"]
  sessions[[i]]$cadence <- FitnessTrack[[i]][,"cadence"]
  sessions[[i]]$power <- FitnessTrack[[i]][,"power"]
  sessions[[i]]$pace <- FitnessTrack[[i]][,"pace"]
}

# Store column names, units and titles (axis heading, e.g. "latitude (degree)") in column_info
column_info <- append(data.frame(name="timestamp", units="s"), data.frame(name = attr(FitnessTrack,"units")[,1], units = attr(FitnessTrack,"units")[,2]))
column_info$title <- append("timestamp (s)", paste(column_info$name, " (", column_info$units, ")", sep=""))

column_info$name[1]
column_info$units[1]
column_info$title[1]

column_info$title

axis_titles <- split(column_info$title, f=as.factor(column_info$title))
names(axis_titles) <- column_info$name

axis_titles["latitude"]

# Pre-computed column info
columns <- c(
  "timestamp"=c(
    "units"="s",
    "units_full"="seconds",
    "title"="timestamp (s)"
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
  "heartrate"=c(
    "units"="bpm",
    "units_full"="beats per minute",
    "title"="heartrate (bpm)"
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

print(columns["timestamp.title"])

# Example plotting some data
plot(xlab=columns["timestamp.title"],
     x=sessions[[1]]$timestamp,
     ylab=columns["distance.title"],
     y=sessions[[1]]$distance,
     type='l')

data_plot <- function(session_index, x_property, y_property) {
  s  <- sessions[[session_index]]
  x  <- getElement(s, x_property)  # TODO: HOW TO DO DIS??
  y  <- getElement(s, y_property)  # TODO: HOW TO DO DIS??
  xl <- columns[paste(x_property, ".title", sep="")]
  yl <- columns[paste(y_property, ".title", sep="")]
  
  plot(xlab=xl, x=x, ylab=yl, y=y, type='l')
}

data_plot(1, "latitude", "longitude")
data_plot(2, "latitude", "longitude")
data_plot(3, "latitude", "longitude")
data_plot(4, "latitude", "longitude")
data_plot(5, "latitude", "longitude")

print(paste(columntail(sessions[[1]]$distance, n=1)))

