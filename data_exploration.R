
library(fields)

create.data.directory = function() {
  if (!dir.exists("data")) {
    dir.create("data")
  }
}


download.storm.event.data = function(meta.data) {

  data.source = meta.data$source
  date.range = meta.data$date.range
  file.prefix = meta.data$file.prefix
  alt.file.prefix = meta.data$alt.file.prefix
  file.posfix = meta.data$file.posfix
  
  create.data.directory()
  
  for (date in meta.data$date.range) {
    file.name = paste0("data/", file.prefix, date, file.posfix)
    if (file.exists(file.name)) {
      print(paste0("File: ", file.name, " already exists. Skipping"))
    } else {
      prefix = ifelse(date < 2010, file.prefix, alt.file.prefix)
      download.url = paste0(data.source, prefix, date, file.posfix)
      print(paste0("Downloading: ", download.url, " to the data directory"))
      download.file(download.url, file.name)
    }
  }
}


fix.lon.lat.values = function(data) {
  # First we fix the "off by ten error"
  # There are many longitude values that are < -600
  # After taking a look all of these values are off by a factor of ten
  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x < -200, x/10, x))
  
  #Some of the data is off by a factor of ten the other way
  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x > -10, x*10, x))
  
  # Now we need to fix the positive/negative issue. 
  # Many points have positive longitude values when they should be negative
  
  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x > 0, -x, x))
  
  # More off by 10 issues
  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x > -18, x*10, x))

  
  # Fix off by 10 error in latitude
  data[c("BEGIN_LAT", "END_LAT")] = lapply(data[c("BEGIN_LAT", "END_LAT")], 
                                           function(x) ifelse(abs(x) > 180, x/10, x))
  
  # WARNING: There is more data cleaning to do!
  # Since we know the state of each event, I would like to plot by state
  # And see if all points are in that state. Sanity check
  
  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x < -200, x/10, x))
  
  return(data)
}

load.storm.data = function(meta.data) {
  file.prefix = meta.data$file.prefix
  file.posfix = meta.data$file.posfix
  
  all.data = data.frame()
  for (date in meta.data$date.range) {
    file.name = paste0("data/", file.prefix, date, file.posfix)
    print(paste0("Loading data: ", file.name))
    data = read.csv(file.name)
    all.data = rbind(all.data, data)
  }
  all.data
}

begin.lat.lon.not.null = function(data) {
  data[!is.na(data$BEGIN_LAT),]
} 

end.lat.lon.not.null = function(data) {
  data[!is.na(data$END_LAT),]
}

plot.by.category = function(category, data, state.to.color=NULL) {
  if (is.null(category)) {
    category.data = data
    category = "All Events"
  } else {
    if (!(category %in% unique(data$EVENT_TYPE))) {
      print(paste0("Category: ", category, "is not a vaild event type")) 
      return()
    }
    category.data = data[data["EVENT_TYPE"] == category,]
  }
  plot(category.data$BEGIN_LON, 
       category.data$BEGIN_LAT, 
       main = paste0("Locations of ", category),
       xlab = "Longitude",
       ylab = "Latitude")
    US(add=TRUE, col="Magenta")
  if (!is.null(state.to.color)) {
    state.data = category.data[category.data["STATE"] == state.to.color]
    points(
      state.data$BEGIN_LON,
      state.data$BEGIN_LAT,
      color="red"
    )
  }
}



#main = function() {  
storm.event.meta.data = list(
  # Data format can be found here: 
  # https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Bulk-csv-Format.pdf
  source = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/legacy/",
  date.range = 1996:2013,
  file.prefix = "Stormdata_",
  alt.file.prefix = "stormdata_",
  file.posfix = ".csv"
)

download.storm.event.data(storm.event.meta.data)

storm.data = load.storm.data(storm.event.meta.data)

storm.data = fix.lon.lat.values(storm.data)

cat("Number of total rows: ", nrow(storm.data), "\n")
cat("Number of rows that have starting location: ", nrow(begin.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have an ending location: ", nrow(end.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have a start and ending location: ", nrow(begin.lat.lon.not.null(end.lat.lon.not.null(storm.data))), "\n")

cat("Storm Categories: ", unique(storm.data$EVENT_TYPE), "\n")

lat.data = begin.lat.lon.not.null(storm.data)
plot.by.category(NULL, lat.data)

#} # end main















