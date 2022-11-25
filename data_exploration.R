

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

cat("Number of total rows: ", nrow(storm.data), "\n")
cat("Number of rows that have starting location: ", nrow(begin.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have an ending location: ", nrow(end.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have a start and ending location: ", nrow(begin.lat.lon.not.null(end.lat.lon.not.null(storm.data))), "\n")

cat("Storm Categories: ", unique(storm.data$EVENT_TYPE), "\n")
#} # end main





