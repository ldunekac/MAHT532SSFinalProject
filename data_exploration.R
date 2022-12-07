
library(fields)

setwd("C:\\Users\\alexg\\OneDrive\\PhD\\Fall2023\\AMS532-Spatial_Statistics\\Final_Project\\MAHT532SSFinalProject")

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
      print(paste0("File: ", 
                   file.name, 
                   " already exists. Skipping"))
    } else {
      prefix = ifelse(date <= 2010, file.prefix, alt.file.prefix)
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
  

  data[c("BEGIN_LON", "END_LON")] = lapply(data[c("BEGIN_LON", "END_LON")], 
                                           function(x) ifelse(x < -200, x/10, x))
  
  return(data)
}


###############################################################################

# More rows per year. Same columns
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
    state.data = category.data[category.data["STATE"] == state.to.color,]
    points(
      state.data$BEGIN_LON,
      state.data$BEGIN_LAT,
      col="red"
    )
  }
}


count.occurances.in.boxes = function(lat.lon.grid, data, category, year = NULL) {
  filtered.data = data[data["EVENT_TYPE"] == category,]
  if(!is.null(year)) {
    filtered.data = filtered.data[filtered.data["YEAR"] == year,]
  }

  n_lon_count = length(lat.lon.grid$longitude)-1
  n_lat_count = length(lat.lon.grid$latitude)-1
  grid_counts = matrix(NA, 
                       nrow=n_lon_count, 
                       ncol=n_lat_count)
  counts = matrix(NA, 
                  nrow=n_lon_count * n_lat_count, 
                  ncol=1)
  centers = matrix(NA, 
                   nrow=n_lon_count * n_lat_count, 
                   ncol=2)
  
  row_count = 0
  for(i in 1:n_lon_count) {
    for(j in 1:n_lat_count) {
      lon_min = lat.lon.grid$longitude[i]
      lon_max = lat.lon.grid$longitude[i+1]
      lat_min = lat.lon.grid$latitude[j]
      lat_max = lat.lon.grid$latitude[j+1]
      
      grid_counts[i,j] = nrow(filtered.data[ (filtered.data["BEGIN_LAT"] >= lat_min) & 
                             (filtered.data["BEGIN_LAT"] <= lat_max) &
                             (filtered.data["BEGIN_LON"] >= lon_min) &
                             (filtered.data["BEGIN_LON"] <= lon_max),])
      
      counts[row_count] = grid_counts[i,j]
      centers[row_count,] = c((lon_max + lon_min)/2,
                              (lat_max + lat_min)/2)
      row_count = row_count + 1
    }
  }
  return(list(grid_counts = grid_counts, 
              s=centers, 
              z=counts))
}

make.year.column = function(data) {
  data$YEAR = lapply(data$BEGIN_YEAR, function(x) floor(x/100))
  return(data)
}

############################################################################

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

cat("Number of total rows: ", 
    nrow(storm.data), "\n")
cat("Number of rows that have starting location: ", 
    nrow(begin.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have an ending location: ", 
    nrow(end.lat.lon.not.null(storm.data)), "\n")
cat("Number of rows that have a start and ending location: ", 
    nrow(begin.lat.lon.not.null(end.lat.lon.not.null(storm.data))), "\n")
cat("Storm Categories: ", 
    unique(storm.data$EVENT_TYPE), "\n")

lat.data = begin.lat.lon.not.null(storm.data)
lat.data = make.year.column(lat.data)
plot.by.category(NULL, lat.data, "NEBRASKA")
#write.csv(lat.data, file="data/storm_data.csv")

nb_ks_box = list(longitude = seq(-104.056, -94.596, 
                                 length.out=20),
                 latitude = seq(36.9799, 43.0000, 
                                length.out=20))

grid_nb_ks = make.surface.grid(nb_ks_box)
#BR = -94.596, 36.9799
#TL= -104.056 43.0000

count.data = count.occurances.in.boxes(nb_ks_box, 
                                       lat.data, 
                                       "Tornado", 
                                       year=2003)

dev.new()
bubblePlot(count.data$s[,1], count.data$s[,2], count.data$z)
US(add=TRUE, col="magenta")  

count.matrix = matrix(NA, 
                      nrow=nrow(count.data$z), 
                      ncol = length(1996:2013))

idx = 1
for(year in 1996:2013) {
  print(year)
  count.matrix[,idx] = count.occurances.in.boxes(nb_ks_box, 
                                                 lat.data, 
                                                 "Tornado", 
                                                 year=year)$z
  idx = idx + 1
}

count.average = as.matrix(rowMeans(count.matrix))
##### AWWWWWWWWW ########  AAAAAAAAWWWWWWWWW #################
##############################################################
##############################################################



# fit a GLM model to use for starting values
y = count.average
s = count.data$s
glmFit<- glm(y ~ s, family = poisson())
lambda<- .01
# starting value (this does not need to be a GLM estimate but need to be positive)
# poorer estimates may not give convergence. 
gOLD<-  predict(glmFit)
# plot interates
#plot( s, gOLD, type="l", col=1)
coltab<-rainbow(6)
for(I in 1:4){
  print(I)
  fHat <- exp(gOLD)
  z <- c(y[1:length(y)-1] - fHat)/fHat + gOLD 
  weights<- c(fHat)
  TpsObj <-  suppressWarnings(
    spatialProcess(s[1:nrow(s)-1,], z, 
                   weights=weights, 
                   smoothness=.5) # Exponential kernel
  )
  gNEW<- c(predict( TpsObj))
  testTol <- sqrt(mean((gNEW - gOLD)^2)/mean(gOLD^2))
  cat( I, testTol, fill=TRUE)
  gOLD<- gNEW
  # add new estimate
  #lines( s, gNEW, col=coltab[I], lwd=2)
}

dev.new()
bubblePlot(s[1:nrow(s)-1,1], s[1:nrow(s)-1,2], exp(gOLD))
US(add=TRUE)

######

# Plot Residuals for one of the TORNADO // HAIL, etc. 

# 


#} # end main


surface(TpsObj)
US(add=TRUE)

fHatImage <- predictSurface(myfit, nx=60, ny=60)
Image.plot(fHatImage)









