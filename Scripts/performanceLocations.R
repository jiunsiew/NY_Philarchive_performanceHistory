# NY Philharmonic Orchestra data
# get lat/long

rm(list = ls())
library(ggmap)
library(dplyr)

# load the data
setwd("~/Documents/analytics/NY_Philarchive_performanceHistory/")
performanceData <- readr::read_delim(file = "./Programs/NY_Philharmonic_df.csv",
                                     delim = "|")
# drop the NA column
performanceData[, 1] <- NULL

# split out the location
locations <- data.frame(str_split_fixed(performanceData$Location, ", ", 2), 
                        stringsAsFactors = FALSE)
names(locations) <- c("City", "State_Country")

locationList <- unique(performanceData$Location)

# get lat/lon
latLon <- geocode(locationList)
latLon <- cbind(locationList, latLon)
names(latLon) <- c("Location", "lon", "lat")
# Le Grande, OR failed (idx 164)
latLon$lon[164] <- -118.087719
latLon$lat[164] <- 45.324577

save(latLon, file = "./data/performanceLatLon.RData")

# merge with performance data
performanceData.latLon <- left_join(performanceData, latLon, by = "Location")

# summarise number of performances by location
locationSummary <- performanceData.latLon %>%
  group_by(Location, lon, lat) %>%
  summarise(nPerf = length(unique(programID))) %>%
  mutate(nPerf.log = log(nPerf), 
         hoverText = paste0(Location, ": ", nPerf))


# map plot 
# see: https://plot.ly/r/scatter-plots-on-maps/
# marker styling
m <- list(
  # colorbar = list(title = "Number of Performances (log)"),
  size = 8, opacity = 0.5
)

# geo styling
g <- list(
  # scope = 'usa',
  projection = list(type = 'natural earth'), # list(type = 'albers usa'),
  showcountries = TRUE, 
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 1
)

pMap <- plot_ly(locationSummary, lat = lat, lon = lon, text = hoverText, color = nPerf.log,
        type = 'scattergeo', mode = 'markers', marker = m) %>%
  layout(title = 'NY Philharmonic Performances', geo = g)
pMap
pMapFid <- paste0("~/Documents/analytics/plot.ly presentation/",
                  "slidify_test/mydeck/assets/widgets/", 
                  "plotly_nPerf_Map.html")
htmlwidgets::saveWidget(as.widget(pMap), pMapFid)