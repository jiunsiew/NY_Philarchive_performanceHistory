# Data analysis for NY Philharmonic performance data
# 
# first analysis is to do a time series of counts of composer performances 
# data from xml files were extracted and saved to csv using the 
# 'transform_xml_to_df.py' script
# 
rm(list = ls())

# load the data
setwd("~/Documents/analytics/NY_Philarchive_performanceHistory/")
performanceData <- readr::read_delim(file = "./Programs/NY_Philharmonic_df.csv",
                                     delim = "|")

# scrub the data toilet bowl
library(dplyr)
library(lubridate)
library(stringr)
library(plotly)
library(viridis)


# drop the NA column
performanceData[, 1] <- NULL

# split out the location
locations <- data.frame(str_split_fixed(performanceData$Location, ", ", 2), 
                        stringsAsFactors = FALSE)
names(locations) <- c("City", "State_Country")

# split out the composer first/last name
composers <- data.frame(str_split_fixed(performanceData$composerName, ", ", 2),
                        stringsAsFactors = FALSE)
names(composers) <- c("last_name", "first_name")

# clean up the international spellings captured in "[*]"
composerLastName <- str_replace_all(composers$last_name, "\\[[[:graph:]]+\\]", "")

# some weird ones--
composerLastName <- str_replace_all(composerLastName, "\\[Karrer", "")
composerLastName <- str_replace_all(composerLastName, "\\(Nápravník\\)", "")

# clean up whitespace, commas
composerLastName <- str_replace(composerLastName, "[ ,]+$", "")

# view the composers
unique.composers <- unique(composerLastName)
(unique.composers <- unique.composers[order(unique.composers)])

# construct the plotting df
plotDf <- performanceData[, c("programID", "date")]
plotDf$composer <- composerLastName
head(plotDf)

plotDf.sum <- plotDf %>%
  filter(composer != "") %>%
  mutate(date.yr = round_date(date, "year")) %>%
  group_by(composer, date.yr) %>%
  summarise(nPerf = n()) %>%
  arrange(composer, date.yr)

# look at the distribution of total performances
countsByComposer <- plotDf %>%
  filter(composer != "") %>%
  group_by(composer) %>%
  summarise(nPerf = n()) %>%
  arrange(desc(nPerf)) %>%
  mutate(rank = seq(1, n()))
ggplot(countsByComposer, aes(x = nPerf)) + geom_density() + theme_bw()

# plot the top n
n <- 50
pComposers <- plot_ly(countsByComposer[1:n, ], x = composer, y = nPerf,
        color = rank,
        text = paste("Rank: ", rank),
        mode = "markers")
pComposers
pComposerFid <- paste0("~/Documents/analytics/plot.ly presentation/",
                       "slidify_test/mydeck/assets/widgets/", 
                       "plotly_topComposers.html")
htmlwidgets::saveWidget(as.widget(pComposers), pComposerFid)


# ggplot
ggplot(countsByComposer[1:n, ], 
       aes(x = reorder(composer, -nPerf), y = nPerf)) +
  geom_point(aes(colour = rank)) +
  scale_color_viridis() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_blank()) +
  ggtitle(paste0("Top ", n, " Composers By Performances")) +
  ylab("Number of performances")

# look at number of performances by year
nPerfByYear <- performanceData %>%
  filter(!is.na(composerName)) %>%
  mutate(date.yr = round_date(date, "year")) %>%
  group_by(date.yr) %>%
  summarise(nPerf = length(unique(programID)))

p <- plot_ly(nPerfByYear,
             x = date.yr,
             y = nPerf,
             name = "Total Performances")
fitted.val <- fitted(loess(nPerf ~ lubridate::year(date.yr), data = nPerfByYear))
p <- p %>% add_trace(y = fitted.val, x = date.yr, name = "Loess Fit")

pFid <- paste0("~/Documents/analytics/plot.ly presentation/",
               "slidify_test/mydeck/assets/widgets/", 
               "plotly_perfByYear.html")
htmlwidgets::saveWidget(as.widget(p), pFid)

# ggplot way
ggplot(nPerfByYear, aes(x = date.yr, y = nPerf)) +
  geom_line() + 
  theme_bw() +
  stat_smooth() +
  ggtitle("NY Philharmonic Performances") +
  ylab("Number of Performances") +
  xlab("Year")

# Plot time series of performances for the top 10 composers
tmpDf <- plotDf.sum %>%
  filter(composer %in% countsByComposer$composer[1:12])

tmpDf$composer.f <- factor(tmpDf$composer, 
                           levels = countsByComposer$composer[1:12])
# from http://colorbrewer2.org/?type=qualitative&scheme=Paired&n=10
# colorScheme <- c('#a6cee3','#1f78b4',
#                 '#b2df8a','#33a02c',
#                 '#fb9a99','#e31a1c',
#                 '#fdbf6f','#ff7f00',
#                 '#cab2d6','#6a3d9a')

colorScheme = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
                '#cab2d6','#6a3d9a','#ffff99','#b15928')
plot_ly(tmpDf,
        x = date.yr,
        y = nPerf,
        color = composer,
        colors = "colorScheme")

# ggplot
pFacet <- ggplot(tmpDf, aes(x = date.yr, y = nPerf)) +
  geom_line(aes(colour = composer)) +
  facet_wrap(~composer.f) +
  theme_bw() +
  ggtitle("NY Philharmonic Performances") +
  ylab("Number of Performances") +
  xlab("Year") +
  scale_color_manual(values = colorScheme) +
  theme(legend.position="none")

pFacet <- ggplotly(pFacet)
pFacet

pFacetFid <- paste0("~/Documents/analytics/plot.ly presentation/",
                    "slidify_test/mydeck/assets/widgets/", 
                    "plotly_timeSeriesByComposer.html")
htmlwidgets::saveWidget(as.widget(pFacet), pFacetFid)

# 3D plot
nPerfByComposerYear <- performanceData %>%
  mutate(composer = composers$last_name) %>%
  filter(!is.na(composerName)) %>%
  mutate(date.yr = round_date(date, "year")) %>%
  group_by(composer, date.yr) %>%
  summarise(nPerf = length(unique(programID)))

# make it a matrix
plot3DMat <- nPerfByComposerYear %>%
  filter(composer %in% countsByComposer$composer[1:20]) %>%
  mutate(yr = lubridate::year(date.yr)) %>%
  select(-date.yr) # %>%
  # tidyr::spread(key = composer, value = nPerf) 
  
# plot_ly(z = as.matrix(plot3DMat), type = "surface")
p3D <- plot_ly(plot3DMat, x = composer, y = yr, z = nPerf, 
        color = composer, type = "scatter3d", opacity = 0.5)

# save
p3dFid <- paste0("~/Documents/analytics/plot.ly presentation/",
                 "slidify_test/mydeck/assets/widgets/", 
                 "plotly_timeSeriesByComposer_3D.html")
htmlwidgets::saveWidget(as.widget(p3D), p3dFid)


# stopped here:
# do maps next