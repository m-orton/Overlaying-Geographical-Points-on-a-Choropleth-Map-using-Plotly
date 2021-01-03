library("ggplot2")
library("plotly")
library("RColorBrewer")
library("colorspace")

# Assuming you have imported a csv with lat and lon values for invididual american states
# Using 5 states as examples

coord <- read.csv("yourcsv.csv")

dfIowa <- data.frame(coord[1:19,])
dfIowa$stateName <- "IA"

dfIdaho <- data.frame(coord[20:59,])
dfIdaho$stateName <- "ID"

dfNebraska <- data.frame(coord[60:87,])
dfNebraska$stateName <- "NE"

dfSouthDakota <- data.frame(coord[88:131,])
dfSouthDakota$stateName <- "SD"

# Removing the montana point for now and an empty NA entry at the end
dfWyoming <- data.frame(coord[133:153,])
dfWyoming$stateName <- "WY"

# Then you you can simply add them together into one dataframe (just make sure all columns are named exactly the same across all dataframes)
mapdata <- rbind(dfIowa, dfIdaho, dfNebraska, dfSouthDakota, dfWyoming)

# geo styling - can modify these to change parameters of the map
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  countrywidth = 0.5,
  subunitwidth = 0.5,
  # these lists can specify lat and lon ranges
  # you can also move around the map while viewing it and download that map view as a png 
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    # range = c(-130, -88),
    dtick = 5),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    # range = c(40.8, 60),
    dtick = 5)
)

# Break down by state
stateList <- lapply(unique(mapdata$stateName), function(x) 
  mapdata[mapdata$stateName == x,])

# Palette from rcolorbrewer - can find other color palettes here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# I'm using single color ones so each state is distinct from the others

# Iowa - oranges palette
colfuncI <- brewer.pal(9, "Oranges")
display.brewer.pal(9, "Oranges")
palI <- colfuncI[7]
stateList[[1]]$stateColors <- palI
stateList[[1]]$z[1:9] <- 0.0
stateList[[1]]$z[10:18] <- 0.2

# Idaho - purples palette
colfuncId <- brewer.pal(9, "Purples")
# can look at the purple palette with this command in the viewer
display.brewer.pal(9, "Purples")
# then pick which color you would want the points to be - 
# so if I wanted the third darkest purple for Idaho points I'd reference the 7th position (1st pos being lightest, 9th position being darkest) 
# in the colfuncId vector to grab the corresponding hex value:
palId <- colfuncId[7]
stateList[[2]]$stateColors <- palId

# These z variables are important for coloring the map background based on the state
# these may need to be changed depending on if records are removed
stateList[[2]]$z[1:20] <- 0.2
stateList[[2]]$z[21:39] <- 0.4

# Nebraska  - Reds
colfuncN <- brewer.pal(9, "Reds")
display.brewer.pal(9, "Reds")
palN <- colfuncN[7]
stateList[[3]]$stateColors <- palN
stateList[[3]]$z[1:14] <- 0.4
stateList[[3]]$z[15:28] <- 0.6

# South Dakota - Greens
colfuncSk <- brewer.pal(9, "Greens")
display.brewer.pal(9, "Greens")
palSk <- colfuncSk[7]
stateList[[4]]$stateColors <- palSk
stateList[[4]]$z[1:22] <- 0.6
stateList[[4]]$z[23:44] <- 0.8

# Wyoming - Blues
colfuncW <- brewer.pal(9, "Blues")
display.brewer.pal(9, "Blues")
palW <- colfuncW[7]
stateList[[5]]$stateColors <- palW
stateList[[5]]$z[10:21] <- 1
stateList[[5]]$z[1:9] <- 0.8

# Collapse the stateList into a single dataframe replacing original mapdata df 
# with one that has color palettes
mapdata2 <- do.call("rbind", stateList)
mapdata2$stateName = factor(mapdata2$stateName)
mapdata2$stateNum <- as.numeric(mapdata2$stateName)

# Now for the point colors we have to darken them slightly so they stand out
# we can use the colorspace package for that
# the amount variable can be increased to darken the points more so they stand out from the background
# mapdata2$stateColors <- darken(mapdata2$stateColors, amount = 0.5, space = "combined")

# Create a dataframe called colorscale which is necessary to color the background for each of the states
colorScale <- data.frame(z = mapdata2$z, col = mapdata2$stateColors)

# Load in data 
# ***make sure to add the ~ symbols before variables, plotly is weirdly picky about it
fig <- plot_geo(mapdata2, lat = ~lat, lon = ~lon) 

fig <- fig %>%   
  # adds the markers (can exclude size and color if you want)
  # symbol of points can be changed for points
  # opacity changes transparency of points so overlapping points are easier to see (0.1 = high transparency, 1 = fully opaque)
  add_markers(color = ~stateName,
              colors =  ~stateColors, 
              symbol = I("circle"),
              size = 8,
              opacity = 0.9) %>% 
  add_trace(z = ~stateNum,
            locations = ~stateName,
            type = "choropleth", 
            colorscale = colorScale,
            locationmode = "USA-states",
            showscale=FALSE) %>%
  layout(title = 'Map does more things now, yay!', geo = g)
fig
