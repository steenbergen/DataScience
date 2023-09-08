library(rio)
library(rworldmap)
library(tidyverse)
library(viridis)

palette <- viridis(option = "D", n = 7)
df <- import(file.choose())
map_df <- joinCountryData2Map(df,
                              joinCode = "ISO3",
                              nameJoinColumn = "ISO3",
                              verbose = TRUE)
mapCountryData(map_df,
               nameColumnToPlot = "happiness",
               colourPalette = palette,
               catMethod = "quantiles",
               mapTitle = "Happiness")
