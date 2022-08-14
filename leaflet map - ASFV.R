library(leaflet)
library(tidyverse)
library(htmltools) 
library(htmlwidgets)
asfv1 <- read.csv("ASFVraw-data.csv")

#sorting dates
asfv <- asfv1[rep(1:nrow(asfv1), each = 2),] # repeat each row
asfv$date <- with(asfv1, c(rbind(observation.date, Report.date))) # interlace two columns
# or out$Date <- with(out, ifelse(seq_along(first_day) %% 2, first_day, last_day))    

asfv$year <- strptime(as.character(asfv$date), "%d/%m/%Y")
#format year
format(asfv$year, "%Y-%m-%d")
format(as.Date(asfv$year, format="%Y-%m-%d"),"%Y")

asfv$year <- substr(asfv$year, 1, 10)
asfv$year <- as.Date(asfv$year)
library(lubridate)         # year
asfv$year <- year(asfv$year)
years <- sort(unique(asfv$year))
years # sequence of 1996 to 2022

####################mapping

  dp <- asfv %>% 
    filter(Animal.type == "Domestic")
  wb <- asfv %>% 
    filter(Animal.type == "Wild")
#labels 
  dp$label <- paste("<p>", dp$year, "<p>",
                    "<p>", dp$Animal.type, "<p>")
  wb$label <- paste("<p>", wb$year, "<p>",
                    "<p>", wb$Animal.type, "<p>")
  asfv$Animal.type <- as.factor(asfv$Animal.type)
  wb$Animal.type <- as.factor(wb$Animal.type)
  pal <- colorFactor(palette = c("red", "blue"),
                     levels = c("Wild", "Domestic"))
 #colors 
  pals.dp <- colorNumeric(palette = "Blue",
                          domain = dp$year)
  pals.wb <- colorNumeric(palette = "Red",
                          domain = wb$year)
 #leaflet 
  asfv %>% leaflet() %>% addTiles() %>%setView(lng = 15.2551, lat = 54.5260, zoom = 4) %>%
    addCircleMarkers(lng = dp$Longitude, lat = dp$Latitude,
                     color = ~pals.dp(year),
                     weight = 1,
                     group = "domestic pig",
                     radius = 1, label = lapply(dp$label, HTML)) %>%
    addCircleMarkers(lng = wb$Longitude, lat =wb$Latitude,
                    weight = 1,
                    color = ~pals.wb(year),
                     group = "wild boar",
                     radius = 1, label = lapply(wb$label, HTML)) %>%
    addLayersControl(overlayGroups = c("domestic pig", "wild boar"),
                    options = layersControlOptions(collapsed = FALSE))%>%
  
  addLegend(position = "bottomleft", pal= ~pal(Animal.type), values = ~Animal.type, title = "ASFV cases")  
  