## Question: How can the effectiveness of scare-off measure on wild boar be measured? 

## Two methods to confront (pros and cons)
    # 1: measure the mean distance between wild boar location and scare-off 
    #    (mean distance between two consecutive location) and build a graph / map 
    # 2: confront the mean distance between wild boar and scare-off when the
    #    scare-off are turned on and when the scare-off are turned off

## Approach: 
    # 1: here we have to define what is effective in our opinion --> The wild boar 
    #    leaves and stays at a distance from the scare-off for at least two days
    #     - define relevant scare-off-locations: the ones near the wild boar
    #     - choose 3 wild boars to do the analysis  
    #     - enter the coordinates of the relevant scare-off into the boar table. 
    #     - calculate distances 
    #     - 
    # 2: here we measure a statistical effectiveness using p-value: 
    #     - define relevant scare-off-locations: the ones near the wild boar 
    #     - choose 3 wild board to do the analysis (same as above)
    #     - enter the coordinates of the relevant scare-off into the boar table. 
    #     - calculate distances 

## Libraries 
library(ComputationalMovementAnalysisData)
library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(tidyverse)


## first look to the data-sets
head(wildschwein_BE)

View(wildschwein_BE) 
View(wildschwein_metadata)
View(wildschwein_overlap_temp)
View(schreck_agenda)
View(schreck_locations)

# Sample regime 
limits <- c(0,200)
breaks = seq(0,200,50)
labels = paste(c(rep("",length(breaks)-1),">"), breaks)

wildschwein_BE %>%
  mutate(TierName = fct_reorder(TierName, DatetimeUTC,min, .desc = TRUE)) %>%
  group_by(TierID, TierName, CollarID) %>%
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units = "mins")),
  ) %>%
  ggplot(aes(DatetimeUTC, TierName, colour = timelag)) +
  geom_line(lwd = 10) +
  scale_color_gradientn(name = "Sampling interval", colours = RColorBrewer::brewer.pal(11, "Spectral"), limits = limits, na.value = NA, oob = scales::squish, breaks = seq(0,200,50), labels = labels) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))

## Temporal Overlap
sampling_periods <- wildschwein_BE %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise(
    min = min(DatetimeUTC),
    max = max(DatetimeUTC)
  )


wildschwein_overlap_temp <- wildschwein_overlap_temp %>%
  left_join(sampling_periods, by = c("TierID", "TierName", "CollarID"))


wildschwein_overlap_temp %>%
  mutate(TierCollar = paste(TierName, CollarID)) %>%
  ggplot(aes(xmin = min, xmax = max, y = TierCollar)) +
  geom_errorbarh() +
  facet_grid(Groups~., scales = "free_y", space = "free_y")

## Spatial overlap
wildschwein_sf <- wildschwein_BE %>%
  st_as_sf(coords = c("E", "N"), crs = 2056) %>%
  mutate(tiercollar = paste(TierID, TierName, CollarID)) 


wildschwein_convex_hull <- wildschwein_sf %>%
  group_by(TierID, TierName, CollarID) %>%
  summarise() %>%
  st_convex_hull()


wildschwein_convex_hull %>%
  mutate(tiercollar = paste(TierID, TierName, CollarID)) %>%
  ggplot(aes(fill = factor(TierID))) + geom_sf(alpha = 0.1) +
  coord_sf(datum = 2056) +
  facet_wrap(~tiercollar) +
  theme(legend.position = "none")


## Change coordinates scare-off lacations from gps to CH1903+ / LV95
## first transform dataframe into a spatial object
schreck_locations_swiss_coord <- st_as_sf(schreck_locations, 
                              coords = c("lon", "lat"), 
                              crs = 4326)
## Now coordinate transformation
schreck_locations_swiss_coord <- st_transform(schreck_locations_swiss_coord, 2056)

## Transform Wildschwein_BE as weel into a spatial object 
wildschwein_BE_spatial <- st_as_sf(wildschwein_BE, 
                                          coords = c("E", "N"), 
                                          crs = 2056)

## select first 7 wild board
target = c("Sabine", "Ruth", "Rosa", "Nicole", "Isabelle","Fritz","Caroline")

w <- wildschwein_BE_spatial %>%
  filter(TierName %in% target)

## Plotting the location of the scare-off 
ggplot(schreck_locations_swiss_coord)+
  geom_sf() +
  coord_sf(datum = 2056) 

## Plotting wild board together with locations scare-off  
ggplot()+
  geom_sf(data = schreck_locations_swiss_coord) +
  geom_sf(data = wildschwein_BE_spatial, aes(color = "red")) +
  coord_sf(datum = 2056)

## Plotting the location of the scare-off with the corresponding ID, to select the important ones
ggplot(schreck_locations_swiss_coord, label = "id")+
  geom_sf() +
  coord_sf(datum = 2056)+
  geom_sf_text(aes(label = id, check_overlap = TRUE)) +
  scale_y_continuous(limits=c(1206500,1207000)) +
  scale_x_continuous(limits=c(2570000,2571000))

## select important scare-off 
# the following scare-off location are the ones near the wild boards studied and therefore 
# they could be suitable for the analysis. 
target2 = c("WSS_2016_05", "WSS_2016_01", "WSS_2016_13", "WSS_2016_06", "WSS_2015_01","WSS_2015_03","WSS_2015_04","WSS_2014_04","WSS_2014_05","WSS_2014_06","WSS_2017_02","WSS_2017_01","WSS_2017_09","WSS_2017_07","WSS_2017_03","WSS_2017_04")
schrecks <- schreck_locations_swiss_coord %>%
  filter(id %in% target2)

## plot the important scare-off
ggplot(schrecks)+
  geom_sf() +
  coord_sf(datum = 2056)

## create dataframe with single wild board 
Ueli <- wildschwein_BE_spatial %>%
  filter(TierName == "Ueli")
Sabine <- wildschwein_BE_spatial %>%
  filter(TierName == "Sabine")
Caroline <- wildschwein_BE_spatial %>%
  filter(TierName == "Caroline")
Isabelle <- wildschwein_BE_spatial %>%
  filter(TierName == "Isabelle")
Rosa <- wildschwein_BE_spatial %>%
  filter(TierName == "Rosa")
Ruth <- wildschwein_BE_spatial %>%
  filter(TierName == "Ruth")
Miriam <- wildschwein_BE_spatial %>%
  filter(TierName == "Miriam")
Fritz <- wildschwein_BE_spatial %>%
  filter(TierName == "Fritz")
Claude <- wildschwein_BE_spatial %>%
  filter(TierName == "Claude")
Olga <- wildschwein_BE_spatial %>%
  filter(TierName == "Olga")
Franz <- wildschwein_BE_spatial %>%
  filter(TierName == "Franz")
Amos <- wildschwein_BE_spatial %>%
  filter(TierName == "Amos")
Nicole <- wildschwein_BE_spatial %>%
  filter(TierName == "Nicole")
Venus <- wildschwein_BE_spatial %>%
  filter(TierName == "Venus")
Evelin <- wildschwein_BE_spatial %>%
  filter(TierName == "Evelin")
Gaby <- wildschwein_BE_spatial %>%
  filter(TierName == "Gaby")
Frida <- wildschwein_BE_spatial %>%
  filter(TierName == "Frida")
Donar <- wildschwein_BE_spatial %>%
  filter(TierName == "Donar")
Joanna <- wildschwein_BE_spatial %>%
  filter(TierName == "Joanna")


## Join two scare-off dataframes
schreck2 <- left_join(schrecks,schreck_agenda,by="id")

## plotting single trajectories with scare-off-locations
# in this graphs choose which scare-off are suitable for the anaylsis 
# (the ones near the location of the wild board location)
# Ueli
ggplot()+
  geom_sf(data = Ueli, aes(color = "Ueli")) +
  geom_sf(data = schreck2) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Ueli")
# suitable: 2016_05, 2017_02, 2017_01, 2016_01, 2014_05, 2014_04, 2016_13

# Sabine 
ggplot()+
  geom_point(data = Sabine, aes(X, Y, color = "Sabine")) +
  geom_sf(data = schreck2) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  # scale_y_continuous(limits=c(1205150,1205250)) +
  # scale_x_continuous(limits=c(2570900,2571000)) +
  theme_classic() +
  labs(colour = "Legend", title = "Wild boar and scare-off-locations", subtitle = "Wild boar: Sabine")
# suitable: 2014_06, 2017_03, 2016_04, 2014_04, 2014_05

# Caroline 
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Caroline, aes(color = "Caroline")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056)+
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Caroline")
# suitable: 2017_07, 2016_13, 2014_04, 2014_05, 2016_01, 2017_02, 2017_01

# Isabelle 
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Isabelle, aes(color = "Isabelle")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  labs(colour = "Legend", title = "Wild boar and scare-off-locations", subtitle = "Wild boar: Isabelle")
# suitable: 2014_05, 2014_04 2016_13

# Rosa 
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Rosa, aes(color = "Rosa")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Rosa")
# suitable:2017_01, 2017_02, 2016_05, 2014_05, 2014_04


#All for fun
ggplot()+
  geom_sf(data = Ueli, aes(color = "Ueli"), alpha = .5) +
  geom_sf(data = Sabine, aes(color = "Sabine"), alpha = .5) +
  geom_sf(data = Caroline, aes(color = "Caroline"), alpha = .5) +
  geom_sf(data = Isabelle, aes(color = "Isabelle"), alpha = .5) +
  geom_sf(data = Rosa, aes(color = "Rosa"), alpha = .5) +
  geom_sf(data = schreck2) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  labs(colour = "Legend", title = "Some wild-boar-trajectories with scare-off-locations")



## check if the telemetry period correspond to the time where scare-off are active or not 
# I don't know if there is a method or if we have to do this manually...
# ...
# Choose definitive 3 wild boar to do the analysis --> Sabine

## first try with Sabine 2014_04 (Approach 2):
# new column Date
# new column schreck on or off
# new column with coordinates
# new coloumn with distance between wild boar locations and schreck 2014_04
Sabine <- Sabine %>%
  mutate(Date = as.Date(DatetimeUTC),
         Schreck = ifelse(Date >= as.Date("2014-05-01") & Date <= as.Date("2014-10-28"), "on", "off"),
         X = unlist(map(Sabine$geometry,1)),
         Y = unlist(map(Sabine$geometry,2)),
         distance = sqrt((X-2570935)^2+(Y-1205197)^2))

# Breit --> Longformat
ggplot(Sabine, aes(Schreck, distance)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean distance between wild boar and schreck-locations", subtitle = "Sabine")+
  xlab("Mode schreck") + ylab("Distance")

# T-Test
# Mean distance when scare-off measures are turned on is smaller than when it's turned off
# H0: Distance Sabine_on is smaller than Sabine_off
# H1: Distance Sabine_on is greater than Sabine_off

Sabine_on <- Sabine %>%
  filter(Schreck == "on")
  
Sabine_off <- Sabine %>%
  filter(Schreck == "off")

t.test(Sabine_on$distance, Sabine_off$distance, var.equal = TRUE, alternative = c("greater"))


par(mfrow=c(1,2))


#Compare Mode on and off
# On
ggplot()+
  geom_point(data = Sabine_on, aes(X, Y, color = "Sabine")) +
  geom_sf(data = schreck2) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  # scale_y_continuous(limits=c(1205150,1205250)) +
  # scale_x_continuous(limits=c(2570900,2571000)) +
  theme_classic() +
  labs(colour = "Legend", title = "Schreck-Mode: On", subtitle = "Wild boar: Sabine")
# suitable: 2014_06, 2017_03, 2016_04, 2014_04, 2014_05

#Off
ggplot()+
  geom_point(data = Sabine_off, aes(X, Y, color = "Sabine")) +
  geom_sf(data = schreck2) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056) +
  # scale_y_continuous(limits=c(1205150,1205250)) +
  # scale_x_continuous(limits=c(2570900,2571000)) +
  theme_classic() +
  labs(colour = "Legend", title = "Schreck-Mode: Off", subtitle = "Wild boar: Sabine")
# suitable: 2014_06, 2017_03, 2016_04, 2014_04, 2014_05


