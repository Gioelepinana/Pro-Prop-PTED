## Question: How can the effectiveness of scare-off measure on wild boar be measured? 

######## Methods of approach 1 sind nicht aktuell !!!!! ####

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
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE) 
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ComputationalMovementAnalysisData", "ggplot2", "dplyr", "sf", "forcats", "tidyverse", "lubridate","ggmap", "mapview", "tidyr","terra","tmap")
ipak(packages)

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


## Change coordinates scare-off lacations from WGS84 to CH1903+ / LV95
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
## check if the telemetry period correspond to the time where scare-off are active or not 
# There has to be a period before the schreck it is turned on and a period after the schreck it is turned off
# The following Wild boar are suitable for the analysis!!

# Caroline sieht realitv gut aus mit WSS_2016_01
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Caroline, aes(color = "Caroline")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056)+
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Caroline")


# Miriam sieht perfekt aus mit WSS_2016_01
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Miriam, aes(color = "Miriam")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056)+
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Miriam")


# Frida sieht perfekt aus mit WSS_2016_01
ggplot()+
  geom_sf(data = schreck2) +
  geom_sf(data = Frida, aes(color = "Frida")) +
  # geom_sf_label(data = schreck2, aes(label = id)) +
  geom_sf_text(data = schreck2, aes(label = id), size=2, check_overlap = TRUE) +
  coord_sf(datum = 2056)+
  labs(colour = "Legend", title = "Single trajectories with scare-off-locations", subtitle = "Wild boar: Frida")


#### CAROLINE 2016_01 (Approach 2)####

# new column Date
# new column schreck off before / on / off after
# new column with coordinates
# new coloumn with distance between wild boar locations and schreck 2016_01
Caroline <- Caroline %>%
  mutate(Date = as.Date(DatetimeUTC),
         Schreck = case_when(Date < as.Date("2016-04-04") ~ "off before",
                                 Date >= as.Date("2016-04-04") & Date <= as.Date("2016-04-23") ~ "on",
                                 Date > as.Date("2016-04-23") ~ "off after"),
         X = unlist(map(Caroline$geometry,1)),
         Y = unlist(map(Caroline$geometry,2)),
         distance = sqrt((X-2570935)^2+(Y-1205197)^2)) %>%
  filter(Date > as.Date("2016-02-29")& Date < as.Date("2016-07-01"))

# Boxplot Caroline
ggplot(Caroline, aes(Schreck, distance)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean distance between wild boar and scare-off", subtitle = "Sabine")+
  xlab("Mode scare-off") + ylab("Distance")

# T-Test MUSS NOCHMALS GEMACHT WERDEN MIT VOR (OFF) / WÄHREND (ON) / NACH (OFF)

# Mean distance when scare-off measures are turned on is smaller than when it's turned off
# H0: Distance Sabine_on is smaller than Sabine_off
# H1: Distance Sabine_on is greater than Sabine_off

Sabine_on <- Sabine %>%
  filter(Schreck == "on")
  
Sabine_off <- Sabine %>%
  filter(Schreck == "off")

t.test(Sabine_on$distance, Sabine_off$distance, var.equal = TRUE, alternative = c("greater"))


### MIRIAM 2016_01 (Approach 2) ####
Miriam <- Miriam %>%
  mutate(Date = as.Date(DatetimeUTC),
         Schreck = case_when(Date < as.Date("2016-04-04") ~ "off before",
                             Date >= as.Date("2016-04-04") & Date <= as.Date("2016-04-23") ~ "on",
                             Date > as.Date("2016-04-23") ~ "off after"),
         X = unlist(map(Miriam$geometry,1)),
         Y = unlist(map(Miriam$geometry,2)),
         distance = sqrt((X-2570935)^2+(Y-1205197)^2)) 
         
# Boxplot Miriam
ggplot(Miriam, aes(Schreck, distance)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean distance between wild boar and scare-off", subtitle = "Miriam")+
  xlab("Mode scare-off") + ylab("Distance")


### FRIDA 2016_01 (Approach 2) ####
Frida <- Frida %>%
  mutate(Date = as.Date(DatetimeUTC),
         Schreck = case_when(Date < as.Date("2016-04-04") ~ "off before",
                             Date >= as.Date("2016-04-04") & Date <= as.Date("2016-04-23") ~ "on",
                             Date > as.Date("2016-04-23") ~ "off after"),
         X = unlist(map(Frida$geometry,1)),
         Y = unlist(map(Frida$geometry,2)),
         distance = sqrt((X-2570935)^2+(Y-1205197)^2)) 

# Boxplot Frida
ggplot(Frida, aes(Schreck, distance)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mean distance between wild boar and scare-off", subtitle = "Frida")+
  xlab("Mode scare-off") + ylab("Distance")


#### APPROACH 1 ####
# Download crop data 
crop_fanel <- read_sf("Feldaufnahmen_Fanel.gpkg")

head(crop_fanel)

summary(crop_fanel)

unique(crop_fanel$Frucht)

st_crs(crop_fanel)

# Visualization
WSS_2016_01 <- schreck2 %>%
  filter(id == "WSS_2016_01")
ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht)) +
  geom_sf(data = WSS_2016_01) +
  geom_sf_text(data = WSS_2016_01, aes(label = id), size=3, color= "white", check_overlap = TRUE)+
  geom_sf_text(data = crop_fanel, aes(label = FieldID), size =1.2)
# Our schreck WSS_2016_01 is located in "Wiese number 2"


# Overlay the  dataset of the 3 selected wildboar with the fanel data to verify the spatial overlap.
# Caroline 

Caroline <-  st_join(Caroline, crop_fanel)
Caroline

ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht)) +
  geom_sf(data = Caroline) +
  geom_sf(data = WSS_2016_01) +
  geom_sf_text(data = WSS_2016_01, aes(label = id), size=3, color= "white", check_overlap = TRUE)

# Miriam
Miriam <-  st_join(Miriam, crop_fanel)
Miriam

ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht)) +
  geom_sf(data = Miriam) +
  geom_sf(data = WSS_2016_01) +
  geom_sf_text(data = WSS_2016_01, aes(label = id), size=3, color= "white", check_overlap = TRUE)

# Frida
Frida <-  st_join(Frida, crop_fanel)
Frida

ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht)) +
  geom_sf(data = Frida) +
  geom_sf(data = WSS_2016_01) +
  geom_sf_text(data = WSS_2016_01, aes(label = id), size=3, color= "white", check_overlap = TRUE)


# Visual exploration for Caroline (only most relevant "Frucht" displayed!!!)
Caroline_2 <- Caroline %>%
  st_set_geometry(NULL) %>%
  mutate(
    hour = hour(round_date(DatetimeUTC,"hour")),
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    Frucht = fct_lump(Frucht, 5,other_level = "other"),
  ) %>%
  group_by(Schreck,hour,Frucht) %>%
  count() %>%
  ungroup() %>%
  group_by(Schreck,hour) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  )


ggplot(Caroline_2, aes(hour,perc, fill = Frucht)) +
  geom_col(width = 1) +
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  scale_x_continuous(name = "Time (rounded to the nearest hour)") +
  facet_wrap(~Schreck ) +
  theme_classic() +
  labs(title = "Percentages of samples in a given crop per hour",subtitle = "Only showing the most common categories")


# Visual exploration for Frida 
Frida_2 <- Frida %>%
  st_set_geometry(NULL) %>%
  mutate(
    hour = hour(round_date(DatetimeUTC,"hour")),
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    Frucht = fct_lump(Frucht, 5,other_level = "other"),
  ) %>%
  group_by(Schreck,hour,Frucht) %>%
  count() %>%
  ungroup() %>%
  group_by(Schreck,hour) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  )


ggplot(Frida_2, aes(hour,perc, fill = Frucht)) +
  geom_col(width = 1) +
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  scale_x_continuous(name = "Time (rounded to the nearest hour)") +
  facet_wrap(~Schreck ) +
  theme_classic() +
  labs(title = "Percentages of samples in a given crop per hour",subtitle = "Only showing the most common categories")


# Visual exploration for Miriam 
Miriam_2 <- Miriam %>%
  st_set_geometry(NULL) %>%
  mutate(
    hour = hour(round_date(DatetimeUTC,"hour")),
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    Frucht = fct_lump(Frucht, 5,other_level = "other"),
  ) %>%
  group_by(Schreck,hour,Frucht) %>%
  count() %>%
  ungroup() %>%
  group_by(Schreck,hour) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  )


ggplot(Miriam_2, aes(hour,perc, fill = Frucht)) +
  geom_col(width = 1) +
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  scale_x_continuous(name = "Time (rounded to the nearest hour)") +
  facet_wrap(~Schreck ) +
  theme_classic() +
  labs(title = "Percentages of samples in a given crop per hour",subtitle = "Only showing the most common categories")

### Calculation approach 1 ####
## WSS_2016_01 was on for 19 days consecutive. So that a comparison has sense,
## we filter the period from 19 days before the schreck was turned on and 19 days after!

# Caroline 
Caroline_approach1 <- Caroline %>%
  st_set_geometry(NULL) %>%
  filter(Date > as.Date("2016-03-16")) %>%
  filter(Date < as.Date("2016-05-12")) %>%
  group_by(Schreck, FieldID) %>%
  summarise(total = sum(FieldID)) %>%
  filter(FieldID == 2)
## Beim Caroline gibt es nicht Data während und nacher.... wir verwenden nur Miriam und Frida

# Miriam
Miriam_approach1 <- Miriam %>%
  st_set_geometry(NULL) %>%
  filter(Date > as.Date("2016-03-16")) %>%
  filter(Date < as.Date("2016-05-12")) %>%
  group_by(Schreck, FieldID) %>%
  summarise(total = sum(FieldID)) %>%
  filter(FieldID == 2)

# Frida
Frida_approach1 <- Frida %>%
  st_set_geometry(NULL) %>%
  filter(Date > as.Date("2016-03-16")) %>%
  filter(Date < as.Date("2016-05-12")) %>%
  group_by(Schreck, FieldID) %>%
  summarise(total = sum(FieldID)) %>%
  filter(FieldID == 2)

## Statistische Test? oder nur vergleich der Anzahl Punkte?


###  Visualization of the "Streifgebiet"
# Calculate Convex Hull for the 3 selected wild boars
wildschwein_BE_spatial_smry <- wildschwein_BE_spatial %>%
  filter(TierName %in% c("Caroline", "Miriam", "Frida")) %>%
  group_by(TierName) %>%
  summarise()

mcp <- st_convex_hull(wildschwein_BE_spatial_smry) 

# Import raster data
pk100_BE <- terra::rast("pk100_BE (2).tif")

# Plot interactive map
tmap_mode("view")
tm_shape(mcp) +
  tm_polygons(col = "TierName",alpha = 0.4,border.col = "red") +
  tm_legend(bg.color = "white") 
