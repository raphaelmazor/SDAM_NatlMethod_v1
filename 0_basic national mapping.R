library(sf)
library(tidyverse)

xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates.csv") %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U")))


sdam_sf<-read_sf("NotForGit/Shapefiles/BetaSDAM_Regions/Major_SDAM_regions.shp")
mlra_sf<-read_sf("NotForGit/Shapefiles/MLRA_Regions/MLRA_Lower48.shp")
nwca_sf<-read_sf("NotForGit/Shapefiles/NWCA_Ecoregions/NWCA_Ecoregions-ECO5.shp")
ohwm_sf<-read_sf("NotForGit/Shapefiles/OHWM_Regions/OHWM_Boundries.shp")
eco_sf<-read_sf("NotForGit/Shapefiles/us_eco_l3/us_eco_l3.shp")


all_regions_sf<-
  bind_rows(sdam_sf %>% mutate(TYPE="SDAM", STRAT=SDAM),
            mlra_sf %>% mutate(TYPE="MLRA", STRAT=Region) %>% st_transform(crs=st_crs(sdam_sf)),
            nwca_sf %>% mutate(TYPE="NWCA", STRAT=NWCA_ECO5) %>% st_transform(crs=st_crs(sdam_sf)),
            ohwm_sf %>% mutate(TYPE="OHWM", STRAT=SDAM) %>% st_transform(crs=st_crs(sdam_sf)),
            eco_sf %>% mutate(TYPE="Eco3", STRAT=US_L3CODE) %>% st_transform(crs=st_crs(sdam_sf)))

st_bbox(mlra_sf)

all_regions_sf$STRAT %>% unique()

randpal_df<-tibble(pal=viridisLite::cividis(108),
                   ord=runif(108)) %>%
  arrange(ord)
  

ggplot()+
  geom_sf(data=all_regions_sf , aes(fill=STRAT))+
  facet_wrap(~TYPE)+
  # scale_fill_viridis_d(guide="none")+
  scale_fill_manual(values=randpal_df$pal, guide="none")+
  coord_sf(xlim=c(-125, -66.5),
           ylim=c(25, 49.5))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

