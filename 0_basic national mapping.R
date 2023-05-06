library(sf)
library(tidyverse)

xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates.csv") %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U")))


sdam_sf<-read_sf("NotForGit/Shapefiles/BetaSDAM_Regions/Major_SDAM_regions.shp")
mlra_sf<-read_sf("NotForGit/Shapefiles/MLRA_Regions/MLRA_Lower48.shp") %>% st_transform(crs=st_crs(sdam_sf))
nwca_sf<-read_sf("NotForGit/Shapefiles/NWCA_Ecoregions/NWCA_Ecoregions-ECO5.shp") %>% st_transform(crs=st_crs(sdam_sf))
ohwm_sf<-read_sf("NotForGit/Shapefiles/OHWM_Regions/OHWM_Boundries.shp") %>% st_transform(crs=st_crs(sdam_sf))
eco_sf<-read_sf("NotForGit/Shapefiles/us_eco_l3/us_eco_l3.shp") %>% st_transform(crs=st_crs(sdam_sf))
#The full 9-region NARS layer has the same error the 5-region layer has (duplicated vertex in edges 554 and 645?)
# nars9_sf<-read_sf("NotForGit/Shapefiles/Aggr_Ecoregions_2015/Aggr_Ecoregions_2015.shp") %>% st_transform(crs=st_crs(sdam_sf)) %>%
#   mutate(NWCA5 = case_when(WSA9  %in% c("NAP","UMW","SAP")~"EMU",
#                            WSA9  %in% c("NPL","SPL","TPL")~"IPL",
#                            T~WSA9))

all_regions_sf<-
  bind_rows(sdam_sf %>% mutate(TYPE="SDAM", STRAT=SDAM),
            mlra_sf %>% mutate(TYPE="MLRA", STRAT=Region) ,
            nwca_sf %>% mutate(TYPE="NWCA", STRAT=NWCA_ECO5) ,
            ohwm_sf %>% mutate(TYPE="OHWM", STRAT=SDAM) 
            # eco_sf %>% mutate(TYPE="Eco3", STRAT=US_L3CODE) 
  )

st_bbox(mlra_sf)

all_regions_sf$STRAT %>% unique()

randpal_df<-tibble(pal=viridisLite::cividis(108),
                   ord=runif(108)) %>%
  arrange(ord)

xwalk_sf<-xwalk_df %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords=c("long","lat"), crs=4326) %>%
  st_transform(crs=st_crs(sdam_sf))

ggplot()+
  geom_sf(data=all_regions_sf %>% filter(TYPE!="Eco3") , aes(fill=STRAT))+
  geom_sf(data=xwalk_sf, aes(shape=Class))+
  geom_sf(data=all_regions_sf %>% filter(TYPE=="SDAM") %>% select(-TYPE) , fill=NA, color="chartreuse")+
  facet_wrap(~TYPE)+
  # scale_fill_viridis_d(guide="none")+
  scale_fill_manual(values=randpal_df$pal, guide="none")+
  coord_sf(xlim=c(-125, -66.5),
           ylim=c(25, 49.5))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

xwalk_df%>%
  filter(is.na(lat))


xwalk_sf2<-xwalk_sf %>%
  st_join(sdam_sf %>% transmute(beta_region=SDAM)) %>%
  st_join(ohwm_sf %>% transmute(ohwm_region=SDAM))%>%
  st_join(nwca_sf %>% transmute(nwca_region=NWCA_ECO5))%>%
  st_join(mlra_sf %>% transmute(corsp_region=Region)) 

xwalk_sf2 %>%
  filter(sitecode %in% c("COCB121","TXSV452","WYNV634"))

xwalk_sf %>% st_join(nars9_sf %>% transmute(nwca_region=NWCA5))
ggplot()+geom_sf(data=nars9_sf, aes(fill=NWCA5))+
  geom_sf(data=nwca_sf, fill=NA, color="white") + 
  geom_sf(data=xwalk_sf)

nars9_sf$geometry[554]
