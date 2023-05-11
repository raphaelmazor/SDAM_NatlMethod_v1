library(sf)
library(tidyverse)

xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates.csv") %>%
  bind_rows(
    read_csv("Data/pnw_xwalk.csv") %>%
              transmute(sitecode=as.character(sitecode),
                        Class, Region_detail, Region, lat, long)
            ) %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U"))) %>%
  mutate(beta_region = case_when(Region_detail2=="NE"~"Northeast",
                                 Region_detail2=="SE"~"Southeast",
                                 Region_detail2=="AW"~"Arid West",
                                 Region_detail2=="WM"~"Western Mountains",
                                 Region_detail2=="CB"~"Caribbean",
                                 Region_detail2=="PNW"~"Pacific Northwest",
                                 Region_detail=="GP_S"~"Southern Great Plains",
                                 Region_detail %in% c("GP_U","GP_C","GP_N")~"Northern Great Plains"),
         beta_id = case_when(Region_detail2=="NE"~1,
                             Region_detail2=="SE"~2,
                             Region_detail2=="AW"~6,
                             Region_detail2=="WM"~5,
                             Region_detail2=="CB"~0,
                             Region_detail2=="PNW"~7,
                             Region_detail=="GP_S"~4,
                             Region_detail %in% c("GP_U","GP_C","GP_N")~3)) #%>%
  
xwalk_df$Region_detail

sdam_sf<-read_sf("NotForGit/Shapefiles/BetaSDAM_Regions/Major_SDAM_regions.shp")
mlra_sf<-read_sf("NotForGit/Shapefiles/MLRA_Regions/MLRA_Lower48.shp") %>% st_transform(crs=st_crs(sdam_sf)) %>%
  mutate(corps_region=Region,
         corps_id=case_when(corps_region=="USACE Eastern Mountains and Piedmont Region"~2,
                            corps_region=="USACE Atlantic and Gulf Coastal Plain Region"~3,
                            corps_region=="USACE Arid West Region"~7,
                            corps_region=="USACE Western Mountains, Valleys, and Coast Region"~6,
                            corps_region=="USACE Great Plains Region"~5,
                            corps_region=="USACE Northcentral and Northeast Region"~1,
                            corps_region=="USACE Midwest Region"~4,
                            corps_region=="USACE Caribbean Region"~0,
                            T~-99))
nwca_sf<-read_sf("NotForGit/Shapefiles/NWCA_Ecoregions/NWCA_Ecoregions-ECO5.shp") %>% st_transform(crs=st_crs(sdam_sf)) %>%
  mutate(nwca_region=NWCA_ECO5,
         nwca_id=case_when(nwca_region=="EMU"~2,
                           nwca_region=="CPL"~1,
                           nwca_region=="XER"~5,
                           nwca_region=="WMT"~4,
                           nwca_region=="IPL"~3,
                           T~0))
ohwm_sf<-read_sf("NotForGit/Shapefiles/OHWM_Regions/OHWM_Boundries.shp") %>% st_transform(crs=st_crs(sdam_sf)) %>%
  mutate(ohwm_region=case_when(SDAM=="Northern Great Plains"~"Northern Plains",
                               SDAM=="Southern Great Plains"~"Southern Plains",
                               T~SDAM),
         ohwm_id=case_when(ohwm_region=="Southeast"~2,
                           ohwm_region=="Northeast"~1,
                           ohwm_region=="Southwest"~6,
                           ohwm_region=="Pacific Northwest"~5, #DOES NOT EXIST IN OHWM!
                           ohwm_region=="Northern Plains"~3,
                           ohwm_region=="Southern Plains"~4,
                           T~0))
# eco_sf<-read_sf("NotForGit/Shapefiles/us_eco_l3/us_eco_l3.shp") %>% st_transform(crs=st_crs(sdam_sf))

xwalk_sf<-xwalk_df %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords=c("long","lat"), crs=4326, remove=F) %>%
  st_transform(crs=st_crs(sdam_sf))


xwalk_sf %>%
  st_join(ohwm_sf %>% select(ohwm_region, ohwm_id)) %>%
  # st_join(nwca_sf %>% select(nwca_region, nwca_id))
  st_join(mlra_sf %>% select(corps_region, corps_id)) 

write_csv(xwalk_sf %>%
            st_join(ohwm_sf %>% select(ohwm_region, ohwm_id)) %>%
            # st_join(nwca_sf %>% select(nwca_region, nwca_id))
            st_join(mlra_sf %>% select(corps_region, corps_id)) %>%
            as.data.frame() %>%
            select(-geometry), 
          file="Data/master_site_class_xwalk_030723_coordinates_REGIONS.csv")

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



  ggplot()+geom_sf(data=nwca_sf, aes(fill=nwca_region))
  
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
