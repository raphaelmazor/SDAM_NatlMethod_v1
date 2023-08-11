library(tidyverse)
library(skimr)
library(clipr)
######Define metrics
#Define sets of predictors suitable for different data sets
#PNW: Appropriate for PNW, West, GP, and East
#WGE: Appropriate for West, GP and East, but not PNW
#WG: Appropriate for West and GP, but not East or PNW



lumets<-read_csv("Data/metric_lookup.csv")

#############Import data
gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df_08012023.csv")

main_df_nopnw<-read_csv("NotForGit/Step1/main_df_step1.csv") %>%
  #Temporary fix while dupes are resolved
  group_by(SiteCode, CollectionDate) %>% slice_head(n=1) %>% ungroup()
# 
# junk<-main_df_nopnw %>%
#   group_by(SiteCode, CollectionDate) %>% tally() %>% 
#   filter(n>1) %>%
#   mutate(site_date = paste(SiteCode, CollectionDate))
# 
# main_df_nopnw %>%
#   mutate(site_date = paste(SiteCode, CollectionDate)) %>%
#   filter(site_date %in% junk$site_date) %>%
#   select(SiteCode, CollectionDate, Database, ParentGlobalID, all_of(setdiff(all_metrics, gis_metrics)))
#   # skim()


pnw_df_all<-read_csv("NotForGit/Step1/pnw_df_step1.csv") %>%
  mutate(Database="PNW",
         Region_DB="PNW",
         SiteCode=as.character(SiteCode)) 

pnw_df<-pnw_df_all %>%
  na.omit()
# skim_without_charts()
# filter(!is.na())

# main_df_nopnw %>%
#   bind_rows(pnw_df) %>%
#   left_join(gis_metrics_df) %>%
#   filter(is.na(tmin)) %>%
#   select(all_of(all_metrics_PNW)) %>%
#   skim_without_charts()
xwalk_df<-read_csv("NotForGit/Step1/xwalk_df.csv")
main_df <- main_df_nopnw %>%
  bind_rows(pnw_df) %>%
  inner_join(gis_metrics_df) %>%
  inner_join(xwalk_df %>%
               select(SiteCode=sitecode, DRNAREA_mi2))




lumets<-read_csv("Data/metric_lookup.csv") %>%
  filter(MetricSubtype!="Direct")
all_metrics<-lumets$Metric[which(lumets$MetricSubtype!="Direct")]
gis_metrics<-lumets$Metric[which(lumets$MetricType=="Geospatial")]
metrics_wg<-lumets$Metric[which(lumets$West & lumets$GP)] %>% as.vector()
metrics_wgp<-lumets$Metric[which(lumets$West & lumets$GP & lumets$PNW)] %>% as.vector()
metrics_wge<-lumets$Metric[which(lumets$West & lumets$GP & lumets$East)] %>% as.vector()
metrics_wgep<-lumets$Metric[which(lumets$West & lumets$GP & lumets$East & lumets$PNW)] %>% as.vector()
metrics_e<-lumets$Metric[which(lumets$East)] %>% as.vector()
metrics_ep<-lumets$Metric[which(lumets$East & lumets$PNW)] %>% as.vector()#Not sure this is needed
metrics_p<-lumets$Metric[which(lumets$PNW)] %>% as.vector() #Same as metrics_ep
lumets %>% group_by(West, GP, East, PNW) %>% tally()

mod_summary<- xwalk_df %>% 
  select(all_region,beta_region,ohwm_region,corps_region,nwca_region) %>%
  pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region,nwca_region), 
               names_to = "Stratification", values_to = "Strata") %>%
  crossing(IncludeGISPreds=c(T,F),
           IncludePNW=c(T,F)) %>%
  mutate(Stratf_Strat=paste(Stratification, Strata, sep="_"),
         ModName = case_when(IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW_GIS", sep="_"),
                             IncludeGISPreds & !IncludePNW~paste(Stratification, Strata, "GIS", sep="_"),
                             !IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW", sep="_"),
                             T~paste(Stratification, Strata, sep="_"))) %>%
  mutate(FlagNoPNW = case_when(
    Stratification == "all_region"~"OK",
    Strata=="Caribbean"~"Delete",
    Strata=="CB"~"Delete",
    
    Stratification == "beta_region" & Strata %in% c("PNW") & IncludePNW~"OK",
    Stratification == "beta_region" & Strata %in% c("PNW") & !IncludePNW~"Delete",
    Stratification == "beta_region" & !Strata %in% c("PNW") & IncludePNW~"Delete",
    Stratification == "beta_region" & !Strata %in% c("PNW") & !IncludePNW~"OK",
    
    Stratification == "corps_region" & Strata %in% c("AW","WMVC") & IncludePNW~"OK",
    Stratification == "corps_region" & Strata %in% c("AW","WMVC") & !IncludePNW~"OK",
    Stratification == "corps_region" & !Strata %in% c("AW","WMVC") & IncludePNW~"Delete",
    Stratification == "corps_region" & !Strata %in% c("AW","WMVC") & !IncludePNW~"OK",
    
    Stratification == "nwca_region" & Strata %in% c("XER","WMT") & IncludePNW~"OK",
    Stratification == "nwca_region" & Strata %in% c("XER","WMT") & !IncludePNW~"OK",
    Stratification == "nwca_region" & !Strata %in% c("XER","WMT") & IncludePNW~"Delete",
    Stratification == "nwca_region" & !Strata %in% c("XER","WMT") & !IncludePNW~"OK",
    
    Stratification == "ohwm_region" & Strata %in% c("Northwest") & IncludePNW~"OK",
    Stratification == "ohwm_region" & Strata %in% c("Northwest") & !IncludePNW~"OK",
    Stratification == "ohwm_region" & !Strata %in% c("Northwest") & IncludePNW~"Delete",
    Stratification == "ohwm_region" & !Strata %in% c("Northwest") & !IncludePNW~"OK"
    
    
  )) %>%
  filter(FlagNoPNW != "Delete") %>%
  mutate(RegionalDatasets = 
           case_when(Stratification=="all_region" & IncludePNW~"WGEP",
                     Stratification=="all_region" & !IncludePNW~"WGE",
                     Stratf_Strat %in% c("beta_region_Arid West", "beta_region_Western Mountains") ~"W",
                     Stratf_Strat %in% c("beta_region_Northeast", "beta_region_Southeast") ~"E",
                     Stratf_Strat %in% c("beta_region_Northern Great Plains", "beta_region_Southern Great Plains") ~"G",
                     Stratf_Strat %in% c("beta_region_PNW") ~"P",
                     Stratf_Strat %in% c("corps_region_AGCP") ~"E", #There are two sites in Texas GP, so it's not worth dropping the data
                     Stratf_Strat %in% c("corps_region_AW") & IncludePNW ~"WP", 
                     Stratf_Strat %in% c("corps_region_AW") & !IncludePNW ~"W",
                     Stratf_Strat %in% c("corps_region_EMP")  ~"E", 
                     Stratf_Strat %in% c("corps_region_GP")  ~"GE", #There are 22 eastern sites, so keep. There is 1 WM site, so drop
                     Stratf_Strat %in% c("corps_region_MW") ~"GE", #9 NE sites vs 41 NGP sites
                     Stratf_Strat %in% c("corps_region_NCNE") ~"GE", #Lots of sites in both regions
                     Stratf_Strat %in% c("corps_region_WMVC") & IncludePNW ~"WP", # A smattering of AW, GP sites for some reason
                     Stratf_Strat %in% c("corps_region_WMVC") & !IncludePNW ~"W",
                     Stratf_Strat %in% c("nwca_region_CPL") ~"E",#Only 4 GP sites. Not worth keeping
                     Stratf_Strat %in% c("nwca_region_EMU") ~"GE",#Sites in both regions
                     Stratf_Strat %in% c("nwca_region_IPL") ~"WGE",#Sites in multiple regions
                     Stratf_Strat %in% c("nwca_region_WMT") & IncludePNW ~"WGP", #A handful of GP sites 
                     Stratf_Strat %in% c("nwca_region_WMT") & !IncludePNW ~"WG",
                     Stratf_Strat %in% c("nwca_region_XER") & IncludePNW ~"WGP", #A handful of GP sites 
                     Stratf_Strat %in% c("nwca_region_XER") & !IncludePNW ~"WG",
                     Stratf_Strat %in% c("ohwm_region_Northeast") ~"E",#Sites in multiple regions
                     Stratf_Strat %in% c("ohwm_region_Northwest") & IncludePNW~"WGP", #A handfull of GP sites
                     Stratf_Strat %in% c("ohwm_region_Northwest") & !IncludePNW~"WG", #A handfull of GP sites
                     Stratf_Strat %in% c("ohwm_region_Northern Plains") ~"WGE",#1 AW site and 22 WM sites
                     Stratf_Strat %in% c("ohwm_region_Southeast") ~"E",
                     Stratf_Strat %in% c("ohwm_region_Southern Plains") ~"WG",
                     Stratf_Strat %in% c("ohwm_region_Southwest") ~"W",
                     T~"Other")) %>%
  filter(Stratification %in% c("beta_region","corps_region","all_region")) %>%
  filter(Strata %in% c("USA","Arid West","Western Mountains","AW","WMVC")) %>%
  mutate(RegionalDatasets=case_when(RegionalDatasets %in% c("WGE")~"W",
                                    RegionalDatasets %in% c("WGEP")~"WP",
                                    T~RegionalDatasets),
         ModName2 = case_when(Stratification=="all_region" & IncludePNW~"West (PNW)",
                              Stratification=="all_region" & !IncludePNW~"West",
                              Stratification=="corps_region" & Strata=="AW"~"Arid West (PNW)",
                              Stratification=="corps_region" & Strata=="WMVC"~"Western Mountains (PNW)",
                              Stratification=="beta_region" & Strata=="Arid West"~"Arid West",
                              Stratification=="beta_region" & Strata=="Western Mountains"~"Western Mountains",
                              T~"OTHER"),
         ModName2 = case_when(IncludeGISPreds~paste(ModName2,"(GIS)"),
                              T~ModName2)
  ) %>%
  filter(!(Stratification=="corps_region" & !IncludePNW)) 


mod_summary %>% filter(RegionalDatasets=="Other")%>% print(n=68)
mod_summary %>% group_by(RegionalDatasets) %>% tally()
xwalk_df %>%
  filter(ohwm_region=="Southwest") %>%
  group_by(beta_region) %>% tally()
# filter(beta_region=="Northern Great Plains")


# junk<-lumets #%>%  filter(MetricType!="Geospatial")
# 
# setdiff(junk$Metric, names(main_df))
# 
# main_long<-main_df %>%
#   select(Region_DB, all_of(junk$Metric)) %>%
#   pivot_longer(cols=all_of(junk$Metric))
# main_long %>%
#   group_by(Region_DB, name) %>%
#   summarise(n_tot = length(value),
#             length_not_na = sum(!is.na(value))) %>%
#   ungroup() %>%
#   mutate(pct_complete = length_not_na/n_tot) %>%
#   select(-n_tot, -length_not_na) %>%
#   mutate(Region_DB=paste0(Region_DB,"_Complete")) %>%
#   pivot_wider(names_from=Region_DB, values_from = pct_complete) %>%
#   rename(Metric=name)%>%
#   right_join(junk) %>%
#   write_clip()
# 

# main_df %>% skim_without_charts()
# main_df %>% group_by(Region_DB) %>%
#   select(Erosion_Deposition_Score, hydrophytes_present_any,RiparianCorridor_score, UplandRootedPlants_score2) %>%
#   skim()

xwalk_df<-read_csv("Data/master_site_class_xwalk_08012023_coordinates_REGIONS.csv")  %>%
  filter(!Region %in% c("CB")) %>%
  filter(Region %in% c("PNW","West"))

xwalk_df %>% group_by(ohwm_region) %>% tally()

xwalk_df %>%
  filter(Class!="U") %>%
  filter(Region_detail2!="CB") %>%
  # filter(Region_detail2!="PNW") %>%
  select(Class, beta_region, ohwm_region, corps_region, nwca_region) %>%
  pivot_longer(cols=c(beta_region, ohwm_region, corps_region, nwca_region),
               names_to="Regionalization",
               values_to="Region") %>%
  group_by(Regionalization, Region, Class) %>% 
  tally() %>%
  group_by(Regionalization, Region) %>% 
  mutate(ntot=sum(n),
         pct = n/ntot) %>%
  slice_min(pct, n=1) %>%
  arrange(pct) %>%
  select(-ntot)# %>%  clipr::write_clip()

#Continue to use the PNW minimum metric set to define complete cases
main_df2<-main_df %>%
  select(SiteCode, Region_DB, Database, CollectionDate, ParentGlobalID,
         all_of(metrics_p)) %>%
  select(-Fish_UpTo3) %>%
  na.omit() %>%
  inner_join(xwalk_df %>% 
               rename(SiteCode=sitecode) %>%
               select(-DRNAREA_mi2)) %>%
  filter(Class!="U") 
# main_df2 %>% group_by(Region_DB) %>% skim_without_charts()

library(skimr)
# main_df2 %>% 
# filter(ohwm_region %in% c("Northeast","Southeast")) %>%
# skim_without_charts()


visit_tally<-main_df2 %>%
  group_by(Region_DB,SiteCode, Class) %>%
  tally() %>%
  ungroup()
visit_tally_plot<-ggplot(data=visit_tally, aes(x=n, fill=Class))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_fill_brewer(palette="RdYlBu")+
  theme_bw()+xlab("# visits")
ggsave(visit_tally_plot, filename="Figures_VaryingPredictors_WEST/visit_tally_plot.png", height=6, width=6)

visit_tally_plot_regions<-ggplot(data=visit_tally, aes(x=n, fill=Class))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_fill_brewer(palette="RdYlBu")+
  theme_bw()+xlab("# visits")+
  facet_wrap(~Region_DB, ncol=1)
ggsave(visit_tally_plot_regions, filename="Figures_VaryingPredictors_WEST/visit_tally_plot_regions.png", height=6, width=6)

####Upsample to 6 per site
visit_tally$AtLeast3Samples<-visit_tally$n>=3

set.seed(1)
main_df2_GT3 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[visit_tally$AtLeast3Samples]) %>%
  group_by(SiteCode) %>%
  slice_sample(n=3, replace=F) %>%
  ungroup() %>%
  mutate(DataType="Original")


# main_df2_LT6 <- main_df2 %>%
#   filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast6Samples]) %>%
#   group_by(SiteCode) %>%
#   slice_sample(n=6, replace=T)

set.seed(1)
main_df2_LT3 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast3Samples]) %>%
  mutate(DataType="Original") %>%
  #If visited 2 times, add a random visit
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=1, replace=F) %>%
      mutate(DataType="Augmented") 
  ) %>%
  #If visited once, add triple visits again
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]) %>%
      mutate(DataType="Augmented") ,
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]) %>%
      mutate(DataType="Augmented") ,
  ) %>%
  ungroup()

main_df2_LT3 %>%
  group_by(SiteCode) %>% tally() %>%
  filter(n!=3)

main_df2_GT3 %>%
  group_by(SiteCode) %>% tally() %>%
  filter(n!=3)

main_df3<-main_df2_GT3 %>%
  bind_rows(main_df2_LT3) %>%
  mutate(Class = factor(Class, levels=c("E","I","P"))) %>%
  
  inner_join(    main_df %>% select(SiteCode, CollectionDate, all_of(metrics_wg )))
setdiff(all_metrics, names(main_df3))

main_df3 %>%
  select(SiteCode, Class, ohwm_region) %>%
  unique() %>%
  group_by(ohwm_region, Class) %>% tally()


main_df3 %>%
  group_by(SiteCode) %>% tally() %>%
  group_by(n) %>% tally()

#########MODELING
library(tidymodels)

# mod_summary<- xwalk_df %>% 
#   select(all_region,beta_region,ohwm_region,corps_region,nwca_region) %>%
#   pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region,nwca_region), 
#                names_to = "Stratification", values_to = "Strata") %>%
#   crossing(IncludeGISPreds=c(T,F),
#            IncludePNW=c(T,F)) %>%
#   mutate(Stratf_Strat=paste(Stratification, Strata, sep="_"),
#          ModName = case_when(IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW_GIS", sep="_"),
#                              IncludeGISPreds & !IncludePNW~paste(Stratification, Strata, "GIS", sep="_"),
#                              !IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW", sep="_"),
#                              T~paste(Stratification, Strata, sep="_"))) %>%
#   mutate(FlagNoPNW = case_when(
#     Stratification == "all_region"~"OK",
#     Strata=="Caribbean"~"Delete",
#     Strata=="CB"~"Delete",
#     
#     Stratification == "beta_region" & Strata %in% c("PNW") & IncludePNW~"OK",
#     Stratification == "beta_region" & Strata %in% c("PNW") & !IncludePNW~"Delete",
#     Stratification == "beta_region" & !Strata %in% c("PNW") & IncludePNW~"Delete",
#     Stratification == "beta_region" & !Strata %in% c("PNW") & !IncludePNW~"OK",
#     
#     Stratification == "corps_region" & Strata %in% c("AW","WMVC") & IncludePNW~"OK",
#     Stratification == "corps_region" & Strata %in% c("AW","WMVC") & !IncludePNW~"OK",
#     Stratification == "corps_region" & !Strata %in% c("AW","WMVC") & IncludePNW~"Delete",
#     Stratification == "corps_region" & !Strata %in% c("AW","WMVC") & !IncludePNW~"OK",
#     
#     Stratification == "nwca_region" & Strata %in% c("XER","WMT") & IncludePNW~"OK",
#     Stratification == "nwca_region" & Strata %in% c("XER","WMT") & !IncludePNW~"OK",
#     Stratification == "nwca_region" & !Strata %in% c("XER","WMT") & IncludePNW~"Delete",
#     Stratification == "nwca_region" & !Strata %in% c("XER","WMT") & !IncludePNW~"OK",
#     
#     Stratification == "ohwm_region" & Strata %in% c("Northwest") & IncludePNW~"OK",
#     Stratification == "ohwm_region" & Strata %in% c("Northwest") & !IncludePNW~"OK",
#     Stratification == "ohwm_region" & !Strata %in% c("Northwest") & IncludePNW~"Delete",
#     Stratification == "ohwm_region" & !Strata %in% c("Northwest") & !IncludePNW~"OK",
#   )) %>%
#   filter(FlagNoPNW != "Delete") %>%
#   mutate(RegionalDatasets = 
#            case_when(Stratification=="all_region" & IncludePNW~"WGEP",
#                      Stratification=="all_region" & !IncludePNW~"WGE",
#                      Stratf_Strat %in% c("beta_region_Arid West", "beta_region_Western Mountains") ~"W",
#                      Stratf_Strat %in% c("beta_region_Northeast", "beta_region_Southeast") ~"E",
#                      Stratf_Strat %in% c("beta_region_Northern Great Plains", "beta_region_Southern Great Plains") ~"G",
#                      Stratf_Strat %in% c("beta_region_PNW") ~"P",
#                      Stratf_Strat %in% c("corps_region_AGCP") ~"E", #There are two sites in Texas GP, so it's not worth dropping the data
#                      Stratf_Strat %in% c("corps_region_AW") & IncludePNW ~"WP", 
#                      Stratf_Strat %in% c("corps_region_AW") & !IncludePNW ~"W",
#                      Stratf_Strat %in% c("corps_region_EMP")  ~"E", 
#                      Stratf_Strat %in% c("corps_region_GP")  ~"GE", #There are 22 eastern sites, so keep. There is 1 WM site, so drop
#                      Stratf_Strat %in% c("corps_region_MW") ~"GE", #9 NE sites vs 41 NGP sites
#                      Stratf_Strat %in% c("corps_region_NCNE") ~"GE", #Lots of sites in both regions
#                      Stratf_Strat %in% c("corps_region_WMVC") & IncludePNW ~"WP", # A smattering of AW, GP sites for some reason
#                      Stratf_Strat %in% c("corps_region_WMVC") & !IncludePNW ~"W",
#                      Stratf_Strat %in% c("nwca_region_CPL") ~"E",#Only 4 GP sites. Not worth keeping
#                      Stratf_Strat %in% c("nwca_region_EMU") ~"GE",#Sites in both regions
#                      Stratf_Strat %in% c("nwca_region_IPL") ~"WGE",#Sites in multiple regions
#                      Stratf_Strat %in% c("nwca_region_WMT") & IncludePNW ~"WGP", #A handful of GP sites 
#                      Stratf_Strat %in% c("nwca_region_WMT") & !IncludePNW ~"WG",
#                      Stratf_Strat %in% c("nwca_region_XER") & IncludePNW ~"WGP", #A handful of GP sites 
#                      Stratf_Strat %in% c("nwca_region_XER") & !IncludePNW ~"WG",
#                      Stratf_Strat %in% c("ohwm_region_Northeast") ~"E",#Sites in multiple regions
#                      Stratf_Strat %in% c("ohwm_region_Northwest") & IncludePNW~"WGP", #A handfull of GP sites
#                      Stratf_Strat %in% c("ohwm_region_Northwest") & !IncludePNW~"WG", #A handfull of GP sites
#                      Stratf_Strat %in% c("ohwm_region_Northern Plains") ~"WGE",#1 AW site and 22 WM sites
#                      Stratf_Strat %in% c("ohwm_region_Southeast") ~"E",
#                      Stratf_Strat %in% c("ohwm_region_Southern Plains") ~"WG",
#                      Stratf_Strat %in% c("ohwm_region_Southwest") ~"W",
#                      T~"Other"))


# mod_met_sets<-
#   lumets %>%
#   crossing(mod_summary) %>%
#   mutate(
#     MetUse = 
#       case_when(
#         MetricType=="Geospatial" & IncludeGISPreds ~T,
#         MetricType=="Geospatial" & !IncludeGISPreds ~F,
#         PNW~T,
#         Stratification == "all_region" & IncludePNW & West & GP & East & PNW ~ T,
#         Stratification == "all_region" & IncludePNW & !PNW ~ F,
#         Stratification == "all_region" & !IncludePNW & West & GP & East  ~ T,
#         Stratification == "all_region" & !IncludePNW & !(West & GP & East)  ~ F,
#         T~NA,
#       )
#   )
# mod_met_sets %>% 
#   # filter(MetricType!="Geospatial") %>%
#   filter(Stratification=="all_region")%>%
#   filter(!IncludePNW & !PNW) %>%
#   as.data.frame() %>% head()

pnw_sites <-xwalk_df$sitecode[which(xwalk_df$beta_region=="PNW")]
main_df3x<-main_df3# %>%
# left_join(main_df %>%
# select(SiteCode, CollectionDate,
# all_of(setdiff(all_metrics, names(main_df3)))))
main_df3x %>% skim_without_charts()
mod_summary$RegionalDatasets %>% unique()





mod_dats<-lapply(1:nrow(mod_summary), function(i){
  stratf.i=mod_summary$Stratification[i]
  strat.i=mod_summary$Strata[i]
  pnw.i=mod_summary$IncludePNW[i]
  gis.i=mod_summary$IncludeGISPreds[i]
  dbs.i=mod_summary$RegionalDatasets[i]
  print(paste(stratf.i, strat.i, pnw.i, dbs.i))
  main_df3.i<-main_df3x %>%
    pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region,nwca_region),
                 names_to = "Stratification", values_to = "Strata") %>%
    filter(Stratification==stratf.i & Strata==strat.i)
  if(pnw.i)
    main_df3.i=main_df3.i
  else
    main_df3.i=main_df3.i %>% filter(!SiteCode %in% pnw_sites )
  
  #Figure out mets with 99% completeness
  met_completeness<-main_df3.i %>%
    select(SiteCode, CollectionDate, all_of(metrics_wg)) %>%
    pivot_longer(cols=all_of(metrics_wg)) %>%
    group_by(name) %>%
    summarise(n_complete = sum(!is.na(value)),
              n_total = length(SiteCode)) %>%
    ungroup() %>%
    mutate(PctComplete = n_complete/n_total)
  print(met_completeness %>% filter(PctComplete<.99))
  met_completeness<-met_completeness %>%
    filter(PctComplete>=.99)
  # print(met_completeness)
  main_df3.i=main_df3.i %>%
    select(SiteCode, Class, CollectionDate, DataType, all_of(met_completeness$name)) %>%
    na.omit()
  if(stratf.i=="all_region" & gis.i)
    main_df3.i=main_df3.i %>%
    inner_join(xwalk_df %>% select(SiteCode=sitecode, mlra=corps_region, beta=beta_region, ohwm=ohwm_region, nwca=nwca_region))
  else
    main_df3.i=main_df3.i
  if(gis.i)
    main_df3.i=main_df3.i
  else
    main_df3.i=main_df3.i %>% select(-all_of(gis_metrics))
  main_df3.i %>% na.omit()
})

skim(mod_dats[[1]])
mod_summary
setdiff(
  names(mod_dats[[1]]),
  names(mod_dats[[2]]))

set.seed(2)
mod_dats_split<-lapply(mod_dats, function(x){  
  x2 = x %>% select(SiteCode, Class) %>% unique()
  initial_split(x2, prop=4/5, strata=Class) })

mod_dats_training<-lapply(1:nrow(mod_summary), function(i){
  x=mod_dats_split[[i]]
  x2 = training(x)
  mod_dats[[i]] %>%     filter(SiteCode %in% x2$SiteCode)
})

setdiff(
  names(mod_dats_training[[1]]),
  names(mod_dats_training[[2]]))

mod_dats_testing<-lapply(1:nrow(mod_summary), function(i){
  x=mod_dats_split[[i]]
  x2 = testing(x)
  mod_dats[[i]] %>%     filter(SiteCode %in% x2$SiteCode)
})


mod_summary$n_training<-sapply(mod_dats_split, function(x){
  x %>% training() %>% nrow() })
mod_summary$n_training_P<-sapply(mod_dats_split, function(x){
  x %>% training() %>% filter(Class=="P") %>% nrow() })
mod_summary$n_training_I<-sapply(mod_dats_split, function(x){
  x %>% training() %>% filter(Class=="I") %>% nrow() })
mod_summary$n_training_E<-sapply(mod_dats_split, function(x){
  x %>% training() %>% filter(Class=="E") %>% nrow() })
mod_summary$n_testing<-sapply(mod_dats_split, function(x){
  x %>% testing() %>% nrow() })
mod_summary$n_testing_P<-sapply(mod_dats_split, function(x){
  x %>% testing() %>% filter(Class=="P") %>% nrow() })
mod_summary$n_testing_I<-sapply(mod_dats_split, function(x){
  x %>% testing() %>% filter(Class=="I") %>% nrow() })
mod_summary$n_testing_E<-sapply(mod_dats_split, function(x){
  x %>% testing() %>% filter(Class=="E") %>% nrow() })

#####Recursive feature eliminiation
# library(caret)
# my_ctrl <- rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "all")

#RFE is prohibitively slow
# maxsize_x<-21
# my_rfes_total<-lapply(1:nrow(mod_summary), function(i){
#   gis.i = mod_summary$IncludeGISPreds[i]
#   print(paste(i, mod_summary$ModName[i]))
#   
#   if(gis.i)
#     mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, GISPreds))) 
#   else
#     mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW)))
#   
#   set.seed(200+i)
#   rfe(Class~., 
#       data=mydat,
#       # sizes=c(1:maxsize_x),
#       sizes=c(3:20, 25, 30,40,50),
#       rfeControl = my_ctrl,
#       metric="Kappa", 
#       maximize = T )
# })

library(randomForest)
# sapply(mod_dats, function(x) length(names(x)))

my_rfs<-lapply(1:nrow(mod_summary), function(i){
  # lapply(3:5, function(i){
  stratf.i=mod_summary$Stratification[i]
  gis.i = mod_summary$IncludeGISPreds[i]
  print(paste(i, mod_summary$ModName[i]))
  mydat = mod_dats_training[[i]] %>% select(-CollectionDate, -SiteCode, -DataType)#,
  # -Region_DB, -Database, -Region, -Region_detail,
  # -Region_detail2, -Note, -beta_region,-ohwm_region, -corps_region,
  # -nwca_region, -all_region, -DataType,
  # -beta_id, -ohwm_id, -corps_id, -nwca_id,
  # -lat, -long)
  # print(mydat %>% skim_without_charts())
  
  # if(gis.i) #UPDATE TO CONDITIONALLY INCLUDE MLRA
  #   mydat = mod_dats_training[[i]] %>%  select(SiteCode, Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, GISPreds)))
  # else
  #   mydat = mod_dats_training[[i]] %>%  select(SiteCode, Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW)))
  # 
  # if(stratf.i=="all_region" & gis.i)
  #   mydat = mydat %>% 
  #   inner_join(xwalk_df %>% select(SiteCode=sitecode, mlra=corps_region)) %>%
  #   select(-SiteCode)
  # else
  #   mydat = mydat %>%    select(-SiteCode)
  # print("DRNAREA_mi2" %in% names(mydat))
  # print(skim(mydat))
  set.seed(200+i)
  rf.i=randomForest(Class~., 
                    data=mydat, 
                    importance=T,
                    proximity=T)
  if(!gis.i)
    rf.i
  if(gis.i & stratf.i!="all_region")
  {
    best2_gis_df = rf.i$importance %>%
      as_tibble() %>%
      mutate(myvar = row.names(rf.i$importance)) %>%
      filter(myvar %in% gis_metrics) %>%
      slice_max(MeanDecreaseAccuracy, n=2)
    print(best2_gis_df$myvar)
    # best_region_df = rf.i$importance %>%
    #   as_tibble() %>%
    #   mutate(myvar = row.names(rf.i$importance)) %>%
    #   filter(myvar %in% c("mlra","beta","ohwm","nwca")) %>%
    #   slice_max(MeanDecreaseAccuracy, n=1)
    # print(best_region_df$myvar)
    mydat2 = mydat %>% 
      select(-setdiff(gis_metrics, best2_gis_df$myvar))# %>%
    # select(-setdiff(c("mlra","beta","ohwm","nwca"),best_region_df$myvar))
    set.seed(300+i)
    rf.i=randomForest(Class~., 
                      data=mydat2,# %>% select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, best2_gis_df$myvar))), 
                      importance=T,
                      proximity=T)
  }
  if(gis.i & stratf.i=="all_region")
  {
    best2_gis_df = rf.i$importance %>%
      as_tibble() %>%
      mutate(myvar = row.names(rf.i$importance)) %>%
      filter(myvar %in% gis_metrics) %>%
      slice_max(MeanDecreaseAccuracy, n=2)
    print(best2_gis_df$myvar)
    best_region_df = rf.i$importance %>%
      as_tibble() %>%
      mutate(myvar = row.names(rf.i$importance)) %>%
      filter(myvar %in% c("mlra","beta","ohwm","nwca")) %>%
      slice_max(MeanDecreaseAccuracy, n=1)
    print(best_region_df$myvar)
    mydat2 = mydat %>% 
      select(-setdiff(gis_metrics, best2_gis_df$myvar)) %>%
      select(-setdiff(c("mlra","beta","ohwm","nwca"),best_region_df$myvar))
    set.seed(300+i)
    rf.i=randomForest(Class~., 
                      data=mydat2,# %>% select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, best2_gis_df$myvar))), 
                      importance=T,
                      proximity=T)
    
  }
  rf.i
  
})
mod_summary

setdiff(
  setdiff(my_rfs[[1]]$importance %>% row.names(),
          my_rfs[[2]]$importance %>% row.names()),
  setdiff(mod_dats[[1]] %>% names(),
          mod_dats[[2]] %>% names())
)

setdiff(my_rfs[[2]]$importance %>% row.names(),
        mod_dats[[2]] %>% names())



####
#Importance

rf_sum_importance<-lapply(1:nrow(mod_summary), function(i){
  gis.i=mod_summary$IncludeGISPreds[i]
  pnw.i=mod_summary$IncludePNW[i]
  # flag.i=mod_summary$FlagNoPNW[i]
  regz.i=mod_summary$Stratification[i]
  reg.i=mod_summary$Strata[i]
  mod.i=my_rfs[[i]]
  # print(mod.i)
  xmat=mod.i$importance
  xdf=xmat %>%
    as_tibble() %>%
    mutate(Regionalization=regz.i,
           Region_id=reg.i,
           GIS=case_when(gis.i~"GIS",T~"No GIS"),
           PNW = case_when(pnw.i~"PNW",T~"No PNW"),
           PNWtf=pnw.i,
           # Flag=flag.i,
           Metric=row.names(xmat)) %>%
    rename(P_imp=P, I_imp=I, E_imp=E)
  
}) %>% bind_rows() %>%
  mutate(MetricType=case_when(Metric %in% lumets$Metric[which(lumets$MetricType=="Geomorph")]~"Geomorphic",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Geospatial")]~"GIS",
                              Metric %in% c("mlra","beta","nwca","ohwm")~"GIS_spatial",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Hydro")]~"Hydro",
                              Metric %in% lumets$Metric[which(lumets$MetricType=="Biology")]~"Bio",
                              T~"Other" ),
         RegLabel = paste(Regionalization, Region_id, sep="-"))

rf_sum_importance %>% group_by(MetricType) %>% tally()
imp_plot_dat<-rf_sum_importance %>%
  mutate(MeanDecreaseAccuracy=case_when(MeanDecreaseAccuracy<0~0,T~MeanDecreaseAccuracy)) %>%
  arrange(MeanDecreaseAccuracy)
imp_plot_dat$Metric<-factor(imp_plot_dat$Metric, levels=unique(imp_plot_dat$Metric))

met_colors_df<-imp_plot_dat %>%
  select(MetricType, Metric) %>%
  mutate(MetricColor = case_when(MetricType =="Bio"~"#4daf4a",
                                 MetricType =="Hydro"~"#377eb8",
                                 MetricType =="Geomorphic"~"#d95f02",
                                 MetricType =="GIS"~"#7570b3",
                                 
  ))


variable_importance_plot_WEST<-
  ggplot(imp_plot_dat %>% 
           mutate(Region_id = 
                    case_when(Regionalization=="all_region" & PNWtf~"West (PNW)",
                              Regionalization=="all_region" & !PNWtf~"West",
                              Region_id=="Arid West" ~ "Arid West",
                              Region_id=="AW" ~ "Arid West (PNW)",
                              Region_id=="Western Mountains" ~ "Western Mountains",
                              Region_id=="WMVC" ~ "Western Mountains (PNW)",
                              T~Region_id) %>%
                    factor(levels=c("West","Arid West", "Western Mountains",
                                    "West (PNW)","Arid West (PNW)","Western Mountains (PNW)"))) %>%
           group_by(Region_id) %>%
           mutate(RankMDA = rank(MeanDecreaseAccuracy)/sum(!is.na(MeanDecreaseAccuracy))) %>%
           ungroup(), 
         aes(x=Region_id, y=Metric, fill=RankMDA))+
  geom_tile(color="white")+
  scale_fill_viridis_c(na.value = "gray", name="Rank\nImportance")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(.~GIS, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  # ggtitle("Variable importance")+
  xlab("")+ylab("")
ggsave(variable_importance_plot_WEST, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_WEST.png",
       height=7.5, width=12)
# 
# variable_importance_plot_pnw<-ggplot(imp_plot_dat %>% 
#                                        group_by(Regionalization, Region_id, GIS) %>%
#                                        slice_max(PNWtf, n=1), 
#                                      aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Includes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_pnw, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_pnw.png", height=15, width=9)
# 
# variable_importance_plot_pnw_gis<-ggplot(imp_plot_dat %>% 
#                                            group_by(Regionalization, Region_id, GIS) %>%
#                                            slice_max(PNWtf, n=1) %>%
#                                            filter(GIS=="GIS"), 
#                                          aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Includes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_pnw_gis, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_pnw_gis.png", height=15, width=9)
# 
# variable_importance_plot_pnw_xgis<-ggplot(imp_plot_dat %>% 
#                                             group_by(Regionalization, Region_id, GIS) %>%
#                                             slice_max(PNWtf, n=1) %>%
#                                             filter(GIS!="GIS"), 
#                                           aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Includes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_pnw_xgis, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_pnw_xgis.png", height=15, width=9)
# 
# 
# 
# variable_importance_plot_xpnw<-ggplot(imp_plot_dat %>% 
#                                         group_by(Regionalization, Region_id, GIS) %>%
#                                         slice_min(PNWtf, n=1), 
#                                       aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Excludes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_xpnw, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_xpnw.png", height=15, width=9)
# 
# 
# variable_importance_plot_xpnw_gis<-ggplot(imp_plot_dat %>% 
#                                             group_by(Regionalization, Region_id, GIS) %>%
#                                             slice_min(PNWtf, n=1) %>% filter(GIS=="GIS"), 
#                                           aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Excludes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_xpnw_gis, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_xpnw_gis.png", height=15, width=9)
# 
# 
# variable_importance_plot_xpnw_xgis<-ggplot(imp_plot_dat %>% 
#                                              group_by(Regionalization, Region_id, GIS) %>%
#                                              slice_min(PNWtf, n=1) %>% filter(GIS!="GIS"), 
#                                            aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
#   geom_tile(color="white")+
#   scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
#   # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
#   facet_grid(GIS~Regionalization, scales="free", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
#         panel.grid = element_blank())+
#   ggtitle("Variable importance", subtitle =  "Excludes PNW data")+
#   xlab("")+ylab("")
# ggsave(variable_importance_plot_xpnw_xgis, filename="Figures_VaryingPredictors_WEST/variable_importance_plot_xpnw_xgis.png", height=15, width=9)

####################
west_sites<-xwalk_df$sitecode[xwalk_df$Region=="West"]

mod_summary$ErrRate_all<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["OOB"]})
mod_summary$ErrRate_P<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["P"]})
mod_summary$ErrRate_I<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["I"]})
mod_summary$ErrRate_E<-  sapply(my_rfs, function(x){ x$err.rate %>% tail(1)["E"]})
junk<-my_rfs[[1]]
predict(junk, type="class")

my_predicted_classes_training<-lapply(1:nrow(mod_summary), function(i){
  print(paste(i, mod_summary$ModName[i]))
  rf.i=my_rfs[[i]]
  xdf=mod_dats_training[[i]] %>%
    mutate(ModName=mod_summary$ModName[i],
           Class_predicted = predict(rf.i, type="class")) %>%
    bind_cols(
      predict(rf.i, type="prob") %>%
        as_tibble() %>%
        transmute(
          probE = E, 
          probI = I,
          probP = P,
          probALI = E+I,
          Class_pred50 = case_when(probP>=.5~"P",
                                   probI>=.5~"I",
                                   probE>=.5~"E",
                                   probALI>=.5~"ALI",
                                   T~"NMI")
          
        )
    ) %>%
    mutate( Class_pred50_best = case_when(Class_pred50 == "NMI"~Class_predicted,
                                          T~Class_pred50),
            SiteSet="Training")
})


my_predicted_classes_testing<-lapply(1:nrow(mod_summary), function(i){
  print(paste(i, mod_summary$ModName[i]))
  rf.i=my_rfs[[i]]
  xdf=mod_dats_testing[[i]] %>%
    mutate(ModName=mod_summary$ModName[i])
  xdf <-xdf %>% 
    mutate(Class_predicted = predict(rf.i, type="class", newdata=xdf))
  xdf<-xdf %>%
    bind_cols(
      predict(rf.i, type="prob", newdata=xdf) %>%
        as_tibble() %>%
        transmute(
          probE = E, 
          probI = I,
          probP = P,
          probALI = E+I,
          Class_pred50 = case_when(probP>=.5~"P",
                                   probI>=.5~"I",
                                   probE>=.5~"E",
                                   probALI>=.5~"ALI",
                                   T~"NMI")
          
        )
    ) %>%
    mutate( Class_pred50_best = case_when(Class_pred50 == "NMI"~Class_predicted,
                                          T~Class_pred50),
            SiteSet="Testing")
})


my_predicted_classes_combined<-lapply(1:length(my_predicted_classes_training), function(i){
  my_predicted_classes_training[[i]] %>%
    bind_rows(
      my_predicted_classes_testing[[i]]
    )
})

my_predicted_classes_combined[[1]] %>% group_by(Class) %>% tally()

mod_summary$NMI_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%    filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="NMI")/length(xdf$Class_pred50_best)
})

mod_summary$P_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="P")/length(xdf$Class_pred50_best)
})
mod_summary$I_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="I")/length(xdf$Class_pred50_best)
})
mod_summary$ALI_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="ALI")/length(xdf$Class_pred50_best)
})
mod_summary$E_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%     filter(SiteCode %in% west_sites)
  sum(xdf$Class_pred50_best =="E")/length(xdf$Class_pred50_best)
})

outcomes_plot_dat<-mod_summary %>%
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName2,
         P_freq, I_freq, ALI_freq, E_freq, NMI_freq) %>%
  pivot_longer(cols=(ends_with("freq"))) %>%
  mutate(name=factor(name, levels=c("P_freq","I_freq","ALI_freq","E_freq", "NMI_freq")))

outcomes_plot_dat %>% group_by(name) %>% summarise(maximum = max(value))

outcomes_plot<-  ggplot(data=outcomes_plot_dat, aes(x=ModName2, y=value))+
  geom_bar(aes(fill=name), stat="identity") +
  scale_fill_manual(values=c("#08519c","#6baed6","#8856a7","#f768a1","orange"))+
  coord_flip()+
  xlab("")+ylab("% of samples")
ggsave(outcomes_plot, filename="Figures_VaryingPredictors_WEST/outcomes_plot.png", height=8, width=6)


mod_summary$Accuracy_PvIvE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary$Accuracy_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary$Accuracy_EvALI_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary$Accuracy_EvALI_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary$Precision_PvIvE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(DataType=="Original") %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) 
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Precision_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) 
  
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})


mod_summary$Precision_EvALI_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Precision_EvALI_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    mutate(Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"))
  xdf2<-xdf %>% 
    group_by(SiteCode) %>%
    mutate(n_orig = length(SiteCode)) %>%
    filter(n_orig>1) %>%
    group_by(SiteCode, n_orig, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/n_orig) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Accuracy_PnotE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_PnotE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(SiteCode %in% west_sites) %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_testing[is.na(mod_summary$Accuracy_EnotP_testing)]<-0
mod_summary_long<-mod_summary %>%
  pivot_longer(cols=c(contains("Accuracy"), contains("Precision"))) %>%
  mutate(MetricType2 = case_when(str_detect(name,"Accuracy")~"Accuracy",
                                 str_detect(name,"Precision")~"Precision",
                                 T~"Other"),
         Comparison = case_when(str_detect(name,"PvIvE")~"PvIvE",
                                str_detect(name,"EvALI")~"EvALI",
                                str_detect(name,"PnotE")~"PnotE",
                                str_detect(name,"EnotP")~"EnotP",
                                T~"Other"),
         SiteSet = case_when(str_detect(name, "training")~"Training",
                             T~"Testing"),
         Metric = paste(MetricType2, Comparison, sep="_")
  ) %>%
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName,ModName2, SiteSet,
         n_training, n_testing,
         MetricType2, Comparison,Metric, name, value
  ) %>%
  mutate(Strata2 = 
           case_when(Stratification=="all_region" & IncludePNW~"West (PNW)",
                     Stratification=="all_region" & !IncludePNW~"West",
                     Strata=="Arid West" ~ "Arid West",
                     Strata=="AW" ~ "Arid West (PNW)",
                     Strata=="Western Mountains" ~ "Western Mountains",
                     Strata=="WMVC" ~ "Western Mountains (PNW)",
                     T~"STRATA") %>%
           factor(levels=c("West","West (PNW)",
                           "Arid West", "Arid West (PNW)",
                           "Western Mountains", "Western Mountains (PNW)")))

accuracy_performance_metrics_WEST<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Accuracy") , 
                                          aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST.png", 
       height=7.5, width=6)

accuracy_performance_metrics_WEST2<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Accuracy") %>% filter(Comparison %in% c("PvIvE","EvALI")) , 
                                           aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST2.png", 
       height=7.5, width=6)
accuracy_performance_metrics_WEST2x<-ggplot(data=mod_summary_long %>% 
                                              filter(MetricType2=="Accuracy") %>% 
                                              filter(Comparison %in% c("EnotP","PnotE")) , 
                                            aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2x, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST2x.png", 
       height=7.5, width=6)

precision_performance_metrics_WEST<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Precision") , 
                                           aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Precision")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(precision_performance_metrics_WEST, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_WEST.png", 
       height=7.5, width=6)

# all_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
#                                       group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                       slice_max(IncludePNW, n=1), 
#                                     aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Metric~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Performance")+xlab("")
# ggsave(all_performance_metrics_pnw, filename="Figures_VaryingPredictors_WEST/all_performance_metrics_pnw.png", height=7.5, width=13)
# # 
# # accuracy_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
# #          filter(MetricType2=="Accuracy") %>%
# #          group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
# #          slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
# #          mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
# #        aes(x=Strata, y=value))+
# #   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
# #   scale_size_manual(values=c(1,2))+
# #   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
# #   theme_bw()+
# #   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
# #   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
# #   ylab("Accuracy")+xlab("")
# # ggsave(accuracy_performance_metrics_pnw, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_pnw.png", height=7.5, width=9)
# # 
# # accuracy_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
# #                                            filter(MetricType2=="Accuracy") %>%
# #                                            group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
# #                                            slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
# #                                            mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
# #                                          aes(x=Strata, y=value))+
# #   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
# #   scale_size_manual(values=c(1,2))+
# #   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
# #   theme_bw()+
# #   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
# #   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
# #   ylab("Accuracy")+xlab("")
# # ggsave(accuracy_performance_metrics_xpnw, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_xpnw.png", height=7.5, width=9)
# # 
# # 
# # precision_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
# #          filter(MetricType2=="Precision") %>%
# #          group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
# #          slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
# #          mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
# #        aes(x=Strata, y=value))+
# #   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
# #   scale_size_manual(values=c(1,2))+
# #   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
# #   theme_bw()+
# #   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
# #   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
# #   ylab("Precision")+xlab("")
# # ggsave(precision_performance_metrics_pnw, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_pnw.png", height=5, width=9)
# # 
# # precision_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
# #                                             filter(MetricType2=="Precision") %>%
# #                                             group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
# #                                             slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
# #                                             mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
# #                                           aes(x=Strata, y=value))+
# #   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
# #   scale_size_manual(values=c(1,2))+
# #   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
# #   theme_bw()+
# #   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
# #   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
# #   ylab("Precision")+xlab("")
# # ggsave(precision_performance_metrics_xpnw, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_xpnw.png", height=5, width=9)


mod_summary_long_across_strata<-mod_summary_long %>%
  group_by(Stratification, IncludeGISPreds, IncludePNW, SiteSet,
           MetricType2, Metric, Comparison, name) %>%
  summarise(value_unweighted = mean(value),
            lowest_value = min(value)) %>%
  ungroup()

mod_summary %>%
  filter(is.na(Accuracy_EnotP_testing)) %>% as.data.frame()
# 
# accuracy_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
#                                            filter(MetricType2=="Accuracy") %>%
#                                            group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                            slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                            mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#                                          aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   geom_hline(data=mod_summary_long_across_strata %>%
#                group_by(Stratification, IncludeGISPreds, SiteSet) %>%
#                slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                filter(MetricType2=="Accuracy") %>%
#                mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#              aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")
# ggsave(accuracy_performance_metrics_pnw, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_pnw.png", height=7.5, width=9)
# 
# accuracy_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
#                                             filter(MetricType2=="Accuracy") %>%
#                                             group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                             slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                             mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#                                           aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   geom_hline(data=mod_summary_long_across_strata %>%
#                group_by(Stratification, IncludeGISPreds, SiteSet) %>%
#                slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                filter(MetricType2=="Accuracy") %>%
#                mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#              aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")
# ggsave(accuracy_performance_metrics_xpnw, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_xpnw.png", height=7.5, width=9)
# 
# precision_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
#                                             filter(MetricType2=="Precision") %>%
#                                             group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                             slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                             mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#                                           aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   geom_hline(data=mod_summary_long_across_strata %>%
#                filter(MetricType2=="Precision") %>%
#                group_by(Stratification, IncludeGISPreds, SiteSet) %>%
#                slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#              aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Precision")+xlab("")
# ggsave(precision_performance_metrics_pnw, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_pnw.png", height=6, width=9)
# 
# precision_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
#                                              filter(MetricType2=="Precision") %>%
#                                              group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                              slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#                                            aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   geom_hline(data=mod_summary_long_across_strata %>%
#                filter(MetricType2=="Precision") %>%
#                group_by(Stratification, IncludeGISPreds, SiteSet) %>%
#                slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#              aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Precision")+xlab("")
# ggsave(precision_performance_metrics_xpnw, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_xpnw.png", height=6, width=9)


mod_summary_long %>%
  filter(Comparison=="EnotP" & Stratification=="corps_region" & !IncludeGISPreds & SiteSet=="Testing")
##################


my_predicted_classes_testing[[26]] %>%
  group_by(SiteCode, Class, Class_pred50_best) %>%
  filter(Class=="E") %>%
  tally()


#####################
#Subpop-level accuracy assessments

#What are my subpops?
#EPA regions?
library(sf)
epa_sf<-st_read("NotForGit/Shapefiles/Environmental_Protection_Agency_(EPA)_Regions/Environmental_Protection_Agency_(EPA)_Regions.shp")
#USACE divisions?
usace_sf<-st_read("NotForGit/Shapefiles/USACE_Civil_Works_Divisions/USACE_Divisions.shp")
xwalk_sf<-xwalk_df %>%
  st_as_sf(coords=c("long","lat"),
           crs=4326) %>%
  st_transform(crs=st_crs(epa_sf)) %>%
  st_join(epa_sf %>%
            transmute(epa_reg = gsub("Region ","EPA_", EPAREGION))) %>%
  st_transform(crs=st_crs(usace_sf)) %>%
  st_join(usace_sf %>%
            transmute(usace_reg = DIV_SYM)) 

#Map of sites over usace divisions
usace_divisions_map_sites_WEST<-ggplot()+
  geom_sf(data=usace_sf, aes(fill=DIV_SYM))+
  geom_sf(data=xwalk_sf)+
  scale_fill_viridis_d(guide="none", option="cividis", begin=.1, end=.9)+
  coord_sf(xlim=c(-124.15433, -103.41054),
           ylim=c(29.43645, 48.93045))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
ggsave(usace_divisions_map_sites_WEST, filename="Figures_VaryingPredictors_WEST/usace_divisions_map_sites_WEST.png", height=4, width=5)

xwalk_df2<-xwalk_sf %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(epa_reg = case_when(epa_reg %in% c("EPA_1","EPA_2")~"EPA_12",T~epa_reg),
         STATE = case_when(Region=="PNW"~"PNW",
                           T~str_sub(sitecode, 1,2)))

xwalk_df2 %>% group_by(usace_reg) %>% tally()
xwalk_df2 %>% group_by(epa_reg) %>% tally()
xwalk_df2 %>% group_by(STATE, Class) %>% tally() %>%
  pivot_wider(names_from=Class, values_from = n, values_fill = 0) %>% clipr::write_clip()


mod_summary_assessment_strata <-  mod_summary %>%
  select(Stratification, Strata, ModName, IncludeGISPreds, IncludePNW, ModName2) %>%
  unique() %>%
  crossing(AssessmentStratum=xwalk_df2$STATE %>% unique()) %>%
  filter(!AssessmentStratum %in% c("PNW")) 

mod_summary_assessment_strata$n_sites <- sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & 
             mod_summary$Strata==strat.i & 
             mod_summary$IncludeGISPreds==gis.i & 
             mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i))
  nrow(xdf)
})
mod_summary_assessment_strata %>% filter(n_sites==0) #There are no AW sites in SD. There are no WM sites in TX

mod_summary_assessment_strata<- mod_summary_assessment_strata %>%
  # crossing(IncludeGISPreds=c(T,F)) %>%
  filter(n_sites>0)



mod_summary_assessment_strata$n_P_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="P") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_P_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="P") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_I_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="I") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_I_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="I") 
  nrow(xdf)
})

mod_summary_assessment_strata$n_E_training<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="E") 
  nrow(xdf)
})
mod_summary_assessment_strata$n_E_testing<-  sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="E") 
  nrow(xdf)
})


mod_summary_assessment_strata$Correct_PvIvE_training<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
  # ifelse(is.na(y),0,y)
})


mod_summary_assessment_strata$Correct_PvIvE_testing<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)
})

mod_summary_assessment_strata$Correct_EvALI_training<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Training") %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})


mod_summary_assessment_strata$Correct_EvALI_testing<-sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, STATE) %>%
                 filter(STATE==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})

# mod_summary_assessment_strata %>% filter(AssessmentStratum=="SPD" & Stratification=="beta_region" & IncludeGISPreds )


mod_summary_assessment_strata_long<-mod_summary_assessment_strata %>%
  select(-n_sites) %>%
  pivot_longer(cols=c(starts_with("n"), starts_with("Correct"))) %>%
  group_by(Stratification, IncludeGISPreds, IncludePNW, AssessmentStratum, name) %>%
  summarise(value=sum(value)) %>%
  # filter(AssessmentStratum=="SPD" & Stratification=="beta_region")
  pivot_wider(names_from="name", values_from = "value", values_fill = 0) %>%
  ungroup() %>%
  mutate(
    n_training = (n_P_training + n_I_training + n_E_training ),
    n_testing = (n_P_testing + n_I_testing + n_E_testing ),
    Accuracy_PvIvE_training = Correct_PvIvE_training/n_training,
    Accuracy_PvIvE_testing = Correct_PvIvE_testing/n_testing,
    Accuracy_EvALI_training = Correct_EvALI_training/n_training,
    Accuracy_EvALI_testing = Correct_EvALI_testing/n_testing
  ) %>%
  select(-starts_with("Correct"), -starts_with("n_")) %>%
  pivot_longer(cols=starts_with("Accuracy")) %>%
  mutate(MetricType2 = case_when(str_detect(name,"Accuracy")~"Accuracy",
                                 str_detect(name,"Precision")~"Precision",
                                 T~"Other"),
         Comparison = case_when(str_detect(name,"PvIvE")~"PvIvE",
                                str_detect(name,"EvALI")~"EvALI",
                                str_detect(name,"PnotE")~"PnotE",
                                str_detect(name,"EnotP")~"EnotP",
                                T~"Other"),
         SiteSet = case_when(str_detect(name, "training")~"Training",
                             T~"Testing"),
         Metric = paste(MetricType2, Comparison, sep="_"),
         value=case_when(is.na(value)~0,T~value),
         
         Stratification2 = case_when(Stratification=="all_region" & IncludePNW~"West (PNW)",
                                     Stratification=="all_region" & !IncludePNW~"West",
                                     Stratification=="beta_region"~"Beta",
                                     Stratification=="corps_region"~"MLRA (PNW)"),
         AssessmentStratum2 = case_when(Stratification=="all_region"& IncludePNW~paste(AssessmentStratum, "West (PNW)"),
                                        Stratification=="all_region" & !IncludePNW~paste(AssessmentStratum,"West"),
                                        Stratification=="beta_region"~paste(AssessmentStratum,"Beta"),
                                        Stratification=="corps_region"~paste( AssessmentStratum, "MLRA (PNW)"))
  ) 
mod_summary_assessment_strata_long %>% filter(is.na(AssessmentStratum2))
# mod_summary_assessment_strata_long_sum<-mod_summary_assessment_strata_long %>%
#   group_by(Stratification, IncludeGISPreds,  AssessmentStratum,
#            name, MetricType2, Metric, Comparison, SiteSet) %>%
#   summarise(value_unweighted = mean(value),
#             value_weighted = weighted.mean(value=n_sites),
#             value_lowest=min(value)) %>%
#   ungroup()

mod_summary_assessment_strata_long %>%
  filter(Stratification=="beta_region" & 
           AssessmentStratum=="SD" & IncludeGISPreds & 
           SiteSet=="Testing")
mod_summary_assessment_strata %>%
  filter(Stratification=="beta_region" &  AssessmentStratum=="SPD" & IncludeGISPreds & Strata %in% c("Arid West","PNW"))


subpop_accuracy_plot_WEST<-  ggplot(data=mod_summary_assessment_strata_long %>%
                                      filter(MetricType2=="Accuracy") %>%
                                      filter(AssessmentStratum!="SD" & SiteSet=="Training") %>%
                                      # group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>%
                                      # slice_max(IncludePNW, n=1) %>%
                                      mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
                                    aes(x=Stratification2 , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~AssessmentStratum, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  scale_y_continuous(limits=c(.5,1))
ggsave(subpop_accuracy_plot_WEST, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_WEST.png", height=6, width=10)


subpop_accuracy_plot_WEST_STATES<-
  ggplot(data=mod_summary_assessment_strata_long %>%
           filter(MetricType2=="Accuracy") %>%
           filter(AssessmentStratum!="SD" ) %>%
           # group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>%
           # slice_max(IncludePNW, n=1) %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification2, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  scale_y_continuous(limits=c(.5,1))
ggsave(subpop_accuracy_plot_WEST_STATES, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_WEST_STATES.png", height=6, width=10)

# 
# 
# subpop_accuracy_plot_pnw<-ggplot(data=mod_summary_assessment_strata_long %>%
#                                    filter(MetricType2=="Accuracy") %>%
#                                    group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>% 
#                                    slice_max(IncludePNW, n=1) %>% 
#                                    mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#                                  aes(x=AssessmentStratum , y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
#   # geom_hline(data=mod_summary_long_across_strata %>%
#   #              filter(MetricType2=="Accuracy") %>%
#   #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#   #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")+
#   scale_y_continuous(limits=c(.5,1))
# ggsave(subpop_accuracy_plot_pnw, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_pnw.png", height=6, width=7.5)
# 
# subpop_accuracy_plot_xpnw<-ggplot(data=mod_summary_assessment_strata_long %>%
#                                     filter(MetricType2=="Accuracy") %>%
#                                     group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>% 
#                                     slice_min(IncludePNW, n=1) %>% 
#                                     mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#                                   aes(x=AssessmentStratum , y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
#   # geom_hline(data=mod_summary_long_across_strata %>%
#   #              filter(MetricType2=="Accuracy") %>%
#   #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#   #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
#   scale_linetype_manual(values=c("dotted","dashed"))+
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")+
#   scale_y_continuous(limits=c(.5,1))
# ggsave(subpop_accuracy_plot_xpnw, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_xpnw.png", height=6, width=7.5)
# 
# subpop_accuracy_plot_diffpnw<-mod_summary_assessment_strata_long %>%
#   mutate(ModelData = case_when(IncludePNW ~"Includes_PNW", T~"Excludes_PNW")) %>%
#   select(-IncludePNW) %>%
#   pivot_wider(names_from=ModelData, values_from = value) %>%
#   mutate(DeltaInclude = Includes_PNW - Excludes_PNW) %>%
#   ggplot(
#     aes(x=AssessmentStratum, y=DeltaInclude)  )+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free", drop=T)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Differences in Accuracy\n(With minus Without PNW)")+xlab("")+
#   geom_hline(yintercept=0)+
#   scale_y_continuous(breaks=(seq(from=-.8, to=.4, by=.1)))
# ggsave(subpop_accuracy_plot_diffpnw, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_diffpnw.png", height=6, width=7.5)


write_csv(mod_summary, "Figures_VaryingPredictors_WEST/mod_summary.csv")
write_csv(mod_summary_assessment_strata, "Figures_VaryingPredictors_WEST/mod_summary_strata.csv")
write_csv(mod_summary_assessment_strata_long, "Figures_VaryingPredictors_WEST/mod_summary_assessment_strata_long.csv")
write_csv(mod_summary_long, "Figures_VaryingPredictors_WEST/mod_summary_long.csv")
write_csv(mod_summary_long_across_strata, "Figures_VaryingPredictors_WEST/mod_summary_long_across_strata.csv")


#############

#PNW Metrics
main_df3$BMI_presence==1
main_df3$Ephemeroptera_SumOfIndividuals==2 #6 or more
main_df3$PerennialPNWMacroPresent==1
main_df3$Slope
main_df3$hydrophytes_present_any
main_df3$Fish_PA
main_df3$AmphSnake_PA

#BetaAW metrics
main_df3$hydrophytes_present
main_df3$TotalAbundance
main_df3$EPT_taxa
main_df3$AlgalCover_LiveOrDead_NoUpstream
main_df3$Fish_PA

#NM method metrics
main_df3$WaterInChannel_score 
main_df3$Fish_score_NM
main_df3$BMI_score
main_df3$AlgaeAbundance_NM
main_df3$UplandRootedPlants_score
main_df3$RifflePoolSeq_score
main_df3$SubstrateSorting_score
main_df3$HydricSoils_score
main_df3$SedimentOnPlantsDebris_score
main_df3$SeepsSprings_yn 
main_df$ironox_bfscore_NM

nm_varz <-c("WaterInChannel_score","Fish_score_NM","BMI_score","AlgaeAbundance_NM","UplandRootedPlants_score",
            "RifflePoolSeq_score","SubstrateSorting_score","HydricSoils_score","SedimentOnPlantsDebris_score",
            "SeepsSprings_yn","ironox_bfscore_NM")

main_df3 %>%
  inner_join(main_df %>% select(ParentGlobalID, WaterInChannel_score, ironox_bfscore_NM, SedimentOnPlantsDebris_score,SeepsSprings_yn) %>% unique()) %>%
  filter(Region=="West") %>% 
  mutate(STATE = str_sub(SiteCode, 1,2),
         ironox_bfscore_NM = case_when(is.na(ironox_bfscore_NM)~0, T~ironox_bfscore_NM),
         SeepsSprings_score = case_when(is.na(SeepsSprings_yn)~0,
                                        SeepsSprings_yn=="notdetected"~0,
                                        SeepsSprings_yn=="present"~1.5,
         )) %>%
  select(all_of(nm_varz)) %>% skim()


main_df3_OtherSDAMs<-main_df3 %>%
  inner_join(main_df %>% select(ParentGlobalID, WaterInChannel_score, ironox_bfscore_NM, SedimentOnPlantsDebris_score,SeepsSprings_yn) %>% unique()) %>%
  filter(Region=="West") %>% 
  mutate(STATE = str_sub(SiteCode, 1,2),
         ironox_bfscore_NM = case_when(is.na(ironox_bfscore_NM)~0, T~ironox_bfscore_NM),
         SeepsSprings_score = case_when(is.na(SeepsSprings_yn)~0,
                                        SeepsSprings_yn %in% c("notdetected","notdectected")~0,
                                        SeepsSprings_yn=="present"~1.5,
         ) ) %>%
  rowwise() %>%
  mutate(NM_ClassScore = sum(WaterInChannel_score, Fish_score_NM, BMI_score, AlgaeAbundance_NM, RiparianCorridor_score, UplandRootedPlants_score,
                             Sinuosity_score, ChannelDimensions_score , RifflePoolSeq_score, SubstrateSorting_score,HydricSoils_score, SedimentOnPlantsDebris_score,
                             SeepsSprings_score,ironox_bfscore_NM, na.rm=T)) %>%
  ungroup() %>%
  mutate(
    # NM_ClassScore = WaterInChannel_score + Fish_score_NM +BMI_score +AlgaeAbundance_NM +RiparianCorridor_score +UplandRootedPlants_score+
    # Sinuosity_score+ChannelDimensions_score+RifflePoolSeq_score+SubstrateSorting_score+HydricSoils_score+SedimentOnPlantsDebris_score+
    # SeepsSprings_score+ironox_bfscore_NM,
    NM_ClassPrelim = case_when(NM_ClassScore<9~"E",
                               NM_ClassScore<12~"It",
                               NM_ClassScore<=19~"I",
                               NM_ClassScore<=22~"Pt",
                               NM_ClassScore>22~"P"),
    NM_ClassNoSI = case_when(NM_ClassScore<9~"E",
                             NM_ClassScore<12~"I",
                             NM_ClassScore<=19~"I",
                             NM_ClassScore<=22~"P",
                             NM_ClassScore>22~"P"),
    NM_Class = case_when(NM_ClassNoSI %in% c("E") & Fish_PA==1~"ALI",
                         NM_ClassNoSI %in% c("E") & BMI_score>0~"ALI",
                         T~NM_ClassNoSI),
    PNW_Class_noSI = case_when(BMI_presence==1 & Ephemeroptera_SumOfIndividuals==2 & PerennialPNWMacroPresent==1 ~"P",
                               BMI_presence==1 & Ephemeroptera_SumOfIndividuals==2 & PerennialPNWMacroPresent==0 & Slope>=16 ~"P",
                               BMI_presence==1 & Ephemeroptera_SumOfIndividuals==2 & PerennialPNWMacroPresent==0 & Slope<16 ~"I",
                               BMI_presence==1 & Ephemeroptera_SumOfIndividuals<2~"I",
                               BMI_presence==0 & hydrophytes_present_any==1 & Slope>=10.5~"E",
                               BMI_presence==0 & hydrophytes_present_any==1 & Slope<10.5~"I",
                               BMI_presence==0 & hydrophytes_present_any==0~"E",
                               T~"OTHER"),
    PNW_Class = case_when(PNW_Class_noSI=="E" & Fish_PA==1 ~ "ALI",
                          PNW_Class_noSI=="E" & AmphSnake_PA==1 ~ "ALI",
                          T~PNW_Class_noSI),
    BetaAW_Class = case_when(hydrophytes_present==0 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==0~"E",
                             hydrophytes_present==0 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==0~"NMI",
                             hydrophytes_present==0 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>2 ~"ALI",
                             
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==0~"NMI",
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==0~"NMI",
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>2~"ALI",
                             
                             hydrophytes_present==0 & TotalAbundance<=19 & EPT_taxa>=1 ~"ALI",
                             
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==0~"NMI",
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==0~"NMI",
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream<=2 & Fish_PA==1~"ALI",
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>2~"ALI",
                             
                             hydrophytes_present==0 & TotalAbundance>=20 & EPT_taxa>=1 ~"ALI",
                             
                             hydrophytes_present<=2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==0~"NMI",
                             hydrophytes_present<=2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==1~"ALI",
                             hydrophytes_present<=2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>0 ~"ALI",
                             
                             hydrophytes_present<=2 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 ~"I",
                             hydrophytes_present<=2 & TotalAbundance<=19 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>0 ~"ALI",
                             
                             hydrophytes_present<=2 & TotalAbundance<=19 & EPT_taxa>0 ~"ALI",
                             
                             hydrophytes_present<=2 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 ~"I",
                             hydrophytes_present<=2 & TotalAbundance>=20 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>0 ~"ALI",
                             hydrophytes_present<=2 & TotalAbundance>=20 & EPT_taxa>0 & AlgalCover_LiveOrDead_NoUpstream==0 ~"ALI",
                             hydrophytes_present<=2 & TotalAbundance>=20 & EPT_taxa>0 & AlgalCover_LiveOrDead_NoUpstream>0 ~"I",
                             
                             hydrophytes_present>2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==0 ~"NMI",
                             hydrophytes_present>2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream==0 & Fish_PA==1 ~"ALI",
                             hydrophytes_present>2 & TotalAbundance==0 & EPT_taxa==0 & AlgalCover_LiveOrDead_NoUpstream>0  ~"ALI",
                             
                             hydrophytes_present>2 & TotalAbundance<=19 & EPT_taxa==0 ~"ALI",
                             hydrophytes_present>2 & TotalAbundance<=19 & EPT_taxa>0 ~"P",
                             hydrophytes_present>2 & TotalAbundance>=20 & EPT_taxa==0 ~"ALI",
                             hydrophytes_present>2 & TotalAbundance>=20 & EPT_taxa>0 ~"P",
                             T~"OTHER"),
    
  )


main_df3_OtherSDAMs %>%
  group_by(Region_detail2,Class, NM_Class) %>% 
  tally() %>%
  mutate(PvIvE_correct = case_when(Class == "E" & NM_Class=="E"~T,
                                   Class == "E" & NM_Class %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & NM_Class %in% c("I","ALI")~T,
                                   Class == "I" & NM_Class %in% c("E","P","NMI")~F,
                                   Class == "P" & NM_Class %in% c("P","ALI")~T,
                                   Class == "P" & NM_Class %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & NM_Class=="E"~T,
                                  Class == "E" & NM_Class %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & NM_Class %in% c("I","ALI","P")~T,
                                  Class == "I" & NM_Class %in% c("E","NMI")~F,
                                  Class == "P" & NM_Class %in% c("I", "P","ALI")~T,
                                  Class == "P" & NM_Class %in% c("E","NMI")~F),
  ) %>%
  group_by(Region_detail2, PvIvE_correct) %>%
  summarise(n=sum(n))

main_df3_OtherSDAMs %>%
  group_by(Region_detail2,Class, BetaAW_Class) %>% 
  tally() %>%
  mutate(PvIvE_correct = case_when(Class == "E" & BetaAW_Class=="E"~T,
                                   Class == "E" & BetaAW_Class %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & BetaAW_Class %in% c("I","ALI")~T,
                                   Class == "I" & BetaAW_Class %in% c("E","P","NMI")~F,
                                   Class == "P" & BetaAW_Class %in% c("P","ALI")~T,
                                   Class == "P" & BetaAW_Class %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & BetaAW_Class=="E"~T,
                                  Class == "E" & BetaAW_Class %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & BetaAW_Class %in% c("I","ALI","P")~T,
                                  Class == "I" & BetaAW_Class %in% c("E","NMI")~F,
                                  Class == "P" & BetaAW_Class %in% c("I", "P","ALI")~T,
                                  Class == "P" & BetaAW_Class %in% c("E","NMI")~F),
  ) %>%
  group_by(Region_detail2, EvALI_correct) %>%
  summarise(n=sum(n))



main_df3_OtherSDAMs %>%
  group_by(Region_detail2, Class, PNW_Class) %>% 
  tally() %>%
  mutate(PvIvE_correct = case_when(Class == "E" & PNW_Class=="E"~T,
                                   Class == "E" & PNW_Class %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & PNW_Class %in% c("I","ALI")~T,
                                   Class == "I" & PNW_Class %in% c("E","P","NMI")~F,
                                   Class == "P" & PNW_Class %in% c("P","ALI")~T,
                                   Class == "P" & PNW_Class %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & PNW_Class=="E"~T,
                                  Class == "E" & PNW_Class %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & PNW_Class %in% c("I","ALI","P")~T,
                                  Class == "I" & PNW_Class %in% c("E","NMI")~F,
                                  Class == "P" & PNW_Class %in% c("I", "P","ALI")~T,
                                  Class == "P" & PNW_Class %in% c("E","NMI")~F),
  ) %>%
  group_by(Region_detail2,EvALI_correct) %>%
  summarise(n=sum(n))


####
#WM models
#Load final simplified RF models
load("NotForGit/WMModels/SnowDomModel_Simplified.Rdata")
load("NotForGit/WMModels/NonSnowDomModel_Simplified.Rdata")
# write_csv(metrics_df, "NotForGit/5.0_Model Refinement/metrics_df.csv")
WM_metrics_df<-read_csv("NotForGit/WMModels/metrics_df.csv")

#Required predictors to run the model
SnowDomModel_Simplified_varz<-SnowDomModel_Simplified$importance %>% row.names()
NonSnowDomModel_Simplified_varz<-NonSnowDomModel_Simplified$importance %>% row.names()

# main_df3_OtherSDAMs$m

main_df3_OtherSDAMs2<-main_df3_OtherSDAMs %>%
  inner_join(main_df %>% select(Mosquitofish, SiteCode, CollectionDate) %>% unique()) %>%
  inner_join(
    gis_metrics_df %>%
      select(SiteCode,  ppt.m05, ppt.m10, tmax, MeanSnowPersistence_10)
  ) %>%
  mutate(PerennialTaxa_cat = case_when(perennial_PNW_taxa==0~0,
                                       perennial_PNW_taxa<=1~1,
                                       perennial_PNW_taxa<=2~2,
                                       T~3),
         TotalAbundance_cat = case_when(TotalAbundance==0~0,
                                        TotalAbundance<20~1, #Same as AW
                                        T~2),
         PerennialAbundance_cat = case_when(perennial_PNW_abundance==0~0,
                                            perennial_PNW_abundance<=6~1,
                                            T~2),
         BankWidth_cat = case_when(BankWidthMean<2~0,
                                   BankWidthMean<6~1,
                                   T~2),
         Algae_cat = case_when(AlgalCover_LiveOrDead_NoUpstream<=2~2,
                               T~AlgalCover_LiveOrDead_NoUpstream),
         mayfly_cat= case_when(Ephemeroptera_abundance==0~0,
                               Ephemeroptera_abundance<=5~1,
                               Ephemeroptera_abundance<=10~2,
                               Ephemeroptera_abundance<=15~3,
                               T~4),
         SI_Fish = case_when(Fish_PA>0~1,T~0),
         SI_Algae = case_when(AlgalCover_LiveOrDead_NoUpstream>2~1,T~0),
         SI_Hydric=HydricSoils_score,
         DifferencesInVegetation_score=RiparianCorridor_score, #CHECK
         alglivedead_cover_score=AlgalCover_LiveOrDead_NoUpstream,
         fishabund_score2=case_when(Mosquitofish=="yes"~0,
                                    T~Fish_score_NM  )
  )

setdiff(NonSnowDomModel_Simplified_varz, names(main_df3_OtherSDAMs2))


wm_snow_classprobs<-
  predict(SnowDomModel_Simplified, newdata= main_df3_OtherSDAMs2, type="prob") %>% 
  as.data.frame() %>%
  rename(WM_Snow_E = E, WM_Snow_I=I, WM_Snow_P=P) %>%
  mutate(SiteCode=main_df3_OtherSDAMs2$SiteCode,
         CollectionDate=main_df3_OtherSDAMs2$CollectionDate,
         WM_Snow_ALI = WM_Snow_I+WM_Snow_P,
         WM_Snow_Class = case_when(
           WM_Snow_P>=.5~"P",
           WM_Snow_I>=.5~"I",
           WM_Snow_E>=.5~"E",
           WM_Snow_ALI>=.5~"ALI",
           T~"NMI"
         ))

wm_nosnow_classprobs<-
  predict(NonSnowDomModel_Simplified, newdata= main_df3_OtherSDAMs2, type="prob") %>% 
  as.data.frame() %>%
  rename(WM_NoSnow_E = E, WM_NoSnow_I=I, WM_NoSnow_P=P) %>%
  mutate(SiteCode=main_df3_OtherSDAMs2$SiteCode,
         CollectionDate=main_df3_OtherSDAMs2$CollectionDate,
         WM_NoSnow_ALI = WM_NoSnow_I+WM_NoSnow_P,
         WM_NoSnow_Class = case_when(
           WM_NoSnow_P>=.5~"P",
           WM_NoSnow_I>=.5~"I",
           WM_NoSnow_E>=.5~"E",
           WM_NoSnow_ALI>=.5~"ALI",
           T~"NMI"
         ))

main_df3_OtherSDAMs3<-main_df3_OtherSDAMs2 %>%
  # filter(ParentGlobalID!="{47e54336-e847-4f71-a45d-d45fd36bca1b}") %>%
  inner_join(wm_nosnow_classprobs %>% unique()) %>%
  inner_join(wm_snow_classprobs %>% unique()) %>%
  mutate(BetaWM_Class = case_when(MeanSnowPersistence_10<25~WM_NoSnow_Class,
                                  MeanSnowPersistence_10>=25~WM_Snow_Class),
         BetaWM_Class = case_when(BetaWM_Class %in% c("E","NMI") & Fish_PA==1~"ALI", T~BetaWM_Class),
         BetaWM_probE= case_when(MeanSnowPersistence_10<25~WM_NoSnow_E,
                                 MeanSnowPersistence_10>=25~WM_Snow_E),
         BetaWM_probI= case_when(MeanSnowPersistence_10<25~WM_NoSnow_I,
                                 MeanSnowPersistence_10>=25~WM_Snow_I),
         BetaWM_probP= case_when(MeanSnowPersistence_10<25~WM_NoSnow_P,
                                 MeanSnowPersistence_10>=25~WM_Snow_P),
         BetaAWWM_Class = case_when(beta_region=="Arid West"~BetaAW_Class,
                                    beta_region=="Western Mountains"~BetaWM_Class,
                                    T~"Other"))
main_df3_OtherSDAMs3 %>% group_by(Region_detail2, BetaAWWM_Class) %>% tally()

main_df3_OtherSDAMs3 %>%
  group_by(Region_detail2,Class, BetaWM_Class) %>% 
  tally() %>%
  mutate(PvIvE_correct = case_when(Class == "E" & BetaWM_Class=="E"~T,
                                   Class == "E" & BetaWM_Class %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & BetaWM_Class %in% c("I","ALI")~T,
                                   Class == "I" & BetaWM_Class %in% c("E","P","NMI")~F,
                                   Class == "P" & BetaWM_Class %in% c("P","ALI")~T,
                                   Class == "P" & BetaWM_Class %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & BetaWM_Class=="E"~T,
                                  Class == "E" & BetaWM_Class %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & BetaWM_Class %in% c("I","ALI","P")~T,
                                  Class == "I" & BetaWM_Class %in% c("E","NMI")~F,
                                  Class == "P" & BetaWM_Class %in% c("I", "P","ALI")~T,
                                  Class == "P" & BetaWM_Class %in% c("E","NMI")~F),
  ) %>%
  group_by(Region_detail2, EvALI_correct) %>%
  summarise(n=sum(n))
###########
#Add Betas to performance plots


beta_accuracy_plot_dat<-mod_summary_long %>% filter(MetricType2=="Accuracy") %>%
  select(-Stratification, -Strata, -ModName, -ModName2, -n_training, -n_testing, -name, -MetricType2, -Metric)

pnw_accuracy_plot_dat<-  crossing(
  IncludeGISPreds=F,
  IncludPNW=T,
  SiteSet="Training",
  Comparison=c("PvIvE","EvALI"),
  Strata2 = c("PNW method_West","PNW method_AW", "PNW method_WM")
)


beta_accuracy_plot_dat1<-main_df3_OtherSDAMs3 %>%
  # group_by(Region_detail2, Class, PNW_Class) %>% 
  # tally() %>%
  select(SiteCode, Region_detail2, STATE, Class,
         # BetaAW_Class, BetaWM_Class, PNW_Class, NM_Class) %>%
         BetaAWWM_Class, PNW_Class, NM_Class) %>%
  # pivot_longer(cols=c(BetaAW_Class, BetaWM_Class, PNW_Class, NM_Class), names_to = "Method", values_to = "MethodClass", values_drop_na = T) %>%
  pivot_longer(cols=c(BetaAWWM_Class, PNW_Class, NM_Class), names_to = "Method", values_to = "MethodClass", values_drop_na = T) %>%
  mutate(PvIvE_correct = case_when(Class == "E" & MethodClass=="E"~T,
                                   Class == "E" & MethodClass %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & MethodClass %in% c("I","ALI")~T,
                                   Class == "I" & MethodClass %in% c("E","P","NMI")~F,
                                   Class == "P" & MethodClass %in% c("P","ALI")~T,
                                   Class == "P" & MethodClass %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & MethodClass=="E"~T,
                                  Class == "E" & MethodClass %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & MethodClass %in% c("I","ALI","P")~T,
                                  Class == "I" & MethodClass %in% c("E","NMI")~F,
                                  Class == "P" & MethodClass %in% c("I", "P","ALI")~T,
                                  Class == "P" & MethodClass %in% c("E","NMI")~F),
         EnotP_correct= Class=="E" & MethodClass %in% c("E","I","ALI"),
         PnotE_correct= Class=="P" &  MethodClass %in% c("P","I","ALI"),
  ) %>%
  crossing(Strata2=c("West","AW","WM")) %>%
  group_by(Method) %>%
  summarise(PvIvE_WEST=sum(PvIvE_correct)/length(PvIvE_correct ),
            PvIvE_AW=sum(PvIvE_correct[Region_detail2=="AW"])/length(PvIvE_correct[Region_detail2=="AW"] ),
            PvIvE_WM=sum(PvIvE_correct[Region_detail2=="WM"])/length(PvIvE_correct[Region_detail2=="WM"] ),
            EvALI_WEST=sum(EvALI_correct)/length(EvALI_correct ),
            EvALI_AW=sum(EvALI_correct[Region_detail2=="AW"])/length(EvALI_correct[Region_detail2=="AW"] ),
            EvALI_WM=sum(EvALI_correct[Region_detail2=="WM"])/length(EvALI_correct[Region_detail2=="WM"] ),
            EnotP_WEST=sum(EnotP_correct)/length(EnotP_correct[Class=="E" & MethodClass!="NMI"] ),
            EnotP_AW=sum(EnotP_correct[Region_detail2=="AW"])/length(EnotP_correct[Region_detail2=="AW" & Class=="E" & MethodClass!="NMI"] ),
            EnotP_WM=sum(EnotP_correct[Region_detail2=="WM"])/length(EnotP_correct[Region_detail2=="WM" & Class=="E" & MethodClass!="NMI"] ),
            PnotE_WEST=sum(PnotE_correct)/length(PnotE_correct[Class=="P" & MethodClass!="NMI"] ),
            PnotE_AW=sum(PnotE_correct[Region_detail2=="AW"])/length(PnotE_correct[Region_detail2=="AW"& Class=="P" & MethodClass!="NMI"] ),
            PnotE_WM=sum(PnotE_correct[Region_detail2=="WM"])/length(PnotE_correct[Region_detail2=="WM" & Class=="P" & MethodClass!="NMI"] ),
            
  ) %>%
  ungroup() %>%
  pivot_longer(cols=c(starts_with("PvIvE"),starts_with("EvALI"), starts_with("PnotE"), starts_with("EnotP"))) %>%
  mutate(Comparison = case_when(str_detect(name, "PvIvE")~"PvIvE",
                                str_detect(name, "EvALI")~"EvALI",
                                str_detect(name, "EnotP")~"EnotP",
                                str_detect(name, "PnotE")~"PnotE",
                                T~"Other"),
         Strata2 =   case_when(str_detect(name, "WEST")~"West",
                               str_detect(name, "AW")~"AW",
                               str_detect(name, "WM")~"WM",
                               T~"Other"),
         Method2 = str_remove_all(Method, "_Class"))



accuracy_performance_metrics_WEST2_BETAS<-ggplot(data=beta_accuracy_plot_dat1 %>% filter(Comparison %in% c("PvIvE","EvALI")), 
                                                 aes(x=Method2, y=value))+
  geom_point(aes(color=Strata2))+ 
  # scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="Where measured?")+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
accuracy_performance_metrics_WEST2_BETAS
ggsave(accuracy_performance_metrics_WEST2_BETAS, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST2_BETAS.png", 
       height=7.5, width=6)


accuracy_performance_metrics_WEST2_BETAS2<-ggplot(data=beta_accuracy_plot_dat1 %>% filter(Comparison %in% c("PnotE","EnotP")), 
                                                  aes(x=Method2, y=value))+
  geom_point(aes(color=Strata2))+ 
  # scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="Where measured?")+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
accuracy_performance_metrics_WEST2_BETAS2
ggsave(accuracy_performance_metrics_WEST2_BETAS2, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST2_BETAS2.png", 
       height=7.5, width=6)



accuracy_performance_metrics_WEST2x<-ggplot(data=mod_summary_long %>% 
                                              # filter(MetricType2=="Accuracy") %>% 
                                              filter(Comparison %in% c("EnotP","PnotE")) , 
                                            aes(x=Strata2, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~., scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle=0))
ggsave(accuracy_performance_metrics_WEST2x, filename="Figures_VaryingPredictors_WEST/accuracy_performance_metrics_WEST2x.png", 
       height=7.5, width=6)
# 
# precision_performance_metrics_WEST<-ggplot(data=mod_summary_long %>% filter(MetricType2=="Precision") , 
#                                            aes(x=Strata2, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~., scales="free_x", space="free")+
#   theme_bw()+
#   geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Precision")+xlab("")+
#   theme(legend.position = "bottom",
#         strip.text.y = element_text(angle=0))
# ggsave(precision_performance_metrics_WEST, filename="Figures_VaryingPredictors_WEST/precision_performance_metrics_WEST.png", 
#        height=7.5, width=6)



beta_accuracy_plot_dat2<-main_df3_OtherSDAMs3 %>%
  # group_by(Region_detail2, Class, PNW_Class) %>% 
  # tally() %>%
  select(SiteCode, Region_detail2, STATE, Class,
         # BetaAW_Class, BetaWM_Class, PNW_Class, NM_Class) %>%
         BetaAWWM_Class, PNW_Class, NM_Class) %>%
  # pivot_longer(cols=c(BetaAW_Class, BetaWM_Class, PNW_Class, NM_Class), names_to = "Method", values_to = "MethodClass", values_drop_na = T) %>%
  pivot_longer(cols=c(BetaAWWM_Class, PNW_Class, NM_Class), names_to = "Method", values_to = "MethodClass", values_drop_na = T) %>%
  mutate(PvIvE_correct = case_when(Class == "E" & MethodClass=="E"~T,
                                   Class == "E" & MethodClass %in% c("I","ALI","P","NMI")~F,
                                   Class == "I" & MethodClass %in% c("I","ALI")~T,
                                   Class == "I" & MethodClass %in% c("E","P","NMI")~F,
                                   Class == "P" & MethodClass %in% c("P","ALI")~T,
                                   Class == "P" & MethodClass %in% c("E","I","NMI")~F),
         EvALI_correct= case_when(Class == "E" & MethodClass=="E"~T,
                                  Class == "E" & MethodClass %in% c("I","ALI","P","NMI")~F,
                                  Class == "I" & MethodClass %in% c("I","ALI","P")~T,
                                  Class == "I" & MethodClass %in% c("E","NMI")~F,
                                  Class == "P" & MethodClass %in% c("I", "P","ALI")~T,
                                  Class == "P" & MethodClass %in% c("E","NMI")~F),
         EnotP_correct= Class=="E" & MethodClass %in% c("E","I","ALI"),
         PnotE_correct= Class=="P" &  MethodClass %in% c("P","I","ALI"),
  ) %>%
  group_by(Method, STATE) %>%
  summarise(PvIvE=sum(PvIvE_correct)/length(PvIvE_correct ),
            EvALI=sum(EvALI_correct)/length(EvALI_correct ),
            # EnotP=sum(EnotP_correct)/length(EnotP_correct[Class=="E" & MethodClass!="NMI"] ),
            # PnotE=sum(PnotE_correct)/length(PnotE_correct[Class=="P" & MethodClass!="NMI"] )
  ) %>%
  ungroup() %>%
  pivot_longer(cols=c(starts_with("PvIvE"),starts_with("EvALI"), starts_with("PnotE"), starts_with("EnotP"))) %>%
  mutate(Comparison = case_when(str_detect(name, "PvIvE")~"PvIvE",
                                str_detect(name, "EvALI")~"EvALI",
                                str_detect(name, "EnotP")~"EnotP",
                                str_detect(name, "PnotE")~"PnotE",
                                T~"Other"),
         # Strata2 =   case_when(str_detect(name, "WEST")~"West",
         #                       str_detect(name, "AW")~"AW",
         #                       str_detect(name, "WM")~"WM",
         #                       T~"Other"),
         Method2 = str_remove_all(Method, "_Class"))


subpop_accuracy_plot_WEST_STATES_BETAS<-
  ggplot(data=beta_accuracy_plot_dat2 %>%
           
           filter(STATE!="SD" ) %>%
           
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=STATE , y=value))+
  geom_point()+ 
  geom_path(aes(group=1))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  # scale_linetype_manual(values=c("dotted","dashed"))+
  # scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Method, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")
subpop_accuracy_plot_WEST_STATES_BETAS
ggsave(subpop_accuracy_plot_WEST_STATES_BETAS, filename="Figures_VaryingPredictors_WEST/subpop_accuracy_plot_WEST_STATES_BETAS.png", height=6, width=10)


combo_consistency_dat<-beta_accuracy_plot_dat2 %>%
  filter(STATE!="SD" )%>%
  bind_rows(
    mod_summary_assessment_strata_long %>%
      filter(MetricType2=="Accuracy" &
               SiteSet=="Training") %>%
      filter(AssessmentStratum!="SD" ) %>%
      transmute(Method=Stratification2,
             STATE=AssessmentStratum,
             name,
             value,
             Comparison,
             Method2=case_when(IncludeGISPreds~paste(Stratification2,"GIS"),
                               T~Stratification2),
             
      )
  ) %>%
  mutate(Type=case_when(Method2 %in% c("BetaAW","BetaWM","NM","PNW")~"Old",
                        Method2 %in% c("Beta","Beta GIS",
                                       "MLRA (PNW)","MLRA (PNW) GIS", 
                                       "West", "West GIS",
                                       "West (PNW)", "West (PNW) GIS")~"New",
                        T~"Other"))


combo_consistency_dat %>%
  filter(Method2 =="NM")
old_v_new_methods_consistency_plot<-ggplot(data=combo_consistency_dat, aes(x=STATE, y=value, color=Method2, linetype=Type))+
  geom_point()+
  geom_path(aes(group=Method2))+
  facet_wrap(~Comparison, ncol=1)+
  theme_bw()
ggsave(old_v_new_methods_consistency_plot, filename="Figures_VaryingPredictors_WEST/old_v_new_methods_consistency_plot.png",
       height=6, width=8)

ggplot(data=combo_consistency_dat, aes(x=Method2, y=value, color=Method2, linetype=Type))+
  # geom_point()+
  # geom_path(aes(group=Method2))+
  geom_boxplot()+
  facet_wrap(~Comparison, ncol=1)+
  theme_bw()

combo_consistency_dat %>%
  group_by(Method2, Type) %>% tally() 

# subpop_accuracy_plot_WEST_STATES<-
  ggplot(data=combo_consistency_dat %>%
           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
         aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  geom_path(data = . %>% filter(SiteSet=="Training"),
            aes( color=IncludeGISPreds, group=IncludeGISPreds))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification2, scales="free_x", space="free")+
  theme_bw()+
  # theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  scale_y_continuous(limits=c(.5,1))