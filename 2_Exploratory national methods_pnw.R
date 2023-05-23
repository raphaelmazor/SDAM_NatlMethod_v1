library(tidyverse)
library(skimr)
library(clipr)
######Define metrics
#Define sets of predictors suitable for different data sets

#BIOLOGICAL PREDICTORS

BioPreds_PNW<-c(
  #Aquatic invertebrates
  "BMI_presence",
  "Ephemeroptera_SumOfIndividuals","Plecoptera_SumOfIndividuals","Trichoptera_SumOfIndividuals",
  "Basommatophora_SumOfIndividuals","Odonata_SumOfIndividuals","Coleoptera_SumOfIndividuals",
  "Hemiptera_SumOfIndividuals","Decapoda_SumOfIndividuals","OtherMacro_SumOfIndividuals",
  "EPT_SumOfIndividuals","OCH_SumOfIndividuals","MacroGroupsPresent","PerennialPNWMacroPresent",
  #Vegetation
  "hydrophytes_present_any","UplandRootedPlants_score2","RiparianCorridor_score",
  #algae
  
  #Fish
  "Fish_PA",
  #Other bio
  "ironox_bfscore_PNW","Amphibian_presence","AlgaeAbundanceMossCover_Score2")
  
#HYDROLOGIC INDICATORS
HydroPreds_PNW<-c("springs_score_NM","SoilMoist_MaxScore","HydricSoils_score")
WaterPreds_PNW<-c("springs_score_NM","SoilMoist_MaxScore")
HydroPreds_Indirect_PNW<-setdiff(HydroPreds_PNW, WaterPreds_PNW)

#GEOMORPH INDICATORS
GeomorphPreds_PNW<-c(
  "SubstrateSorting_score","Sinuosity_score",  "RifflePoolSeq_score",
  "BankWidthMean","Slope"#"erosion_score","floodplain_score"
)
#GIS Predictors
#These predictors are applicable to the entire data set, regarldess of data collection SOP
GISPreds<-c(#"Eco1","Eco2","Eco3",
  "tmean", "tmax", "tmin", 
  "ppt", 
  # "ppt.m01", "ppt.m02", "ppt.m03", "ppt.m04", "ppt.m05", "ppt.m06", "ppt.m07", "ppt.m08", "ppt.m09", "ppt.m10", "ppt.m11", "ppt.m12", 
  # "temp.m01", "temp.m02", "temp.m03", "temp.m04", "temp.m05", "temp.m06", "temp.m07", "temp.m08", "temp.m09", "temp.m10", "temp.m11", "temp.m12", 
  "Elev_m", "DRNAREA_mi2", 
  "MeanSnowPersistence_10", "MeanSnowPersistence_05", "MeanSnowPersistence_01", 
  "SnowDom_SP10", "SnowDom_SP05", "SnowDom_SP01",
  "ppt.234", "ppt.567", "ppt.8910", "ppt.11121", 
  "temp.234", "temp.567", "temp.8910", "temp.11121"
)



all_metrics_PNW<-c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, GISPreds)

#############Import data
gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df.csv")

main_df_nopnw<-read_csv("NotForGit/Step1/main_df_step1.csv")

pnw_df_all<-read_csv("NotForGit/Step1/pnw_df_step1.csv") %>%
  mutate(Database="PNW",
         Region_DB="PNW",
         SiteCode=as.character(SiteCode))

  
pnw_df<-pnw_df_all %>%
  na.omit()

# main_df_nopnw %>%
#   bind_rows(pnw_df) %>%
#   left_join(gis_metrics_df) %>%
#   filter(is.na(tmin)) %>%
#   select(all_of(all_metrics_PNW)) %>%
#   skim_without_charts()
xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates_REGIONS.csv")  %>%
  filter(!Region %in% c("CB"))

main_df <- main_df_nopnw %>%
  bind_rows(pnw_df) %>%
  inner_join(gis_metrics_df) %>%
  inner_join(xwalk_df %>%
               select(SiteCode=sitecode, DRNAREA_mi2))


# main_df %>% skim_without_charts()
# main_df %>% group_by(Region_DB) %>%
#   select(Erosion_Deposition_Score, hydrophytes_present_any,RiparianCorridor_score, UplandRootedPlants_score2) %>%
#   skim()


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
  select(-ntot) %>%
  clipr::write_clip()

#Assesss completeness
main_df2<-main_df %>%
  select(SiteCode, Region_DB, Database, CollectionDate,
         all_of(all_metrics_PNW)) %>%
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
ggsave(visit_tally_plot, filename="Figures/visit_tally_plot.png", height=6, width=6)

visit_tally_plot_regions<-ggplot(data=visit_tally, aes(x=n, fill=Class))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_fill_brewer(palette="RdYlBu")+
  theme_bw()+xlab("# visits")+
  facet_wrap(~Region_DB, ncol=1)
ggsave(visit_tally_plot_regions, filename="Figures/visit_tally_plot_regions.png", height=6, width=6)

####Upsample to 6 per site
visit_tally$AtLeast6Samples<-visit_tally$n>=6

set.seed(1)
main_df2_GT6 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[visit_tally$AtLeast6Samples]) %>%
  group_by(SiteCode) %>%
  slice_sample(n=6, replace=F) %>%
  ungroup() %>%
  mutate(DataType="Original")


# main_df2_LT6 <- main_df2 %>%
#   filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast6Samples]) %>%
#   group_by(SiteCode) %>%
#   slice_sample(n=6, replace=T)

set.seed(1)
main_df2_LT6 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast6Samples]) %>%
  mutate(DataType="Original") %>%
  #If visited 5 times, add a random visit
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==5]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=1, replace=F) %>%
      mutate(DataType="Augmented") 
  ) %>%
  #If visited 4 times, add 2 random visits
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==4]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=2, replace=F) %>%
      mutate(DataType="Augmented") 
  )%>%
  #If visited 3 times, add all visits again
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==3]) %>%
      mutate(DataType="Augmented") 
  ) %>%
  #If visited 2 times, add all visits twice
  bind_rows(
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2]) %>%
      mutate(DataType="Augmented") , 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2]) %>%
      mutate(DataType="Augmented") 
  ) %>%
  # If visited once, add all visits 5 more times
  bind_rows(
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]) %>%
      mutate(DataType="Augmented") ,
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1])%>%
      mutate(DataType="Augmented") , 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1])%>%
      mutate(DataType="Augmented") , 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1])%>%
      mutate(DataType="Augmented") , 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1])%>%
      mutate(DataType="Augmented") 
  ) %>%
  ungroup()

main_df2_LT6 %>%
  group_by(SiteCode) %>% tally() 

main_df3<-main_df2_GT6 %>%
  bind_rows(main_df2_LT6) %>%
  mutate(Class = factor(Class, levels=c("E","I","P")))
main_df3 %>%
  select(SiteCode, Class, ohwm_region) %>%
  unique() %>%
  group_by(ohwm_region, Class) %>% tally()

#########MODELING
library(tidymodels)

mod_summary<- xwalk_df %>% 
  select(all_region,beta_region,ohwm_region,corps_region,nwca_region) %>%
  pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region,nwca_region), 
               names_to = "Stratification", values_to = "Strata") %>%
  crossing(IncludeGISPreds=c(T,F),
           IncludePNW=c(T,F)) %>%
  mutate(ModName = case_when(IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW_GIS", sep="_"),
                             IncludeGISPreds & !IncludePNW~paste(Stratification, Strata, "GIS", sep="_"),
                             !IncludeGISPreds & IncludePNW~paste(Stratification, Strata, "PNW", sep="_"),
                             T~paste(Stratification, Strata))) %>%
  mutate(FlagNoPNW = case_when(
    Stratification == "all_region"~"OK",
    
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
    Stratification == "ohwm_region" & !Strata %in% c("Northwest") & !IncludePNW~"OK",
    )) %>%
  filter(FlagNoPNW != "Delete")

pnw_sites <-xwalk_df$sitecode[which(xwalk_df$beta_region=="PNW")]

mod_dats<-lapply(1:nrow(mod_summary), function(i){
  stratf.i=mod_summary$Stratification[i]
  strat.i=mod_summary$Strata[i]
  pnw.i=mod_summary$IncludePNW[i]
  print(paste(stratf.i, strat.i, pnw.i))
  main_df3.i<-main_df3 %>%
    pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region,nwca_region), 
                 names_to = "Stratification", values_to = "Strata") %>%
    filter(Stratification==stratf.i & Strata==strat.i) 
  if(pnw.i)
    main_df3.i
  else
    main_df3.i %>% filter(!SiteCode %in% pnw_sites )
  })


set.seed(2)
mod_dats_split<-lapply(mod_dats, function(x){  
  x2 = x %>% select(SiteCode, Class) %>% unique()
  initial_split(x2, prop=4/5, strata=Class) })

mod_dats_training<-lapply(mod_dats_split, function(x){
  x2 = training(x)
  main_df3 %>%
    filter(SiteCode %in% x2$SiteCode)
})

mod_dats_testing<-lapply(mod_dats_split, function(x){
  x2 = testing(x)
  main_df3 %>%
    filter(SiteCode %in% x2$SiteCode)
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
my_rfs<-lapply(1:nrow(mod_summary), function(i){
  gis.i = mod_summary$IncludeGISPreds[i]
  print(paste(i, mod_summary$ModName[i]))
  
  if(gis.i)
    mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, GISPreds)))
  else
    mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW)))
  
  set.seed(200+i)
  rf.i=randomForest(Class~., 
               data=mydat, 
               importance=T,
               proximity=T)
  if(!gis.i)
    rf.i
  else
  {
    best2_gis_df = rf.i$importance %>%
      as_tibble() %>%
      mutate(myvar = row.names(rf.i$importance)) %>%
      filter(myvar %in% GISPreds) %>%
      slice_max(MeanDecreaseAccuracy, n=2)
    set.seed(300+i)
    randomForest(Class~., 
                      data=mydat %>% select(Class, all_of(c(BioPreds_PNW, GeomorphPreds_PNW, HydroPreds_Indirect_PNW, best2_gis_df$myvar))), 
                      importance=T,
                      proximity=T)
  }
  
})

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
  mutate(MetricType=case_when(Metric %in% GeomorphPreds_PNW~"Geomorphic",
                              Metric %in% GISPreds~"GIS",
                              Metric %in% HydroPreds_Indirect_PNW~"Hydro",
                              Metric %in% BioPreds_PNW~"Bio",
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
variable_importance_plot_pnw<-ggplot(imp_plot_dat %>% 
                                   group_by(Regionalization, Region_id, GIS) %>%
                                   slice_max(PNWtf, n=1), 
                                 aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
  geom_tile(color="white")+
  scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(GIS~Regionalization, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  ggtitle("Variable importance", subtitle =  "Includes PNW data")+
  xlab("")+ylab("")

ggsave(variable_importance_plot_pnw, filename="Figures/variable_importance_plot_pnw.png", height=15, width=9)
variable_importance_plot_xpnw<-ggplot(imp_plot_dat %>% 
                                       group_by(Regionalization, Region_id, GIS) %>%
                                       slice_min(PNWtf, n=1), 
                                     aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
  geom_tile(color="white")+
  scale_fill_viridis_c(trans="sqrt", na.value = "gray", name="MDA")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(GIS~Regionalization, scales="free", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1),
        panel.grid = element_blank())+
  ggtitle("Variable importance", subtitle =  "Excludes PNW data")+
  xlab("")+ylab("")

ggsave(variable_importance_plot_xpnw, filename="Figures/variable_importance_plot_xpnw.png", height=15, width=9)


####################


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
  xdf = my_predicted_classes_combined[[i]] 
  sum(xdf$Class_pred50_best =="NMI")/length(xdf$Class_pred50_best)
})

mod_summary$P_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] 
  sum(xdf$Class_pred50_best =="P")/length(xdf$Class_pred50_best)
})
mod_summary$I_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] 
  sum(xdf$Class_pred50_best =="I")/length(xdf$Class_pred50_best)
})
mod_summary$ALI_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] 
  sum(xdf$Class_pred50_best =="ALI")/length(xdf$Class_pred50_best)
})
mod_summary$E_freq<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] 
  sum(xdf$Class_pred50_best =="E")/length(xdf$Class_pred50_best)
})

outcomes_plot_dat<-mod_summary %>%
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName,
         P_freq, I_freq, ALI_freq, E_freq, NMI_freq) %>%
  pivot_longer(cols=(ends_with("freq"))) %>%
  mutate(name=factor(name, levels=c("P_freq","I_freq","ALI_freq","E_freq", "NMI_freq")))

outcomes_plot<-  ggplot(data=outcomes_plot_dat, aes(x=ModName, y=value))+
  geom_bar(aes(fill=name), stat="identity") +
  scale_fill_manual(values=c("#08519c","#6baed6","#8856a7","#f768a1","orange"))+
  coord_flip()+
  xlab("")+ylab("% of samples")
ggsave(outcomes_plot, filename="Figures/outcomes_plot.png", height=8, width=6)


mod_summary$Accuracy_PvIvE_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})

mod_summary$Accuracy_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    mutate(CORRECT = Class_pred50_best==Class)
  sum(xdf$CORRECT)/length(xdf$CORRECT)
})


mod_summary$Accuracy_EvALI_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
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
    filter(SiteSet=="Training")
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
    filter(SiteSet=="Testing")
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
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_PnotE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
    filter(Class=="P") 
  sum(xdf$Class_pred50_best!="E")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_training<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Training") %>%
    filter(Class=="E") 
  sum(xdf$Class_pred50_best!="P")/nrow(xdf)
})

mod_summary$Accuracy_EnotP_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing") %>%
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
  select(Stratification, Strata, IncludeGISPreds, IncludePNW, ModName,SiteSet,
         n_training, n_testing,
         MetricType2, Comparison,Metric, name, value
  ) 


all_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
         group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
         slice_max(IncludePNW, n=1), 
       aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Metric~Stratification, scales="free_x", space="free")+
  theme_bw()+
  geom_hline(yintercept = c(.8,.9,.5), linetype="dotted")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Performance")+xlab("")
ggsave(all_performance_metrics_pnw, filename="Figures/all_performance_metrics_pnw.png", height=7.5, width=13)
# 
# accuracy_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
#          filter(MetricType2=="Accuracy") %>%
#          group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#          slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#          mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#        aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")
# ggsave(accuracy_performance_metrics_pnw, filename="Figures/accuracy_performance_metrics_pnw.png", height=7.5, width=9)
# 
# accuracy_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
#                                            filter(MetricType2=="Accuracy") %>%
#                                            group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                            slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                            mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
#                                          aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Accuracy")+xlab("")
# ggsave(accuracy_performance_metrics_xpnw, filename="Figures/accuracy_performance_metrics_xpnw.png", height=7.5, width=9)
# 
# 
# precision_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
#          filter(MetricType2=="Precision") %>%
#          group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#          slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#          mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#        aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Precision")+xlab("")
# ggsave(precision_performance_metrics_pnw, filename="Figures/precision_performance_metrics_pnw.png", height=5, width=9)
# 
# precision_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
#                                             filter(MetricType2=="Precision") %>%
#                                             group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
#                                             slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
#                                             mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
#                                           aes(x=Strata, y=value))+
#   geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
#   scale_size_manual(values=c(1,2))+
#   facet_grid(Comparison~Stratification, scales="free_x", space="free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
#   scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
#   ylab("Precision")+xlab("")
# ggsave(precision_performance_metrics_xpnw, filename="Figures/precision_performance_metrics_xpnw.png", height=5, width=9)


mod_summary_long_across_strata<-mod_summary_long %>%
  group_by(Stratification, IncludeGISPreds, IncludePNW, SiteSet,
           MetricType2, Metric, Comparison, name) %>%
  summarise(value_unweighted = mean(value),
            lowest_value = min(value)) %>%
  ungroup()

mod_summary %>%
  filter(is.na(Accuracy_EnotP_testing)) %>% as.data.frame()

accuracy_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
         filter(MetricType2=="Accuracy") %>%
         group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
         slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
         mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
       aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
  geom_hline(data=mod_summary_long_across_strata %>%
               group_by(Stratification, IncludeGISPreds, SiteSet) %>%
               slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
               filter(MetricType2=="Accuracy") %>%
               mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
             aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")
ggsave(accuracy_performance_metrics_pnw, filename="Figures/accuracy_performance_metrics_pnw.png", height=7.5, width=9)

accuracy_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
                                           filter(MetricType2=="Accuracy") %>%
                                           group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
                                           slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
                                           mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
                                         aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
  geom_hline(data=mod_summary_long_across_strata %>%
               group_by(Stratification, IncludeGISPreds, SiteSet) %>%
               slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
               filter(MetricType2=="Accuracy") %>%
               mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
             aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")
ggsave(accuracy_performance_metrics_xpnw, filename="Figures/accuracy_performance_metrics_xpnw.png", height=7.5, width=9)

precision_performance_metrics_pnw<-ggplot(data=mod_summary_long %>%
         filter(MetricType2=="Precision") %>%
         group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
         slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
         mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
       aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
  geom_hline(data=mod_summary_long_across_strata %>%
               filter(MetricType2=="Precision") %>%
               group_by(Stratification, IncludeGISPreds, SiteSet) %>%
               slice_max(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
               mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
             aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Precision")+xlab("")
ggsave(precision_performance_metrics_pnw, filename="Figures/precision_performance_metrics_pnw.png", height=6, width=9)

precision_performance_metrics_xpnw<-ggplot(data=mod_summary_long %>%
                                            filter(MetricType2=="Precision") %>%
                                            group_by(Stratification, Strata, IncludeGISPreds, SiteSet) %>%
                                            slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
                                            mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
                                          aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds, shape=IncludePNW, group=IncludePNW), position=position_dodge(width=.5))+ 
  geom_hline(data=mod_summary_long_across_strata %>%
               filter(MetricType2=="Precision") %>%
               group_by(Stratification, IncludeGISPreds, SiteSet) %>%
               slice_min(IncludePNW, n=1) %>% #Selecting the max only shows models that included PNW. Use slice_min() for models that exclude PNW data
               mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
             aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Precision")+xlab("")
ggsave(precision_performance_metrics_xpnw, filename="Figures/precision_performance_metrics_xpnw.png", height=6, width=9)


mod_summary_long %>%
  filter(Comparison=="EnotP" & Stratification=="corps_region_short" & !IncludeGISPreds & SiteSet=="Testing")
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
usace_divisions_map_sites<-ggplot()+
  geom_sf(data=usace_sf, aes(fill=DIV_SYM))+
  geom_sf(data=xwalk_sf)+
  scale_fill_viridis_d(guide="none", option="cividis", begin=.1, end=.9)+
  coord_sf(xlim=c(-125, -66.5),
           ylim=c(25, 49.5))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
ggsave(usace_divisions_map_sites, filename="Figures/usace_divisions_map_sites.png", height=4, width=5)

xwalk_df2<-xwalk_sf %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(epa_reg = case_when(epa_reg %in% c("EPA_1","EPA_2")~"EPA_12",T~epa_reg))

xwalk_df2 %>% group_by(usace_reg) %>% tally()
xwalk_df2 %>% group_by(epa_reg) %>% tally()

mod_summary_assessment_strata <-  mod_summary %>%
  select(Stratification, Strata, ModName, IncludeGISPreds, IncludePNW) %>%
  unique() %>%
  crossing(AssessmentStratum=xwalk_sf$usace_reg %>% unique())

mod_summary_assessment_strata$n_sites <- sapply(1:nrow(mod_summary_assessment_strata), function(i){
  stratf.i=mod_summary_assessment_strata$Stratification[i]
  strat.i= mod_summary_assessment_strata$Strata[i]
  gis.i= mod_summary_assessment_strata$IncludeGISPreds[i]
  pnw.i= mod_summary_assessment_strata$IncludePNW[i]
  ass_subpop.i=mod_summary_assessment_strata$AssessmentStratum[i]
  j<-which(mod_summary$Stratification==stratf.i & mod_summary$Strata==strat.i & mod_summary$IncludeGISPreds==gis.i & mod_summary$IncludePNW==pnw.i)
  
  xdf = my_predicted_classes_combined[[j]] %>%
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i))
  nrow(xdf)
})

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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
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
    inner_join(xwalk_df2 %>% select(SiteCode=sitecode, usace_reg) %>%
                 filter(usace_reg==ass_subpop.i)) %>%
    filter(SiteSet=="Testing") %>%
    mutate(Class2 = case_when(Class %in% c("I","P")~"ALI", T~"E"),
           Class_pred50_best2 = case_when(Class_pred50_best %in% c("I","P","ALI")~"ALI",
                                          Class_pred50_best %in% c("E")~"E",
                                          Class_pred50_best %in% c("NMI")~"NMI",
                                          T~"Other"),
           CORRECT = Class_pred50_best2==Class2)
  sum(xdf$CORRECT)
})




mod_summary_assessment_strata_long<-mod_summary_assessment_strata %>%
  select(-n_sites) %>%
  pivot_longer(cols=c(starts_with("n"), starts_with("Correct"))) %>%
  group_by(Stratification, IncludeGISPreds, IncludePNW, AssessmentStratum, name) %>%
  summarise(value=sum(value)) %>%
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
         value=case_when(is.na(value)~0,T~value)  ) 

# mod_summary_assessment_strata_long_sum<-mod_summary_assessment_strata_long %>%
#   group_by(Stratification, IncludeGISPreds,  AssessmentStratum,
#            name, MetricType2, Metric, Comparison, SiteSet) %>%
#   summarise(value_unweighted = mean(value),
#             value_weighted = weighted.mean(value=n_sites),
#             value_lowest=min(value)) %>%
#   ungroup()

mod_summary_assessment_strata_long %>%
  filter(Stratification=="beta_region" & 
           AssessmentStratum=="SPD" & IncludeGISPreds & 
           SiteSet=="Training")

subpop_accuracy_plot_pnw<-ggplot(data=mod_summary_assessment_strata_long %>%
         filter(MetricType2=="Accuracy") %>%
         group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>% 
           slice_max(IncludePNW, n=1) %>% 
         mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
       aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  scale_y_continuous(limits=c(.5,1))
ggsave(subpop_accuracy_plot_pnw, filename="Figures/subpop_accuracy_plot_pnw.png", height=6, width=7.5)

subpop_accuracy_plot_xpnw<-ggplot(data=mod_summary_assessment_strata_long %>%
                                   filter(MetricType2=="Accuracy") %>%
                                   group_by(Stratification, AssessmentStratum, IncludeGISPreds, SiteSet) %>% 
                                   slice_min(IncludePNW, n=1) %>% 
                                   mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI"))),
                                 aes(x=AssessmentStratum , y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  # geom_hline(data=mod_summary_long_across_strata %>%
  #              filter(MetricType2=="Accuracy") %>%
  #              mutate(Comparison=factor(Comparison, levels=c("PvIvE","EvALI","PnotE","EnotP"))),
  #            aes(yintercept=value_unweighted, color=IncludeGISPreds, linetype=SiteSet))+
  scale_linetype_manual(values=c("dotted","dashed"))+
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Accuracy")+xlab("")+
  scale_y_continuous(limits=c(.5,1))
ggsave(subpop_accuracy_plot_xpnw, filename="Figures/subpop_accuracy_plot_xpnw.png", height=6, width=7.5)

subpop_accuracy_plot_diffpnw<-mod_summary_assessment_strata_long %>%
  mutate(ModelData = case_when(IncludePNW ~"Includes_PNW", T~"Excludes_PNW")) %>%
  select(-IncludePNW) %>%
  pivot_wider(names_from=ModelData, values_from = value) %>%
  mutate(DeltaInclude = Includes_PNW - Excludes_PNW) %>%
  ggplot(
    aes(x=AssessmentStratum, y=DeltaInclude)  )+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Comparison~Stratification, scales="free_x", space="free", drop=T)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Differences in Accuracy\n(With minus Without PNW)")+xlab("")+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks=(seq(from=-.8, to=.4, by=.1)))
ggsave(subpop_accuracy_plot_diffpnw, filename="Figures/subpop_accuracy_plot_diffpnw.png", height=6, width=7.5)
