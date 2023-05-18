library(tidyverse)

######Define metrics
BioPreds<-c(
  #Aquatic invertebrates
  ##ai_mets
  "TotalAbundance", "Richness", "mayfly_abundance", "perennial_PNW_abundance", 
  "perennial_PNW_taxa", "perennial_PNW_live_abundance", "perennial_NC_abundance", 
  "perennial_NC_taxa", "perennial_NC_live_abundance", "EPT_abundance", 
  "EPT_taxa", "EPT_relabd", "EPT_reltaxa", "GOLD_abundance", "GOLD_taxa", 
  "OCH_abundance", "OCH_taxa", "Noninsect_abundance", "Noninsect_taxa", 
  "Noninsect_relabund", "Noninsect_reltaxa", "GOLD_relabd", "GOLD_reltaxa", 
  "OCH_relabd", "OCH_reltaxa", "GOLDOCH_relabd", "GOLDOCH_reltaxa", 
  "Crayfish_abundance", "Crayfish_taxa", "Mollusk_abundance", "Mollusk_taxa", 
  "Bivalves_NonFG_Abundance", "Clam_Fingernail_Abundance", "TolRelAbund", 
  "TolRelAbundAlt", "NonTolTaxa", "NonTolTaxaAlt", "TolTaxa", "TolTaxaAlt",
  ##other invert metrics
  #BMI_score not calculated for NESE
  #Vegetation
  "hydrophytes_present","hydrophytes_present_noflag",
  "UplandRootedPlants_score",# "FibrousRootedPlants_score" Only in NESE
  "DifferencesInVegetation_score", "Moss_cover","Liverwort_cover",
  "PctShading",
  #algae
  "AlgalCover_Live","AlgalCover_LiveOrDead","AlgalCover_LiveOrDead_NoUpstream",#Algae_score not calculated for NESE
  #Fish
  # "Fish_score_NM",
  "Fish_PA","Fish_PA_nomosq",
  #Other bio
  "ironox_bfscore_NM"
)
#HYDROLOGIC INDICATORS



HydroPreds<-c(
  #NM varz
  "WaterInChannel_score","HydricSoils_score","springs_score_NM",
  #Others and novel
  "SurfaceFlow_pct","SurfaceSubsurfaceFlow_pct","IsolatedPools_number","WoodyJams_number",
  "SoilMoist_MeanScore",  "SoilMoist_MaxScore"
)

WaterPreds<-c("WaterInChannel_score", "springs_score_NM", 
              "SurfaceFlow_pct","SurfaceSubsurfaceFlow_pct",
              "IsolatedPools_number","SoilMoist_MeanScore", "SoilMoist_MaxScore")
HydroPreds_Indirect<-setdiff(HydroPreds, WaterPreds)

#GEOMORPH INDICATORS
main_df %>%
  # select(Region_DB, ironox_bfscore_NM) %>%
  select(Region_DB, contains("SedimentOnPlantsDebris_score")) %>%
  group_by(Region_DB) %>%
  skim_without_charts()

GeomorphPreds<-c(
  #NM varz
  "Sinuosity_score","ChannelDimensions_score","RifflePoolSeq_score",
  "SubstrateSorting_score","SedimentOnPlantsDebris_score",
  #Other varz
  "BankWidthMean","Slope"#"erosion_score","floodplain_score"
)

GISPreds<-c(#"Eco1","Eco2","Eco3",
  "tmean", "tmax", "tmin", 
  "ppt", 
  # "ppt.m01", "ppt.m02", "ppt.m03", "ppt.m04", "ppt.m05", "ppt.m06", "ppt.m07", "ppt.m08", "ppt.m09", "ppt.m10", "ppt.m11", "ppt.m12", 
  # "temp.m01", "temp.m02", "temp.m03", "temp.m04", "temp.m05", "temp.m06", "temp.m07", "temp.m08", "temp.m09", "temp.m10", "temp.m11", "temp.m12", 
  "Elev_m", 
  "MeanSnowPersistence_10", "MeanSnowPersistence_05", "MeanSnowPersistence_01", 
  "SnowDom_SP10", "SnowDom_SP05", "SnowDom_SP01",
  "ppt.234", "ppt.567", "ppt.8910", "ppt.11121", 
  "temp.234", "temp.567", "temp.8910", "temp.11121"
)




#############Import data
main_df<-read_csv("NotForGit/Step1/main_df_step1.csv") %>%
  filter(Region_DB!="CB")

xwalk_df<-read_csv("NotForGit/Step1/xwalk_df.csv") %>%
  mutate(corps_region_short = case_when(corps_region=="USACE Eastern Mountains and Piedmont Region"~"EMP",
                                        corps_region=="USACE Atlantic and Gulf Coastal Plain Region"~"AGCP",
                                        corps_region=="USACE Arid West Region"~"AW",
                                        corps_region=="USACE Western Mountains, Valleys, and Coast Region"~"WMVC",
                                        corps_region=="USACE Great Plains Region"~"GP",
                                        corps_region=="USACE Northcentral and Northeast Region"~"NCNE",
                                        corps_region=="USACE Midwest Region"~"MW",
  )) %>%
  filter(!Region %in% c("CB","PNW"))

xwalk_df %>% group_by(ohwm_region) %>% tally()

xwalk_df %>%
  filter(Class!="U") %>%
  filter(Region_detail2!="CB") %>%
  filter(Region_detail2!="PNW") %>%
  select(Class, beta_region, ohwm_region, corps_region_short, nwca_region) %>%
  pivot_longer(cols=c(beta_region, ohwm_region, corps_region_short, nwca_region),
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
         all_of(c(BioPreds, GeomorphPreds, GISPreds, HydroPreds_Indirect))) %>%
  na.omit() %>%
  inner_join(xwalk_df %>% rename(SiteCode=sitecode)) %>%
  filter(Class!="U")

library(skimr)
main_df2 %>% 
  filter(ohwm_region %in% c("Northeast","Southeast")) %>%
  skim_without_charts()


visit_tally<-main_df2 %>%
  group_by(SiteCode, Class) %>%
  tally() %>%
  ungroup()
ggplot(data=visit_tally, aes(x=n, fill=Class))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=c(0:12))+
  scale_fill_brewer(palette="RdYlBu")+
  theme_bw()+xlab("# visits")

####Upsample to 6 per site
visit_tally$AtLeast6Samples<-visit_tally$n>=6

set.seed(1)
main_df2_GT6 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[visit_tally$AtLeast6Samples]) %>%
  group_by(SiteCode) %>%
  slice_sample(n=6, replace=F) %>%
  ungroup()


# main_df2_LT6 <- main_df2 %>%
#   filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast6Samples]) %>%
#   group_by(SiteCode) %>%
#   slice_sample(n=6, replace=T)

set.seed(1)
main_df2_LT6 <- main_df2 %>%
  filter(SiteCode %in% visit_tally$SiteCode[!visit_tally$AtLeast6Samples]) %>%
  #If visited 5 times, add a random visit
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==5]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=1, replace=F)
  ) %>%
  #If visited 4 times, add 2 random visits
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==4]) %>%
      group_by(SiteCode) %>%
      slice_sample(n=2, replace=F) 
  )%>%
  #If visited 3 times, add all visits again
  bind_rows(
    main_df2 %>%
      filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==3]) 
  ) %>%
  #If visited 2 times, add all visits twice
  bind_rows(
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2]), 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==2])
  ) %>%
  # If visited once, add all visits 5 more times
  bind_rows(
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]),
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]), 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]), 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1]), 
    main_df2 %>% filter(SiteCode %in% visit_tally$SiteCode[visit_tally$n==1])
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
  select(all_region,beta_region,ohwm_region,corps_region_short,nwca_region) %>%
  pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region_short,nwca_region), 
               names_to = "Stratification", values_to = "Strata") %>%
  crossing(IncludeGISPreds=c(T,F)) %>%
  mutate(ModName = case_when(IncludeGISPreds~paste(Stratification, Strata, "GIS", sep="_"),
                             T~paste(Stratification, Strata)))

mod_dats<-lapply(1:nrow(mod_summary), function(i){
  stratf.i=mod_summary$Stratification[i]
  strat.i=mod_summary$Strata[i]
  print(paste(stratf.i, strat.i))
  main_df3.i<-main_df3 %>%
    pivot_longer(cols=c(all_region,beta_region,ohwm_region,corps_region_short,nwca_region), 
                 names_to = "Stratification", values_to = "Strata") %>%
    filter(Stratification==stratf.i & Strata==strat.i) 
  main_df3.i
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
library(caret)
my_ctrl <- rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "all")

#RFE is prohibitively slow
# maxsize_x<-21
# my_rfes_total<-lapply(1:nrow(mod_summary), function(i){
#   gis.i = mod_summary$IncludeGISPreds[i]
#   print(paste(i, mod_summary$ModName[i]))
#   
#   if(gis.i)
#     mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds, GeomorphPreds, HydroPreds_Indirect, GISPreds))) 
#   else
#     mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds, GeomorphPreds, HydroPreds_Indirect)))
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
    mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds, GeomorphPreds, HydroPreds_Indirect, GISPreds)))
  else
    mydat= mod_dats_training[[i]] %>%  select(Class, all_of(c(BioPreds, GeomorphPreds, HydroPreds_Indirect)))
  
  set.seed(200+i)
  randomForest(Class~., 
               data=mydat, 
               importance=T,
               proximity=T)
  
})

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
    filter(SiteSet=="Training")
  xdf2<-xdf %>% 
    group_by(SiteCode, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/6) %>%
    ungroup() %>%
    summarise(Precision=mean(Consistency))
  xdf2$Precision
})

mod_summary$Precision_PvIvE_testing<-sapply(1:nrow(mod_summary), function(i){
  xdf = my_predicted_classes_combined[[i]] %>%
    filter(SiteSet=="Testing")
  xdf2<-xdf %>% 
    group_by(SiteCode, Class_pred50_best) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/6) %>%
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
    group_by(SiteCode, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/6) %>%
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
    group_by(SiteCode, Class_pred50_best2) %>%
    tally() %>%
    slice_max(order_by = n) %>%
    mutate(Consistency = n/6) %>%
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
  select(Stratification, Strata, IncludeGISPreds, ModName,SiteSet,
         n_training, n_testing,
         MetricType2, Comparison,Metric, name, value
         ) 


ggplot(data=mod_summary_long, aes(x=Strata, y=value))+
  geom_point(aes(size=SiteSet, color=IncludeGISPreds), position=position_dodge(width=0))+ 
  scale_size_manual(values=c(1,2))+
  facet_grid(Metric~Stratification, scales="free_x", space="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  scale_color_brewer(palette = "Set1", name="GIS", labels=c("No","Yes"))+
  ylab("Performance")+xlab("")


which(mod_summary$Strata=="NCNE" & mod_summary$IncludeGISPreds)
mod_summary[26,]
my_predicted_classes_testing[[26]] %>%
  group_by(SiteCode, Class, Class_pred50_best) %>%
  filter(Class=="E") %>%
  tally()

####
#Importance


rf_sum_importance<-lapply(1:nrow(mod_summary), function(i){
  gis.i=mod_summary$IncludeGISPreds[i]
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
           Metric=row.names(xmat)) %>%
    rename(P_imp=P, I_imp=I, E_imp=E)
  
}) %>% bind_rows() %>%
  mutate(MetricType=case_when(Metric %in% GeomorphPreds~"Geomorphic",
                              Metric %in% GISPreds~"GIS",
                              Metric %in% HydroPreds_Indirect~"Hydro",
                              Metric %in% BioPreds~"Bio",
                              T~"Other" ),
         RegLabel = paste(Regionalization, Region_id, sep="-"))

rf_sum_importance %>% group_by(MetricType) %>% tally()
imp_plot_dat<-rf_sum_importance %>%
  mutate(MeanDecreaseAccuracy=case_when(MeanDecreaseAccuracy<0~0,T~MeanDecreaseAccuracy),
         Region_id = gsub("USACE ","", Region_id)) %>%
  arrange(MeanDecreaseAccuracy)
imp_plot_dat$Metric<-factor(imp_plot_dat$Metric, levels=unique(imp_plot_dat$Metric))

met_colors_df<-imp_plot_dat %>%
  select(MetricType, Metric) %>%
  mutate(MetricColor = case_when(MetricType =="Bio"~"#4daf4a",
                                 MetricType =="Hydro"~"#377eb8",
                                 MetricType =="Geomorphic"~"#d95f02",
                                 MetricType =="GIS"~"#7570b3",
                                 
                                 ))
variable_importance_plot<-ggplot(imp_plot_dat, 
       aes(x=Region_id, y=Metric, fill=MeanDecreaseAccuracy))+
  geom_tile()+
  scale_fill_viridis_c(trans="sqrt")+
  # facet_wrap(~Regionalization, scales="free_x", nrow=1)+
  facet_grid(GIS~Regionalization, scales="free", space="free")+
  theme(axis.text.x = element_text(angle=90,  vjust = 0.5, hjust=1))+
  xlab("")+ylab("")

ggsave(variable_importance_plot, filename="NotForGit/variable_importance_plot.png", height=15, width=9)
