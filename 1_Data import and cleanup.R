library(tidyverse)

#Import xwalk<-
xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates.csv") %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U")))

EastDBs<-c("NESE Baseline Revisits v2", "NESE Baseline v1", "NESE Validation v1")
WestDBs<-c("WMBR_1_1","WEx_SDAM_0","WMBR_2","WMV_1","FD003","FD004")
GPDBs<-c("Great Plains Baseline Revisits v3",  "Great Plains SDAM v3",  "Great Plains Validation_v2", "Great Plains Baseline Revisits v2_1_October", "GreatPlains_SDAM_V3_edits")
# xwalk_df$Region_detail2<-factor(xwalk_df$Region_detail2, levels=c("AW","WM","GP","NE","SE", "CB"))

xwalk_df %>%
  group_by(Region_detail2, Class) %>% tally() %>%
  pivot_wider(names_from=Class, values_from = n, values_fill = 0) %>%
  mutate(Total = E+I+P+U,
         TotalKnown=E+I+P,
         Pct_P=P/TotalKnown,
         Pct_I=I/TotalKnown,
         Pct_E=E/TotalKnown) #%>%
# write.table(file="clipboard", sep="\t", row.names=F)

#Import main_df
#### MAIN - Read in main site data table and filter for GP sites ####
library(skimr)


main_raw<-read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all")
# main_raw %>% filter(origin_database %in% EastDBs) %>% select(abundancealgae) %>% View()
# main_raw %>% filter(origin_database %in% WestDBs) %>% View()


# main_df %>%
#   select(Region_DB,Database, SiteCode, CollectionDate, starts_with("Alg")) %>%
#   mutate(AlgScore2 = case_when(Algae_score=="Not recorded"~NA_character_,
#                                T~Algae_score) %>%
#            as.numeric()) %>%
#   filter(is.na(AlgalCover_Live) & !is.na(AlgScore2)) %>% as.data.frame()
# group_by(Region_DB) %>% skim_without_charts()

# main_df %>%
# select(Region_DB,Database, SiteCode, CollectionDate, starts_with("Hydric")) %>%
# group_by(Region_DB) %>% skim_without_charts()

main_df<- main_raw %>% #read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all") %>%
  # filter(origin_database %in% myDBs) %>%
  transmute( 
    #SITE AND SAMPLE METADATA
    Download_date = Sys.time(),
    Database= origin_database,
    Region_DB = case_when(Database %in% EastDBs ~"East",
                          Database %in% WestDBs~"West",
                          Database %in% GPDBs~"GP",
                          T~"Other"),
    ParentGlobalID = globalid,
    SiteCode = sitecode,
    SiteName = sitename,
    Assessors = assessor,
    Recorder= recorder, QA=qa,
    CollectionDate= collectiondate,
    CreationDate= creationdate,
    Creator=creator,
    EditDate=editdate,
    Lat_field= lat, 
    Long_field= long,
    Weather=weathernow,
    PctCloudCover = case_when(is.na(cloudynow) == T ~ 0,T~cloudynow), # Replace missing cloud cover with zero
    rain_yesno, #NESE only
    Disturbances=disturbed,
    Disturbances_details=disturbed_note,
    # Landuse=p_landuse,#NESE
    Landuse = case_when(is.na(p_landuse)~s_landuse, T~p_landuse), 
    # Landuse_s=s_landuse, #Now secondary landuse for NESE. Was primary for other datasets
    Landuse_s =case_when(is.na(p_landuse)~NA_character_, T~s_landuse),
    Lanuse_Notes=landuse_note,
    ReachLength_targ= reachlengthcalc,
    ReachLength_actual= reachlength,
    Riparian_yn= riparian, 
    ErosionDeposition_yn=erosion,
    FloodplainConnectivity_yn=floodplain,
    #GEOMORPHIC INDICATORS
    Bankwidth_0 = bankfullwidth0, #drop later
    Bankwidth_15 = bankfullwidth15, #drop later
    Bankwidth_30 = bankfullwidth30, #,#drop later
    # BankWidthMean = (Bankwidth_0+Bankwidth_15+Bankwidth_30)/(!is.na(Bankwidth_0)+!is.na(Bankwidth_15)+!is.na(Bankwidth_30)),
    #main_raw %>% select(Bankwidth_0, Bankwidth_15, Bankwidth_30) %>% rowMeans(na.rm=T), #nearly complete
    Slope=valleyslope,  #nearly complete
    
    SedimentOnPlantsDebris_score= hi_debrisscore, 
    SedimentOnPlantsDebris_notes= debrisnotes,
    Continuity_score = gi_contbbscore, # NESE - Continuity  of channel and bank score
    Continuity_notes = contbbnotes,# NESE
    Depositional_score = gi_depbbscore,# NESE - Depositional bars and benches
    Depositional_notes = depbbnotes,# NESE
    AlluvialDep_score = gi_radscore,# NESE
    AlluvialDep_notes = radnotes,# NESE
    Headcut_score = gi_headcutscore,# NESE
    Headcut_notes = headcutnotes,# NESE
    GradeControl_score = gi_gcscore,# NESE
    GradeControl_notes = gcnotes,# NESE
    NaturalValley_score = gi_nvscore,# NESE
    NaturalValley_notes = nvnotes, # NESE
    Sinuosity_score= si_sinuosityscore,
    Sinuousity_method= sinuositymethod,
    Sinuosiy_notes=sinuositycondition,
    ActiveFloodplain_score = gi_afpscore,# NESE ONLY
    ActiveFloodplain_notes = afpnotes,# NESE
    fp_number,# NESE - number of locations assessed for bankfull width
    fp_bankful,# NESE - Bankfull width, location 1
    fp_floodprone,# NESE - Floodprone width, location 1
    fp_greaterthan,# NESE - 2x bankfull width, location 1
    fp_bankful2,# NESE
    fp_floodprone2,#NESE
    fp_greaterthan2,# NESE
    fp_bankful3,# NESE
    fp_floodprone3,# NESE
    fp_greaterthan3,# NESE
    fp_entrenchmentratio1= case_when(fp_greaterthan=="yes"~2.5,T~fp_floodprone/fp_bankful),
    fp_entrenchmentratio2= case_when(fp_greaterthan2=="yes"~2.5,T~fp_floodprone2/fp_bankful2),
    fp_entrenchmentratio3= case_when(fp_greaterthan3=="yes"~2.5,T~fp_floodprone/fp_bankful3),
    ChannelDimensions_method= "direct measurement",# Not recorded for NESE
    RifflePoolSeq_score= gi_sequencescore, 
    RifflePoolSeq_notes= seqnotes,
    SubstrateSorting_score=gi_substratesorting, 
    SubstrateSorting_notes=ginotes, 
    Substrate_Bedrock = scsc_bedrock, #NESE
    Substrate_Boulder = scsc_boulder,#NESE
    Substrate_BoulderSlab = scsc_boulderslab,#NESE
    Substrate_Cobble = scsc_cobble,#NESE
    Substrate_Gravel = scsc_gravel,#NESE
    Substrate_Sand = scsc_sand,#NESE
    Substrate_Silt = scsc_silt,#NESE
    Substrate_Clay = scsc_clay,#NESE
    Substrate_Muck = scsc_muck,#NESE
    Substrate_Leaf = scsc_leaf,#NESE
    Substrate_Fine = scsc_fine,#NESE
    Substrate_Artificial = scsc_artifical,#NESE
    #HYDROLOGIC INDICATORS
    SurfaceFlow_pct=hi_reachlengthsurface,
    SurfaceSubsurfaceFlow_pct= case_when(
      is.na(hi_reachlengthsub) ==T ~ SurfaceFlow_pct, # Replace missing subsurface flow with surface flow
      T~hi_reachlengthsub),
    IsolatedPools_number= case_when(
      is.na(pools_observed) == T ~ 0, # Replace missing pool number with zero
      T~pools_observed),
    MaxPoolDepth_calc = max_pool_depth_calc, # NESE only
    MaxPoolDepth = max_pool_depth,# NESE only
    SeepsSprings_yn=hi_seepsspring,# All regions
    SeepsSprings_inchannel = inchannel,# NESE only
    springs_score_NM = case_when(SeepsSprings_yn=="present"~3,T~0),
    baseflowscore,# NESE only
    baseflow_notes,#NESE
    WaterInChannel_score = case_when(is.na(hi_channelscore) & !is.na(baseflowscore)~2*baseflowscore,
                                     T~hi_channelscore), # CALCULATE DUE TO MISSING FOR NESE
    WaterInChannel_notes=  case_when(is.na(hi_channelscore)~paste("Calculated as twice the value of NC indicator.",baseflow_notes),
                                     T~channelscorenote),
    Wet=WaterInChannel_score>3,
    LeafLitter_score = hi_leaflitter,# NESE
    LeafLitter_notes = leafnotes,# NESE
    ODL_score = hi_odl,# NESE
    ODL_notes = odlnotes, # NESE
    
    HydricSoils_score=hydricsoils, #nearly complete
    HydricSoils_locations=locations, 
    HydricSoils_notes= hydricnotes,
    
    WoodyJams_number= case_when( # This will consolidate the two woody jams columns for NESE - needs to be fixed in consolidation script still
      is.na(woody_jams) ==T ~ woodyjams1,
      is.na(woodyjams1) ==T ~ woody_jams,
      T~0 # Replace missing wood jams count with zero
    ), 
    WoodyJams_source=case_when(is.na(woody_material)~trees_shrubs, 
                               T~woody_material),  # Same as trees_shrubs in other databases
    # WoodyJams_notes=woodyjamsnotes,# Not recorded for NESE
    
    SoilMoisture1=locationonemoisture, 
    SoilMoisture2=locationtwomoisture,
    SoilMoisture3=locationthreemoisture,
    SoilTexture1=locationonetexture,
    SoilTexture2=locationtwotexture,
    SoilTexture3=locationthreetexture,
    
    #BIOLOGICAL INDICATORS
    
    
    UplandRootedPlants_score= uplandrootedplants_score, # Both uplandrootedplants score and fiberousroots score ARE recorded for the NESE region (KSM)
    UplandRootedPlants_notes= uplandrootedplants_notes, 
    FibrousRootedPlants_score= fibrous_rootscore, # Actually, fibrous roots for NESE in ununified database
    FibrousRootedPlants_notes= fibrous_rootnotes, # Actually, fibrous roots for NESE in ununified database
    
    number_of_fish,#NESE
    Mosquitofish = case_when(is.na(mosquitofish) & is.na(all_mosqfish)~"no",
                             mosquitofish=="yes" | all_mosqfish=="yes"~"yes",
                             T~"no"),
    # Mosquitofish = mosquitofish, #Not NESE
    # all_mosqfish,#NESE
    fishabund_note,#NESE
    Fish_score_NM = abundancescorefish , # Not recorded for NESE
    Fish_score_NC = case_when(number_of_fish =="0"~0,
                           number_of_fish =="1"~0.5,
                           number_of_fish =="2"~1,
                           number_of_fish =="3"~1,
                           number_of_fish ==">3"~1.5,
                           T~NA_real_),
    Fish_PA = case_when(number_of_fish>0~1,
                        number_of_fish==">3"~1,
                        Fish_score_NM>0~1,
                        is.na(Fish_score_NM) & is.na(number_of_fish)~0,
                        T~0), #Both East and Other regions
    Fish_PA_nomosq = (Fish_PA*(Mosquitofish=="no")), #Both East and Other regions
    #BMI_score= abundancescorebenthic,# Not recorded for NESE
    Algae_score= abundancealgae, 
    
    
    
    #Mosquitofish = mosquitofish, # Not recorded for NESE
    #Vertebrate_notes= observedabundancenote, # Not recorded for NESE
    # IOFB_yn= observedfungi, 
    ironox_bfscore_NM = case_when(observedfungi=="present"~3,
                               observedfungi %in% c("notdetected","notdectected")~0,
                               is.na(observedfungi)~0,
                               is.na(ironox_bfscore)~0,
                               T~ironox_bfscore), 
    ironox_bfnotes,#NESE
    Snakes_yn= observedsnakes, # Not recorded for NESE
    # Snakes_abundance= obsnakesabundance, # Not recorded for NESE
    
    Turtles_yn= observedturtles, # Not recorded for NESE
    # Turtles_abundance=obturtlesabundance, # Not recorded for NESE
    
    Amphibians_yn=observedamphibians, # Not recorded for NESE # Needs transformation from amphibian data
    # Amphibians_abundance= obampiabundance, # Not recorded for NESE #Not meaningful!
    #FrogVocalizations_yn= observedvocalfrogs, # Not recorded for NESE
    BiologicalIndicators_notes= observedabundancenote, # Not recorded for NESE
    
    AlgalCover_Live=case_when(streambedlive %in% c("2 to 10%", "2-10%")~"2 to 10%",
                              streambedlive %in% c("10 to 40%", "10-40%")~"10 to 40%",
                              streambedlive %in% c("notdetected")~"Not detected",
                              T~streambedlive) ,
    AlgalCover_Dead=case_when(streambeddead %in% c("2 to 10%", "2-10%")~"2 to 10%",
                              streambeddead %in% c("10 to 40%", "10-40%")~"10 to 40%",
                              streambeddead %in% c("notdetected")~"Not detected",
                              T~streambeddead) ,
    AlgalCover_LiveOrDead = case_when(AlgalCover_Live==">40%" | AlgalCover_Dead==">40%"~">40%",
                                      AlgalCover_Live=="10 to 40%" | AlgalCover_Dead=="10 to 40%"~"10 to 40%",
                                      AlgalCover_Live=="2 to 10%" | AlgalCover_Dead=="2 to 10%"~"2 to 10%",
                                      AlgalCover_Live=="<2%" | AlgalCover_Dead=="<2%"~"<2%",
                                      AlgalCover_Live=="Not detected" & AlgalCover_Dead=="Not detected"~"Not detected",
                                      T~"Other"
    ),
    AlgalCover_Upstream=streambeddeadmats,
    AlgalCover_Live_NoUpstream = case_when(AlgalCover_Upstream=="yes"~"Not detected",T~AlgalCover_Live),
    AlgalCover_Dead_NoUpstream = case_when(AlgalCover_Upstream=="yes"~"Not detected",T~AlgalCover_Dead),
    AlgalCover_LiveOrDead_NoUpstream = case_when(AlgalCover_Upstream=="yes"~"Not detected",T~AlgalCover_LiveOrDead),
    AlgalCover_notes= streambedalgaenotes, 
    
    dens_UU=u_upstream,
    dens_UL=u_left,
    dens_UR=u_right,
    dens_UD=u_downstream,
    dens_MU=m_upstream,
    dens_ML=m_left,
    dens_MR=m_right,
    dens_MD=m_downstream,
    dens_DU=l_upstream,
    dens_DL=l_left,
    dens_DR=l_right,
    dens_DD=l_downstream, 
    
    Moss_cover=bryophytemosses, 
    Liverwort_cover=bryophyteliverworts,
    Bryophyte_notes=bryophtyenotes,
    
    DifferencesInVegetation_score=vegetationdifferencescore, 
    DifferencesInVegetation_notes=vegenotes, 
    NWPL_checklist= regionalindicators, 
    ai_fieldid, #NESE
    Additional_notes= additionalnotes,
    hydrovegenote #NESE
  ) %>%
  rowwise() %>%
  mutate(fp_entrenchmentratio_mean=mean(c(fp_entrenchmentratio1,fp_entrenchmentratio2,fp_entrenchmentratio3), na.rm=T),
         fp_entrenchmentratio_mean=case_when(fp_entrenchmentratio_mean>2.5~2.5, T~fp_entrenchmentratio_mean),
         ChannelDimensions_score=case_when(fp_entrenchmentratio_mean<1.2~0,
                                           fp_entrenchmentratio_mean<2.5~1.5,
                                           fp_entrenchmentratio_mean>=2.5~3,
                                           T~NA_real_),
         BankwidthMean = mean(c(Bankwidth_0, Bankwidth_15, Bankwidth_30), na.rm = T),
         PctShading=mean(c(dens_UU/.17, dens_UL/.17, dens_UR/.17, dens_UD/.17,
                           dens_MU/.17, dens_ML/.17, dens_MR/.17, dens_MD/.17,
                           dens_DU/.17, dens_DL/.17, dens_DR/.17, dens_DD/.17), na.rm = T)) %>%
  ungroup() %>%
  #Eliminate interim metrics used for calculating other metrics
  select(-Bankwidth_0, -Bankwidth_15, -Bankwidth_30,
         -fp_entrenchmentratio1, -fp_entrenchmentratio2, -fp_entrenchmentratio3, -fp_entrenchmentratio_mean,
         -starts_with("dens_"))


####Soil moisture
main_df$SoilMoisture1 %>% unique()

soil_moisture_df<-main_df %>%
  transmute(ParentGlobalID=ParentGlobalID,
            WaterInChannel_score = 2*baseflowscore,
            SoilMoisture1=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture1=="dry"~0,
                                    SoilMoisture1=="partiallydry"~1,
                                    SoilMoisture1=="saturated"~2,
                                    T~NA_real_),
            SoilMoisture2=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture2=="dry"~0,
                                    SoilMoisture2=="partiallydry"~1,
                                    SoilMoisture2=="saturated"~2,
                                    T~NA_real_),
            SoilMoisture3=case_when(WaterInChannel_score>0~2, MaxPoolDepth>0~2,
                                    SoilMoisture3=="dry"~0,
                                    SoilMoisture3=="partiallydry"~1,
                                    SoilMoisture3=="saturated"~2,
                                    T~NA_real_)) %>%
  pivot_longer(cols=c(SoilMoisture1, SoilMoisture2, SoilMoisture3)) %>%
  group_by(ParentGlobalID) %>%
  summarise(SoilMoist_MeanScore = mean(value, na.rm=T),
            SoilMoist_MaxScore = max(value, na.rm=T),
            SoilMeasures=sum(!is.na(value))
  ) %>% 
  ungroup() %>%
  select(-SoilMeasures)

has_moisture<-main_df %>% 
  filter(ParentGlobalID %in% soil_moisture_df$ParentGlobalID[is.na(soil_moisture_df$SoilMoist_MeanScore)]) %>%
  select(Database, ParentGlobalID, SiteCode, CollectionDate, SurfaceFlow_pct,SurfaceSubsurfaceFlow_pct, IsolatedPools_number) %>%
  filter(SurfaceFlow_pct>0 | SurfaceSubsurfaceFlow_pct>0 | IsolatedPools_number>0)

soil_moisture_df$SoilMoist_MeanScore[soil_moisture_df$ParentGlobalID %in% has_moisture$ParentGlobalID]<-2
soil_moisture_df$SoilMoist_MaxScore[soil_moisture_df$ParentGlobalID %in% has_moisture$ParentGlobalID]<-2

main_df<-main_df %>%
  left_join(soil_moisture_df %>% select(ParentGlobalID, SoilMoist_MeanScore,SoilMoist_MaxScore))

soil_moisture_df %>% filter(is.na(SoilMoist_MeanScore)) %>%
  left_join(main_df %>% select(Database, ParentGlobalID, SiteCode, CollectionDate) ) %>%
  select(-ParentGlobalID, -starts_with("Soil")) %>% 
  arrange(Database, SiteCode, CollectionDate) %>% 
  clipr::write_clip()

#AMPHIBIANS
#Calculate amphibian p/a from NESE data
salamander_species<-c("Desmognathus","Desmognathus auriculatus","Desmognathus fuscus","Desmognathus monticola","Desmognathus ochrophaeus",
                      "Eurycea","Eurycea bislineata","Eurycea cirrigera","Eurycea longicauda","Eurycea wilderae",
                      "Gyrinophius", "Gyrinophius porphyriticus" ,"Plethodontidae","Plethodon", "Plethodon cinereus", "Urodela")
frog_species<-c("Acris","Acris crepitans","Acris crepitans blanchardi","Acris gryllus",
                "Anaxyurus", "Anaxyurus americanus","Anura","Eleutherodactylus",
                "Hyla","Hyla chrysoscelis",
                "Eleutherodactylidae",
                "Lithobates","Lithobates catesbeianus","Lithobates clamitans","Lithobates palustris","Lithobates pipiens","Lithobates sphenocephalus","Lithobates sphenocephalus utricularius","Lithobates sylvaticus",
                "Incilius","Incilius nebulifer",
                "Pseudacris","Pseudacris brachyphona","Pseudacris fouquettei","Pseudacris nigrita","Ranidae" )
multiyear_tadpoles<-c("Lithobates catesbeianus", "Lithobates hecksheri","Lithobates virgatipes")


amphib_df<-read_csv("https://sdamchecker.sccwrp.org/checker/download/amphibians-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    AmphibGlobalID=globalid,
    SiteCode = am_sitecode, 
    Amphib_Species=amphibians_taxon, 
    Amphib_LifeStage=amphibians_lifestage, 
    Amphib_Abundance=amphibians_abundance, 
    Amphib_Notes=amphibians_notes
  )%>%
  mutate(Amphib_Species = case_when(is.na(Amphib_Species)~"None",
                                    Amphib_Species=="American toad"~"Anaxyurus americanus",
                                    Amphib_Species=="Plethodonitdae"~"Plethodontidae",
                                    T~str_remove(Amphib_Species,"\r\n")),
         Frog = Amphib_Species %in% frog_species,
         Salamander = Amphib_Species %in% salamander_species,
         Multiyear_Tadpole = Amphib_Species %in% multiyear_tadpoles 
  ) %>%
  filter(! Amphib_Species %in% c("None","None observed","Not recorded"))  
setdiff(amphib_df$Amphib_Species , c(frog_species, salamander_species, multiyear_tadpoles)) 



amphib_mets<-amphib_df %>%
  group_by(ParentGlobalID) %>%
  summarise(Amphib_richness = Amphib_Species %>% unique() %>% length(),
            Amphib_abundance = sum(Amphib_Abundance),
            Frog_richness = Amphib_Species[Frog] %>% unique() %>% length(),
            Frog_abundance = sum(Amphib_Abundance[Frog]),
            Salamander_richness = Amphib_Species[Salamander] %>% unique() %>% length(),
            Salamander_abundance = sum(Amphib_Abundance[Salamander]),
            Amphib_Juvenile_richness = Amphib_Species[Amphib_LifeStage!="A"] %>% unique() %>% length(),
            Amphib_Juvenile_abundance = sum(Amphib_Abundance[Amphib_LifeStage!="A"]),
            Salamander_Juvenile_abundance = sum(Amphib_Abundance[Salamander & Amphib_LifeStage %in% c("L","J")]),
            Multiyear_tadpole_abundance = sum(Amphib_Abundance[Multiyear_Tadpole & Amphib_LifeStage=="L"])
  )

main_df<-main_df %>%
  left_join(amphib_mets %>% transmute(ParentGlobalID, 
                                      Amphib_count=Amphib_abundance)) %>%
  mutate(Amphibians_yn = case_when(Amphibians_yn=="present"~"present",
                                   Amphib_count>0~"present",
                                   Amphibians_yn %in% c("notdetected","notdectected")~"notdetected",
                                   Amphib_count==0~"notdetected",
                                   is.na(Amphib_count) & is.na(Amphibians_yn)~"notdetected",
                                   T~"OTHER")) %>%
  #Get rid of interim metrics
  select(-Amphib_count)


#### HYDROVEG ####
hydroveg_df<- 
  read_csv("https://sdamchecker.sccwrp.org/checker/download/hydroveg-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    VegGlobalID=globalid,
    SiteCode = hv_sitecode, 
    Plant_Species=hv_species, 
    Plant_Status=indicatorstatus, 
    Plant_flag=unusualdistro,
    InChannel = hv_inchannel, #NESE
    Plant_notes=hv_notes
  ) 
# hydroveg_df %>% group_by(Plant_Species) %>% tally()  %>%
#   clipr::write_clip()
# 
# hydroveg_df %>%
#   select(ParentGlobalID, Plant_Species, Plant_Status, Plant_notes) %>%
#   inner_join(main_df %>% 
#                select(ParentGlobalID, Region_DB, Database, SiteCode, CollectionDate)) %>%
#   filter(is.na(Plant_notes)) %>%
#   # filter(Plant_Species=="Not recorded")
#   # filter(Plant_Species=="Tri-finger leaf tree") %>%
#   filter(str_detect(Plant_Species,"Yellow"))   %>%
#   select(Database, SiteCode, CollectionDate) %>%
#   unique() %>% as.data.frame()


veg_metrics<-hydroveg_df %>%
  mutate(Plant_Species = str_trim(Plant_Species)) %>% # Remove carriage returns, leading white space and trailing white space
  mutate(Plant_Species = case_when(
    Plant_Species %in% c("NR","?") ~ "No plant",
    T~Plant_Species
  )) %>%
  mutate(Plant_Status = case_when(
    is.na(Plant_Status) == T ~ "nonhydrophyte",
    T~ Plant_Status),
    Plant_flag = case_when(
      is.na(Plant_flag) == T ~ "no",
      T~Plant_flag
    )) %>%
  mutate(hydrophyte = Plant_Status %in% c("hydrophyte", "facw", "obl/sav"),
         obl = Plant_Status %in% c("obl/sav"),
         flagged = Plant_flag=="yes") %>%
  # select(ParentGlobalID, Plant_Species, hydrophyte, obl, flagged, InChannel) %>%
  transmute(ParentGlobalID, Plant_Species, 
            hydrophyte = Plant_Status %in% c("hydrophyte","hydrophyte_facw","hydrophyte_obl","obl/sav","facw"), 
            flagged=Plant_flag=="yes") %>%
  # filter(hydrophyte) %>%
  unique()%>%
  group_by(ParentGlobalID) %>%
  summarise(plants_reported=length(Plant_Species),
            hydrophytes_present = sum(hydrophyte, na.rm=T),
            hydrophytes_present_noflag = sum(hydrophyte[!flagged], na.rm=T),
            
            hydrophytes_present_any = hydrophyte %>% any(na.rm=T) %>% sum(na.rm=T),
            hydrophytes_present_any_noflag = hydrophyte[!flagged] %>% any(na.rm=T) %>% sum(na.rm=T)
            
  ) %>% 
  ungroup()

main_df<-main_df %>%
  left_join(veg_metrics %>% select(ParentGlobalID, hydrophytes_present, hydrophytes_present_noflag)) %>%
  mutate(hydrophytes_present = case_when(is.na(hydrophytes_present)~0,T~hydrophytes_present),
         hydrophytes_present_noflag = case_when(is.na(hydrophytes_present_noflag)~0,T~hydrophytes_present_noflag))


#######AQUATIC INVERTEBRATES
ai_df<-
  read_csv("https://sdamchecker.sccwrp.org/checker/download/aquatic_invertebrates-all") %>%
  transmute(
    ParentGlobalID = parentglobalid,
    AIGlobalID=globalid,
    SiteCode = ai_sitecode, 
    AI_Taxon=taxon,
    AI_Lifestage=lifestage,
    AI_LiveDead=ai_living, 
    AI_Abundance=case_when(ai_abundance<0~NA_real_, T~ai_abundance), #Negative values are actually blanks
    AI_Notes= ai_notes) %>%
  filter(AI_Taxon != "Na" & AI_Taxon != "na" & !(is.na(AI_Taxon)))
skim_without_charts(ai_df)

ai_df %>% filter(AI_Taxon=="Ranis") %>% 
  select(-SiteCode) %>%
  inner_join(main_df %>% select(ParentGlobalID, SiteCode, Database, CollectionDate)) %>% 
  arrange(Database, SiteCode, CollectionDate) %>%
  as.data.frame()
ai_df$AI_Taxon %>% unique() %>% sort()

"{fccec8b8-25d5-443d-9a1b-3f28289199d3}" %in% main_df$ParentGlobalID
main_df %>% filter(ParentGlobalID=="{fccec8b8-25d5-443d-9a1b-3f28289199d3}")

ai_df %>%
  
#################
BioPreds<-c(
  #NM varz
  "fishabund_score2","BMI_score","Algae_score","DifferencesInVegetation_score",
  "UplandRootedPlants_score","iofb_score",
  #BMI varz
  "mayfly_abundance",
  "perennial_abundance","perennial_taxa","perennial_live_abundance", #"Perennial" means that it's a metric based on perennial indicator taxa from Mazacanno and Black 2012
  #Vertebrates
  "snake_score","turt_score","vert_score",
  # "vertvoc_score","vertvoc_sumscore",#"frogvoc_score",
  "vert_sumscore",
  #Hydrophytes
  "hydrophytes_present",
  "hydrophytes_present_noflag",
  #Novel bio
  "alglive_cover_score","algdead_cover_score","algdead_noupstream_cover_score",
  "alglivedead_cover_score",
  "moss_cover_score","liverwort_cover_score","PctShading",
  "ripariancorr_score", #This is from the PNW ancillary data
  #A few novel BMI metrics
  "TotalAbundance", "Richness",  
  "EPT_abundance", "EPT_taxa", "EPT_relabd","EPT_reltaxa",
  "GOLD_abundance", "GOLD_taxa", "OCH_abundance",   "OCH_taxa",
  "GOLD_relabd", "GOLD_reltaxa", "OCH_relabd","OCH_reltaxa", "GOLDOCH_relabd","GOLDOCH_reltaxa",  
  "Noninsect_abundance", "Noninsect_taxa", "Noninsect_relabund",   "Noninsect_reltaxa"
)
#These require taxonomic IDs and can't be evaluated until those data are reconciled
#OK now
BugPreds<-c("TotalAbundance", "Richness",  
            "EPT_abundance", "EPT_taxa", "EPT_relabd","EPT_reltaxa",
            "GOLD_abundance", "GOLD_taxa", "OCH_abundance",   "OCH_taxa",
            "GOLD_relabd", "GOLD_reltaxa", "OCH_relabd","OCH_reltaxa", "GOLDOCH_relabd","GOLDOCH_reltaxa",  
            "Noninsect_abundance", "Noninsect_taxa", "Noninsect_relabund",   "Noninsect_reltaxa")
setdiff(BioPreds, names(metrics_df))


HydroPreds<-c(
  #NM varz
  "WaterInChannel_score","HydricSoils_score","springs_score",
  #Others and novel
  "SurfaceFlow_pct","SurfaceSubsurfaceFlow_pct","IsolatedPools_number","WoodyJams_number",
  "SoilMoist_MeanScore",
  "SoilMoist_MaxScore"
)

WaterPreds<-c("WaterInChannel_score", "springs_score", 
              "SurfaceFlow_pct","SurfaceSubsurfaceFlow_pct","IsolatedPools_number","SoilMoist_MeanScore", "SoilMoist_MaxScore")

HydroPreds_nowater<-HydroPreds %>%  setdiff(WaterPreds)

setdiff(HydroPreds, names(metrics_df))

GeomorphPreds<-c(
  #NM varz
  "Sinuosity_score","ChannelDimensions_score","RifflePoolSeq_score",
  "SubstrateSorting_score","SedimentOnPlantsDebris_score",
  #Other varz
  "BankWidthMean","Slope"#"erosion_score","floodplain_score"
)
setdiff(GeomorphPreds, names(metrics_df))
metrics_df$Slope

GISPreds<-c(#"Eco1","Eco2","Eco3",
  "Elev_m",
  "tmean","tmax","tmin",
  "MeanSnowPersistence_10","MeanSnowPersistence_05","MeanSnowPersistence_01",
  "ppt","ppt.m01","ppt.m02","ppt.m03","ppt.m04","ppt.m05","ppt.m06","ppt.m07","ppt.m08","ppt.m09","ppt.m10","ppt.m11","ppt.m12")

gis_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df.csv")
GISPreds %in% names(gis_df)
setdiff(HydroPreds, names(main_df))
###################

main_metrics<-main_df %>%
  inner_join(xwalk_df %>% rename(SiteCode=sitecode)) %>%
  inner_join(gis_df )
