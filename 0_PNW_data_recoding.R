library(tidyverse)

pnw_df_raw<-read_csv("Data/PNW_SDAM_FINAL.csv") 
# main_df$W
# pnw_df_raw$Redox
# main_df %>% group_by(HydricSoils_score) %>% tally()
# pnw_df_raw %>% group_by(GW) %>% tally()
# pnw_df_raw %>% group_by(Redox) %>% tally()

pnw_df<-pnw_df_raw%>%
  transmute(SiteCode = Site_ID,
            CollectionDate=Date %>% lubridate::mdy(),
            SubstrateSorting_score = case_when(Texture %in% c("0")~0,
                                               Texture %in% c("1", "2")~1.5,
                                               Texture %in% c("3")~3,
                                               T~NA_real_),
            Erosion_Deposition_Score=case_when(Erosion == "0"~0, 
                                               Erosion  %in% c("0.5","1","1.5")~1,
                                               T~NA_real_),
            Sinuosity_score= case_when(Sinuosity %in% c("0")~0,
                                       Sinuosity %in% c("1")~1,
                                       Sinuosity %in% c("2")~2,
                                       Sinuosity %in% c("3")~3,
                                       T~NA_real_),
            Slope = case_when(Slope_t %in% c("VNA","NR")~NA_real_,
                              T~as.numeric(Slope_t)),
            hydrophytes_present_any = case_when(Wtl_pl_present=="VNA"~NA_real_, T~as.numeric(Wtl_pl_present)), ###Cannot do new scoring as Kristina's xwalk indicates
            UplandRootedPlants_score2 = case_when(Roots %in% c("0","1","2")~1,
                                                  Roots %in% c("3")~0,
                                                  T~NA_real_),
             Mosses,
            AlgaeAbundanceMossCover_Score = case_when(Mosses=="VNA"~NA_real_,
                                                      Mosses %in% c("0")~0,
                                                      Mosses %in% c("0.5")~0.5,
                                                      Mosses %in% c("1")~1,
                                                      Mosses %in% c("1.5")~1,
                                                      T~NA_real_),
            AlgaeAbundanceMossCover_Score2 = case_when(Mosses=="VNA"~NA_real_,
                                                      Mosses %in% c("0")~0,
                                                      Mosses %in% c("0.5")~1,
                                                      Mosses %in% c("1","1.5")~2,
                                                      T~NA_real_),
            ironox_bfscore_PNW = case_when(Bacteria %in% c("0")~0,
                                           Bacteria %in% c("1","2","3")~1.5,
                                           T~NA_real_),
            RiparianCorridor_score=case_when(is.na(Riparian)~NA_real_,
                                             Riparian %in% c("VNA")~NA_real_,
                                             T~as.numeric(Riparian)),
            springs_score_NM = case_when(Springs=="VNA"~NA_real_,
                                         Springs %in% c("0","1","2")~0,
                                         Springs=="3"~1.5),
            Amphibian_presence = case_when(SI_amph=="VNA"~NA_real_, #We sometimes assume absent if not reported
                                           T~as.numeric(SI_amph)),
            Fish_PA=case_when(SI_fish=="VNA"~NA_real_, #We sometimes assume absent if not reported
                              T~as.numeric(SI_fish)),
            Fish_UpTo3 = case_when(Fish=="VNA"~NA_real_, T~as.numeric(Fish)),
            BMI_presence=case_when(SI_macro=="VNA"~NA_real_, #We sometimes assume absent if not reported
                                   T~as.numeric(SI_fish)),
            Ephemeroptera_SumOfIndividuals=case_when(Ephem=="VNA"~NA_real_,
                                                     Ephem %in% c("0")~0,
                                                     Ephem %in% c("1")~1,
                                                     Ephem %in% c("2","3","4")~2),
            Plecoptera_SumOfIndividuals=case_when(Plecop=="VNA"~NA_real_,
                                                  Plecop %in% c("0")~0,
                                                  Plecop %in% c("1")~1,
                                                  Plecop %in% c("2","3","4")~2),
            Trichoptera_SumOfIndividuals=case_when(Trichop=="VNA"~NA_real_,
                                                   Trichop %in% c("0")~0,
                                                   Trichop %in% c("1")~1,
                                                   Trichop %in% c("2","3","4")~2),
            Basommatophora_SumOfIndividuals=case_when(Basom=="VNA"~NA_real_,
                                                      Basom %in% c("0")~0,
                                                      Basom %in% c("1")~1,
                                                      Basom %in% c("2","3","4")~2),
            Odonata_SumOfIndividuals=case_when(Odon=="VNA"~NA_real_,
                                               Odon %in% c("0")~0,
                                               Odon %in% c("1")~1,
                                               Odon %in% c("2","3","4")~2),
            Coleoptera_SumOfIndividuals=case_when(Coleop=="VNA"~NA_real_,
                                                  Coleop %in% c("0")~0,
                                                  Coleop %in% c("1")~1,
                                                  Coleop %in% c("2","3","4")~2),
            Hemiptera_SumOfIndividuals=case_when(Hemip=="VNA"~NA_real_,
                                                 Hemip %in% c("0")~0,
                                                 Hemip %in% c("1")~1,
                                                 Hemip %in% c("2","3","4")~2),
            Decapoda_SumOfIndividuals=case_when(Decap=="VNA"~NA_real_,
                                                Decap %in% c("0")~0,
                                                Decap %in% c("1")~1,
                                                Decap %in% c("2","3","4")~2),
            OtherMacro_SumOfIndividuals=case_when(Other_macro=="VNA"~NA_real_,
                                                  Other_macro %in% c("0")~0,
                                                  Other_macro %in% c("1")~1,
                                                  Other_macro %in% c("2","3","4")~2),
            EPT_SumOfIndividuals=Ephemeroptera_SumOfIndividuals+Plecoptera_SumOfIndividuals+Trichoptera_SumOfIndividuals,
            OCH_SumOfIndividuals=Odonata_SumOfIndividuals+Coleoptera_SumOfIndividuals+Hemiptera_SumOfIndividuals,
            
            MacroGroupsPresent = 
              (Ephemeroptera_SumOfIndividuals>0)+
              (Plecoptera_SumOfIndividuals>0)+
              (Trichoptera_SumOfIndividuals>0)+
              (Coleoptera_SumOfIndividuals>0)+
              (Odonata_SumOfIndividuals>0)+
              (Hemiptera_SumOfIndividuals>0)+
              (Basommatophora_SumOfIndividuals>0)+
              (Decapoda_SumOfIndividuals>0)+
              (OtherMacro_SumOfIndividuals>0),
            PerennialPNWMacroPresent=case_when(Per_macro_present=="VNA"~NA_real_,
                                               T~as.numeric(Per_macro_present)),
            BankWidthMean=case_when(Width=="VNA"~NA_real_, T~as.numeric(Width)),
            RifflePoolSeq_score=case_when(Structure=="VNA"~NA_real_, T~as.numeric(Structure)),
            SoilMoist_MaxScore=case_when(GW=="VNA"~NA_real_,
                                         GW %in% c("0")~0,
                                         GW %in% c("1")~1,
                                         GW %in% c("2","3")~2),
            HydricSoils_score=case_when(Redox=="VNA"~NA_real_,
                                        Redox=="0"~0,
                                        Redox=="1.5"~3)
            
  )

pnw_df %>% select(contains("moss"))  %>%
  skim()
  
pnw_df %>%
  na.omit() %>%
  select(SiteCode) %>% unique() 
  skim_without_charts()

# gis_metrics_df<-read_csv("Data/GISmetrics/COMPLETE_gis_metrics_df.csv")
# setdiff(pnw_df$SiteCode, gis_metrics_df$SiteCode)
# pnw_df<-
#   pnw_df %>%
#   inner_join(gis_metrics_df)

write_csv(pnw_df , "NotForGit/Step1/pnw_df_step1.csv")

pnw_df %>% skim_without_charts()
pnw_df %>%
  select(-AlgaeAbundanceMossCover_Score) %>%
  na.omit() %>%
  select(SiteCode) %>%
  unique()

xwalk<-read_csv("NotForGit/Step1/xwalk_df.csv")
pnw_df %>%
  select(-AlgaeAbundanceMossCover_Score) %>%
  na.omit() %>%
  inner_join(
    xwalk %>%
      rename(SiteCode=sitecode)
  ) %>%
  filter(Class!="U") %>%
  select(SiteCode, Class) %>%
  unique() %>%
  group_by(Class) %>%
  tally()

