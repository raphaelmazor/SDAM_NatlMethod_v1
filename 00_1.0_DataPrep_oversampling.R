################################################################################
# This script takes the NESE input spreadsheet and formats it appropriately
# - NaN handling
# - dtype transformation, where needed
# - oversampling
# - split into train/test
# NOTE: the input spreadsheet is the version that includes BMI_alts
################################################################################
library(rlang)
library(tidyverse)
library(ggplot2)
library(skimr)
library(rstudioapi)
library(grid)
library(gridExtra)
library(rmarkdown)
library(knitr)
library(readxl)

# If TRUE, saves auxillary spreadsheets and plots
save_all_output <- TRUE

# Set your working directory relative to this script
this_script_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(this_script_path))
HOME_DIR <- getwd()
print(paste("Your working dir has been set to:", HOME_DIR))

# get input dataset
df_input <- read_csv(paste0(HOME_DIR,"/input/nese_input_data.csv"))

# Create dir for storage
store_input_dir <- paste0(HOME_DIR, "/input/store")
if (!dir.exists(store_input_dir)){dir.create(store_input_dir)}

eda_dir <- paste0(HOME_DIR, "/output/eda")
if (!dir.exists(eda_dir)){dir.create(eda_dir)}

# Renaming
df_input <- df_input %>%
  rename(
    Class = Determination_final #Class is what we are trying to predict
  )

#############################  LISTS OF DATA ##########################
# Use csv to store details about each metric (for convenience) for use in 
# later scripts. Make updates in csv as needed.
#
###########################################################################

# use data dictionary to subset data into lists
parameters_info <- read_excel(paste0(HOME_DIR,"/input/data_dictionary.xlsx"))

info_list <- (parameters_info %>% filter(Action =="info"))$Column
info_list <- replace(info_list, info_list=="Determination_final","Class") #Rename class for clarity
info_list <- setdiff(info_list, "Disturbances")
info_list <- c(info_list, "Lat_field","Long_field")
candidate_list <- (parameters_info %>% filter(Action =="candidate"))$Column
# candidate_list <- setdiff(candidate_list, c("BMI_score","BMI_score_alt1","BMI_score_alt2","BMI_score_alt3","BMI_score_alt4"))

bio_list <- (parameters_info %>% filter(Category =="Biological" & Action =="candidate"))$Column
geomorph_list <- (parameters_info %>% filter(Category =="Geomorphological" & Action =="candidate"))$Column
gis_list <- (parameters_info %>% filter(Category =="Geospatial" & Action =="candidate"))$Column
h20_indirect_list <- (parameters_info %>% filter(Category =="Hydrological" & Action =="candidate"))$Column
# h20_direct_list <- (parameters_info %>% filter(Category =="H20 (Direct)" & Action =="candidate"))$Column

con_list <- (parameters_info %>% filter(Dtype =="Con" & Action =="candidate"))$Column
ord_list <- (parameters_info %>% filter(Dtype =="Ord" & Action =="candidate"))$Column
bin_list <- (parameters_info %>% filter(Dtype =="Bin" & Action =="candidate"))$Column

## rm caribbean data
df_input <- (df_input %>% filter(REGION %in% c("NE","SE")))

## Remove H20 and GIS preds from candidate list along w Reject preds
df_input <- df_input %>% select(info_list, candidate_list)

# get smaller dataframes for eda
# df_h20_direct <- df_input[,h20_direct_list]
df_h20_indirect <- df_input[,h20_indirect_list]
df_bio <- df_input[,bio_list]
df_gis <- df_input[,gis_list]
df_geomorph <- df_input[,geomorph_list]


################################################################################
# Prepare the input dataset
# Carribbean sites for Testing only
# 1. Keep only candidate metric columns
# 2. Put "Unknown" classes in a separate file - not going to use for model 
#    building, but may want to reference later
# 3. Split into training and testing datasets
# 4. Perform oversampling on the training set
# 5. drop nans
################################################################################

# 1. Keep only candidate metric columns
print(paste("Number of candidate indicators:", length(candidate_list)))
df_input_cand_only <- df_input %>% select(info_list, candidate_list)

# 2. Put "Unknown" classes in a separate file
df_unknowns <- df_input_cand_only %>% filter(Class=="U")
if (save_all_output==TRUE){write.csv(df_unknowns,"input/store/df_unknowns.csv")}

df_input_cleaned <- df_input_cand_only %>% filter(Class!="U")


# 2.a. Make chart showing removal of unknown classes
four_classes <- ggplot(data = df_input_cand_only, aes(Class))+
  geom_histogram(aes(fill=Class),stat="count", fill="cornflowerblue", 
                 show.legend = FALSE)+
  geom_text(stat="count", aes(label=..count..), vjust=1.6, 
            color="white", size=3.5)+
  labs(caption = paste("Total samples:", dim(df_input_cand_only)[1]),
       y="Count", x="Class")

three_classes <- ggplot(data = df_input_cleaned, aes(Class))+
  geom_histogram(aes(fill=Class),stat="count", fill="cornflowerblue", 
                 show.legend = FALSE)+
  geom_text(stat="count", aes(label=..count..), vjust=1.6, 
            color="white", size=3.5)+
  labs(caption = paste("Total samples:", dim(df_input_cleaned)[1]),
       y="Count", x="Class")

if (save_all_output==TRUE){
  ggsave(paste0(eda_dir, "/data_prep_unknowns_removed.png"),
       arrangeGrob(four_classes, three_classes, 
                  nrow = 1, top=textGrob("Removing Unknown Class for Beta Development")),
       dpi=300, height=6, width=12)}


# 3. Split into training and testing datasets

# 3.a. First.. perform some additional clean up
# Correct data type
df_input_cleaned$Class <- as.factor(df_input_cleaned$Class)
df_input_cleaned$REGION <- as.factor(df_input_cleaned$REGION)
df_input_cleaned$HydricSoils_score <- as.numeric(df_input_cleaned$HydricSoils_score)
print(class(df_input_cleaned$CollectionDate))

# NOTE: had to manually change dtype in the input csv of this column in order for R to understand it correctly!!
df_input_cleaned <- df_input_cleaned[!(df_input_cleaned$CollectionDate == "" | is.na(df_input_cleaned$CollectionDate)), ]
print(class(df_input_cleaned$CollectionDate))
df_input_cleaned$CollectionDate <- as.factor(df_input_cleaned$CollectionDate)
df_input_cleaned$CollectionDate <- as.Date(df_input_cleaned$CollectionDate, format="%m/%d/%Y")


## Make a column w total number of times a site was visited
set.seed(1)
df_input_cleaned <- df_input_cleaned %>% group_by(SiteCode) %>%
  mutate(TotalVisits = n()) %>%
  ungroup()

## add revisted flag
df_input_cleaned <- df_input_cleaned %>%
  mutate(revist = case_when(TotalVisits>1~TRUE, T~FALSE))

# Plot: Check distribution across Dates 
df_input_cleaned$month <- strftime(df_input_cleaned$CollectionDate, "%b")
df_input_cleaned$month <- factor(df_input_cleaned$month,
                       levels=c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"))

plot_all_samples_by_month <- ggplot(df_input_cleaned, aes(month)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of all samples by month",
    subtitle = "(Before train-test split or oversampling)", y="",x="",
    caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) + 
  theme(axis.text.x = element_text(angle = 90))
print(plot_all_samples_by_month)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_month, 
  filename=paste0(eda_dir, "/data_prep_samples_by_month.png"), 
  dpi=300, height=4, width=6)}

# Plot: Check distribution across Dates 
df_input_cleaned$mY <- strftime(df_input_cleaned$CollectionDate, "%Y-%m")
df_input_cleaned$mY <- factor(df_input_cleaned$mY)

plot_all_samples_by_mY <- ggplot(df_input_cleaned, aes(month)) + geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of all samples by month",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) + 
  theme(axis.text.x = element_text(angle = 90))
print(plot_all_samples_by_mY)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_mY, 
    filename=paste0(eda_dir,"/plot_all_samples_by_mY.png"), 
    dpi=300, height=4, width=6)}

# Get a list of all unique siteCodes 
site_info <- unique(df_input_cleaned %>% select(Class, SiteCode, REGION))

# Plot: Check distribution of Samples across Region
plot_all_samples_by_region <- ggplot(df_input_cleaned, aes(REGION)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of all SAMPLES by Region",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) 
print(plot_all_samples_by_region)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_region, 
    filename=paste0(eda_dir, "/data_prep_samples_by_region.png"), 
    dpi=300, height=5, width=5)}

# Plot: Check distribution of Samples by Class
plot_all_samples_by_class<- ggplot(df_input_cleaned, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of all SAMPLES by Class",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) 
print(plot_all_samples_by_class)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_class, 
    filename=paste0(eda_dir, "/data_prep_samples_by_class.png"), 
    dpi=300, height=5, width=5)}

# Plot: Check distribution of Samples across Region
plot_all_samples_by_region_class <- ggplot(df_input_cleaned,
    aes(x=Class, fill=REGION)) + geom_bar(stat='count') +
  labs(title = "Distribution of all Samples by Region and Class",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) +
  geom_text(stat="count", aes(label=after_stat(..count..)),
       vjust=-5,
       color="black", size=2.8)
print(plot_all_samples_by_region_class)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_region_class, 
    filename=paste0(eda_dir, "/plot_all_samples_by_region_class.png"), 
    dpi=300, height=5, width=5)}


plot_all_samples_by_class_region <- ggplot(df_input_cleaned, 
  aes(x=REGION, fill=Class)) + 
  geom_bar(stat='count') +
  labs(title = "Distribution of all SAMPLES by Region and Class",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_input_cleaned)[1])) 
print(plot_all_samples_by_class_region)
if (save_all_output==TRUE){ggsave(plot_all_samples_by_class_region, 
    filename=paste0(eda_dir, "/data_prep_samples_by_class_region.png"), 
    dpi=300, height=5, width=5)}


plot_all_sites_by_class_region <- ggplot(site_info, 
                                       aes(x=REGION, fill=Class)) + geom_bar(stat='count') +
labs(title = "Distribution of all SITES by Region and Class",
     subtitle = "(Before train-test split or oversampling)", y="",x="",
     caption=paste0("Number of sites: ", dim(site_info)[1])) 
print(plot_all_sites_by_class_region)
ggsave(plot_all_sites_by_class_region, filename=paste0(eda_dir, 
         "/data_prep_sites_by_class_region.png"), dpi=300, height=5, width=5)

# Plot: Check distribution of Sites across Region
plot_all_sites_by_region <- ggplot(site_info, aes(REGION)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of SITES by Region",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of Sites: ", dim(site_info)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) 
print(plot_all_sites_by_region)
if (save_all_output==TRUE){ggsave(plot_all_sites_by_region, 
    filename=paste0(eda_dir, "/data_prep_sites_by_region.png"), 
    dpi=300, height=5, width=5)}

# Plot: Check distribution of Sites by class
plot_all_sites_by_class <- ggplot(site_info, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(title = "Distribution of SITES by Class",
       subtitle = "(Before train-test split or oversampling)", y="",x="",
       caption=paste0("Number of Sites: ", dim(site_info)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) 
print(plot_all_sites_by_class)
if (save_all_output==TRUE){ggsave(plot_all_sites_by_class, 
    filename=paste0(eda_dir, "/data_prep_sites_by_class.png"), 
    dpi=300, height=5, width=5)}

## Split into training vs testing based on site codes
set.seed(2)
## ALL CB SITES, THEN EVENLY DISTRIBUTE NE AND SE
testing_sites1 <- site_info %>% filter(REGION=="CB")
testing_sites2 <- site_info %>% filter(REGION %in% c("NE","SE"))%>% 
    group_by(Class,REGION) %>% 
    slice_sample(prop = 0.2) 
testing_sites <- rbind(testing_sites1, testing_sites2)

testing_sites_list <- unique(testing_sites$SiteCode)
training_sites_list <- setdiff(site_info$SiteCode, testing_sites_list) 

## HANDLE NANs (NOTE DISTURBANCES SHOULD BE FILLED)
# df_input_cleaned <- df_input_cleaned %>% select(-c("Disturbances"))

df_test <- df_input_cleaned %>% filter(SiteCode %in% testing_sites_list)
df_train <- df_input_cleaned %>% filter(SiteCode %in% training_sites_list)

## Check that number samples in training + testing adds up to original
print(paste("Training sites:", dim(df_train)[1]))
print(df_train %>% count(Class))
print(df_train %>% count(REGION))
print(paste("Testing sites:", dim(df_test)[1]))
print(df_test %>% count(Class))
print(df_test %>% count(REGION))
dim(df_train)[1] + dim(df_test)[1] == dim(df_input_cleaned)[1]

## Are there any sites in training that are also in testing? Shouldn't be.
any(df_train$SiteCode %in% df_test$SiteCode)
any(df_test$SiteCode %in% df_train$SiteCode)

#Add qualifier
df_test$Dataset <- "Testing"
df_train$Dataset <- "Training"
df_train$Notes <- "Train"
df_test$Notes <- "Test"


##Add state column
library(tidyr)
library(dplyr)
df_test_train <- rbind(df_train, df_test)
df_test_train$State <- substr(df_test_train$SiteCode,1,2)
if (save_all_output==TRUE){write.csv(df_test_train, "input/store/df_test_train.csv")}

################################# DATA AUGMENTATION ############################
# Set aside some test sites to be used to check model quality. All samples from
# the test sites are withheld from model development. Want test sites that
# are balanced by strata, class, etc.
################################################################################
## Prep training data
set.seed(3)

# If site was only visited 1x, repeat row 4x
df_1visit <- df_train %>%
  filter(TotalVisits==1) %>%
  slice(rep(1:n(), each = 4)) %>%
  group_by(ParentGlobalID)%>%
  mutate(Augment = 1:n(),
         Notes = case_when(Augment ==1 ~"Original",
                           T~"Augmented")) %>%
  select(-Augment) %>%
  ungroup()

# If site was only visited 2x, repeat row twice
df_2visit <- df_train %>%
  filter(TotalVisits==2) %>%
  slice(rep(1:n(), each = 2)) %>%
  group_by(ParentGlobalID)%>%
  mutate(Augment = 1:n(),
         Notes = case_when(Augment ==1 ~"Original",
                           T~"Augmented"))%>%
  select(-Augment)%>%
  ungroup()

# Leave sites visited 3x and 4x as-is
df_3_4visit <- df_train %>%
  filter(TotalVisits==3|TotalVisits==4)  %>%
  mutate(Notes = "Original")

# Create augmented dataframe
df_train_aug <- rbind(df_1visit, df_2visit, df_3_4visit) %>% ungroup()
if (save_all_output==TRUE){write.csv(df_train_aug, "input/store/df_train_aug.csv")}

df_model <- rbind(df_train_aug, df_test)
write.csv(df_model, "input/df_model.csv")

df_orig <- rbind(df_train, df_test)

# Plot: Distribution of site visits
plot_no_site_visits <- ggplot(df_train, aes(TotalVisits)) + 
  geom_bar(fill="cornflowerblue") +
  labs(#title = "Number of Times Site Was Visited",
       subtitle = "(Before oversampling)", 
       y="",x="",
       caption=paste0("Number of samples: ", dim(df_train)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=3) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=4) + ylim(0,560)
print(plot_no_site_visits)
if (save_all_output==TRUE){ggsave(plot_no_site_visits, 
    filename=paste0(eda_dir, "/plot_no_site_visits.png"), 
    dpi=300, height=5, width=5)}


# Plot: Distribution of site visits AUG
plot_no_site_visits_aug <- ggplot(df_train_aug, aes(TotalVisits)) + 
  geom_bar(fill="cornflowerblue") +
  labs(#title = "Number of Times Site Was Visited",
       subtitle = "(After oversampling)",
       y="",x="",
       caption=paste0("Number of Samples: ", dim(df_train_aug)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=3) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=4) + ylim(0,560)
print(plot_no_site_visits_aug)
if (save_all_output==TRUE){ggsave(plot_no_site_visits_aug, 
    filename=paste0(eda_dir, "/plot_no_site_visits_aug.png"), 
    dpi=300, height=5, width=5)}

# Plot: Distribution of site visits
plot_no_site_visits_test <- ggplot(df_test, aes(TotalVisits)) + 
  geom_bar(fill="cornflowerblue") +
  labs(#title = "Number of Times Site Was Visited",
    subtitle = "(Test set - withheld from beta method development)", 
    y="",x="",
    caption=paste0("Number of samples: ", dim(df_test)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=3) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=4) + ylim(0,560)
print(plot_no_site_visits_test)
if (save_all_output==TRUE){ggsave(plot_no_site_visits, 
    filename=paste0(eda_dir, "/plot_no_site_visits_test.png"), 
    dpi=300, height=5, width=5)}

# ggsave(paste0(eda_dir, "/no_site_visits_before_after.png"),
#        arrangeGrob(plot_no_site_visits, plot_no_site_visits_aug,
#                    nrow = 1,#),
#                    top=textGrob("Number of Times Site Was Visited")),
#        dpi=300, height=6, width=12)

# ggsave(paste0(eda_dir, "/no_site_visits_before_after_3.png"),
#        arrangeGrob(plot_no_site_visits, plot_no_site_visits_aug, plot_no_site_visits_test,
#          nrow = 1,#),
#         top=textGrob("Number of Times Site Was Visited")),
#        dpi=300, height=6, width=18)
# 
# ############################# MAP OF SIRES AND SAMPLES ########################
# # Move this 
# #
# ################################################################################
# library(ggmap)
# 
# nese_basemap <- get_stamenmap(bbox = c(left = -88, bottom = 25, 
#                                        right = -71, top = 45),
#                               zoom = 6)
# 
# site_visits <- df_model %>% filter(Notes=="Original") %>% select(all_of(c(info_list,"TotalVisits")))
# nese_map <- ggmap(nese_basemap) + geom_point(data = site_visits, 
#                                  aes(x = Long_field, 
#                                      y = Lat_field,
#                                      color=REGION, 
#                                      size=TotalVisits
#                                      # alpha=0.05
#                                      ))
# nese_map
# 
# ggsave( paste0(eda_dir, "/nese_map.png"), 
#        dpi=300, height=12, width=12)

##################################  PLOTS  ####################################
# Study distributions
#
###############################################################################

# Plot: Ensure that distribution of classes is equivalent to original
plot_train_dist <- ggplot(df_train, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Training data (before oversampling)", y="",x="",
    caption=paste0("Number of samples: ", dim(df_train)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,500))
print(plot_train_dist)

plot_test_dist <- ggplot(df_test, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Testing data (withheld from beta method development)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_test)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
ylim(c(0,500))
print(plot_test_dist)

plot_train_aug_dist <- ggplot(df_train_aug, aes(Class)) + 
  geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Training data (after oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_train_aug)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,500))
print(plot_train_aug_dist)

if (save_all_output==TRUE){ggsave(paste0(eda_dir, "/before_after_augment.png"), 
       arrangeGrob(plot_train_dist, plot_train_aug_dist, plot_test_dist, 
          nrow = 1, top= "Distribution of Classes"),
          dpi=300, height=4, width=12)}

faceted <- ggplot(site_info, aes(Class, fill=REGION)) + 
  geom_bar() +
  labs(title = "Distribution of Sites by Class",
       # subtitle = "(Before train-test split or oversampling)", 
       y="Number of Samples", x="Class",
       # caption=paste0("Number of Sites: ", dim(site_info%>%filter(REGION=="CB"))[1])
       ) +
  # geom_text(stat="count",
  #           aes(label = scales::percent(..count../sum(..count..))),
  # vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=4)  + facet_wrap(~ REGION)
print(faceted)
if (save_all_output==TRUE){ggsave(faceted, 
    filename=paste0(eda_dir, "/faceted.png"), dpi=300, height=3, width=6)}



## Investigate CB
plot_cb_by_class <- ggplot(site_info%>%filter(REGION=="CB"), aes(Class)) + 
  geom_bar(fill="coral2") +
  labs(title = "Caribbean Sites: Distribution by Class",
       # subtitle = "(Before train-test split or oversampling)", 
       y="Number of Samples", x="Class",
       caption=paste0("Number of Sites: ", dim(site_info%>%filter(REGION=="CB"))[1])) +
  # geom_text(stat="count",
  #           aes(label = scales::percent(..count../sum(..count..))),
            # vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=6) 
print(plot_cb_by_class)
if (save_all_output==TRUE){ggsave(plot_cb_by_class, 
    filename=paste0(eda_dir, "/cb_by_class.png"), 
    dpi=300, height=5, width=5)}

cb_train_dist <- ggplot(df_train%>%filter(REGION=="CB"), aes(Class)) + 
  geom_bar(fill="coral2") +
  labs(subtitle = "Training data (before oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_train%>%filter(REGION=="CB"))[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,30))
print(cb_train_dist)

cb_test_dist <- ggplot(df_test%>%filter(REGION=="CB"), aes(Class)) + 
  geom_bar(fill="coral2") +
  labs(subtitle = "Testing data (withheld from beta method development)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_test%>%filter(REGION=="CB"))[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,30))
print(cb_test_dist)

cb_train_aug_dist <- ggplot(df_train_aug%>%filter(REGION=="CB"), aes(Class)) + 
  geom_bar(fill="coral2") +
  labs(subtitle = "Training data (after oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_train_aug%>%filter(REGION=="CB"))[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,30))
print(cb_train_aug_dist)

if (save_all_output==TRUE){ggsave(paste0(eda_dir, "/cb_before_after_augment.png"), 
       arrangeGrob(cb_train_dist, cb_train_aug_dist, cb_test_dist, 
                   nrow = 1, top= "CB - Distribution of Classes"),
       dpi=300, height=4, width=12)}


# same figures, but with region colored
plot_train_dist_reg <- ggplot(df_train, aes(Class,fill=REGION)) + 
  geom_bar() +
  labs(subtitle = "Training data (before oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_train)[1])) +
  ylim(c(0,500)) +
  scale_fill_manual(values=c("springgreen2","dodgerblue1"), name="",
                    label=c("NE","SE"))
print(plot_train_dist_reg)

plot_test_dist_reg <- ggplot(df_test, aes(Class,fill=REGION)) + 
  geom_bar() +
  labs(subtitle = "Testing data (withheld from method development)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_test)[1])) +
  ylim(c(0,500)) +
  scale_fill_manual(values=c("springgreen2","dodgerblue1"), name="",
    # values=c("coral2","springgreen2","dodgerblue1"), name="",
                    # label=c("CB","NE","SE")
                    )

print(plot_test_dist_reg)

plot_train_aug_dist_reg <- ggplot(df_train_aug, aes(Class,fill=REGION)) + 
  geom_bar() +
  labs(subtitle = "Training data (after oversampling)", y="",x="",
       caption=paste0("Number of samples: ", dim(df_train_aug)[1])) +
  ylim(c(0,500)) +
  scale_fill_manual(values=c("springgreen2","dodgerblue1"), name="",
                    label=c("NE","SE"))
print(plot_train_aug_dist_reg)

if (save_all_output==TRUE){ggsave(paste0(eda_dir, "/before_after_augment_reg.png"), 
       arrangeGrob(plot_train_dist_reg, plot_train_aug_dist_reg, plot_test_dist_reg, 
                   nrow = 1, top= "Distribution of Classes"),
       dpi=300, height=4, width=12)}


# Plot: Ensure that distribution across Dates is equivalent
m1 <- ggplot(df_train, aes(month)) + geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Training data (before oversampling)",
    caption=paste0("Number of samples: ", dim(df_train)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,250))
print(m1)


m2 <- ggplot(df_train_aug, aes(month)) + geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Training data (after oversampling)",
    caption=paste0("Number of samples: ", dim(df_train_aug)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,250))
print(m2)

m3 <- ggplot(df_test, aes(month)) + geom_bar(fill="cornflowerblue") +
  labs(subtitle = "Testing data (withheld from method development)",
    caption=paste0("Number of samples: ", dim(df_test)[1])) +
  geom_text(stat="count",
            aes(label = scales::percent(..count../sum(..count..))),
            vjust=-0.3, color="black", size=2.7) +
  geom_text(stat="count", aes(label=..count..),
            vjust=1.3, color="white", size=2.8) +
  ylim(c(0,250))
print(m3)

if (save_all_output==TRUE){ggsave(paste0(eda_dir, "/before_after_augment_months.png"), 
          arrangeGrob(m1, m2, m3, nrow = 1, 
          top="Distribution of Samples per Month (Collection Date)"))}
