##############################
##  Project Body Neutrality ##
##  Pilot Data Exploration  ##
##############################

####  Startup  ####
## Load packages
if(!require(qualtRics)){install.packages('qualtRics')}
library(qualtRics)
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)
if(!require(MBESS)){install.packages('MBESS')}
library(MBESS)
if(!require(MOTE)){install.packages('MOTE')}
library(MOTE)

## Download Pilot Data
# To pull a fresh dataset from Qualtrics, set this to TRUE
# In order to do that, you have to register your Qualtrics credentials with the
# qualtRics package: https://github.com/ropensci/qualtRics#register-your-qualtrics-credentials
pull_fresh_dataset <- F

if(pull_fresh_dataset) {
 
  # Pull data
  qualtrics_data <- fetch_survey("SV_6tDDvqz44aZxhCC", 
                                 force_request = T,
                                 label = F,
                                 convert = F)
  
  # Save back-up
  saveRDS(qualtrics_data, "/Volumes/jslab/Body Neutrality SSI/Data/Qualtrics Pilot Data.rds")
  
} else {
  
  # Read back-up
  qualtrics_data <- readRDS("/Volumes/jslab/Body Neutrality SSI/Data/Qualtrics Pilot Data.rds")
  
}

####  Data Exploration  ####
View(qualtrics_data)
str(qualtrics_data)

####  Data Cleaning  ####

## Create summary variables
all_responders <- qualtrics_data %>%
#Variables that are summed (bsss is reverse scored)
  mutate(b_bhs_sum = b_bhs_4_1 + b_bhs_4_2 + b_bhs_4_3 + b_bhs_4_4,
         b_shs_agency_sum = b_shs_agency_1 + b_shs_agency_2 + b_shs_agency_3,
         b_fas_sum = b_fas_1 + b_fas_2 + b_fas_3 + b_fas_4 + b_fas_5 + b_fas_6 + b_fas_7,
         b_bsss_sum = (6 - b_bsss_1) + (6 - b_bsss_2) + (6 - b_bsss_3) + (6 - b_bsss_4) + (6 - b_bsss_5) + 
           (6 - b_bsss_6) + (6 - b_bsss_7) + (6 - b_bsss_8) + (6 - b_bsss_9) + (6 - b_bsss_10),
         pi_pfs_sum = pi_pfs_1 + pi_pfs_2 + pi_pfs_3 + pi_pfs_4 + pi_pfs_5 + pi_pfs_6 + pi_pfs_7,
         pi_bhs_sum = pi_bhs_4_1 + pi_bhs_4_2 + pi_bhs_4_3 + pi_bhs_4_4,
         pi_shs_agency_sum = pi_shs_agency_1 + pi_shs_agency_2 + pi_shs_agency_3,
         pi_fas_sum = pi_fas_1 + pi_fas_2 + pi_fas_3 + pi_fas_4 + pi_fas_5 + pi_fas_6 + pi_fas_7,
         pi_bsss_sum = (6 - pi_bsss_1) + (6 - pi_bsss_2) + (6 - pi_bsss_3) + (6 - pi_bsss_4) + (6 - pi_bsss_5) + 
           (6 - pi_bsss_6) + (6 - pi_bsss_7) + (6 - pi_bsss_8) + (6 - pi_bsss_9) + (6 - pi_bsss_10))
         
## Patters of Use ##

#Identify eligible responders
eligible_responders <- all_responders %>%
  filter(is.na(Eligibility))
  #n = 353 responders total
  #n = 224 eligible responders, regardless of completion

#Identify SSI completers
ssi_completers <- eligible_responders %>%
  rename("ssi_submit" = "bn_bp_bn_like_tim_Page Submit") %>%
  filter(!is.na(ssi_submit))
  #n = 88 eligible, SSI completers

#Identify Program Feedback Scale completers
pfs_completers <- eligible_responders %>%
  filter(!is.na(pi_pfs_sum))
  #n = 81 eligible, Program Feedback Scale completers

#Identify Pre-to-Post measures completers
b_pi_completers <- eligible_responders %>%
  filter(!is.na(b_bhs_sum) & !is.na(b_shs_agency_sum) & !is.na(b_fas_sum) & !is.na(b_bsss_sum) 
         & !is.na(pi_bhs_sum) & !is.na(pi_shs_agency_sum) & !is.na(pi_fas_sum) & !is.na(pi_bsss_sum))
  #n = 75 pre-to-post measure completers


####  Analysis  ####

##Duration

names_in_order <- qualtrics_data %>%
  select(ends_with("Page Submit")) %>%
  names()

#Put mean duration and sd for each page into a new data frame - for completers
page_durations_completers <- b_pi_completers %>% #
  select(ends_with("Page Submit")) %>%
  pivot_longer(cols = everything(),
               names_to = "page",
               values_to = "duration") %>%
  group_by(page) %>%
  summarise(mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration,  na.rm = T), .groups = "drop") 
page_durations_completers
page_durations_completers$page <- factor(page_durations_completers$page, levels = names_in_order)

#Plot mean duration for each page - for completers
ggplot(data = page_durations_completers) +
  geom_col(aes(x = page, y = mean_duration)) +
  coord_flip()

#Identify mean duration and sd for intro and pre-SSI questionnaires, the SSI, and post-SSI questionnaires - for completers

b_ssi_pi_mean_durations_completers <- page_durations_completers %>% 
  mutate(page_category = case_when( #Make a new variable, "page_category":
    grepl("^b_", page) ~ "pre-intervention", #When page starts with "b_", make it "pre-intervention"...
    grepl("^bn_", page) ~ "intervention", #...otherwise, when page starts with "bn_", make it "intervention"...
    grepl("^pi_", page) ~ "post-intervention" #...otherwise, when page starts with "pi", make it "post-intervention"
  )) %>%
  group_by(page_category) %>%
  summarize(mean_duration_seconds = sum(mean_duration), 
            sd_duration_seconds = sum(sd_duration), .groups = "drop") %>%
  mutate(mean_duration_minutes = mean_duration_seconds / 60,
         sd_duration_minutes = sd_duration_seconds / 60) 
b_ssi_pi_mean_durations_completers

#Put mean duration and sd for each page into a new data frame - for all eligible respondents
page_durations_all <- eligible_responders %>% 
  select(ends_with("Page Submit")) %>%
  pivot_longer(cols = everything(),
               names_to = "page",
               values_to = "duration") %>%
  group_by(page) %>%
  summarise(mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration,  na.rm = T), .groups = "drop") 
page_durations_all
page_durations_all$page <- factor(page_durations_all$page, levels = names_in_order)

#Plot mean duration for each page - for all eligible respondents
ggplot(data = page_durations_all) +
  geom_col(aes(x = page, y = mean_duration)) +
  coord_flip()

#Identify mean duration and sd for intro and pre-SSI questionnaires, the SSI, and post-SSI questionnaires - for all eligible respondents
b_ssi_pi_mean_durations_all <- page_durations_all %>% 
  mutate(page_category = case_when( #Make a new variable, "page_category":
    grepl("^b_", page) ~ "pre-intervention", #When page starts with "b_", make it "pre-intervention"...
    grepl("^bn_", page) ~ "intervention", #...otherwise, when page starts with "bn_", make it "intervention"...
    grepl("^pi_", page) ~ "post-intervention" #...otherwise, when page starts with "pi", make it "post-intervention"
  )) %>%
  group_by(page_category) %>%
  summarize(mean_duration_seconds = sum(mean_duration), 
            sd_duration_seconds = sum(sd_duration), .groups = "drop") %>%
  mutate(mean_duration_minutes = mean_duration_seconds / 60,
         sd_duration_minutes = sd_duration_seconds / 60) 
b_ssi_pi_mean_durations_all

#An example histogram - evaluate line-by-line to see what this is doing!
b_pi_completers %>% 
  select(ResponseId, ends_with("Page Submit")) %>%
  pivot_longer(cols = -ResponseId,
               names_to = "page",
               values_to = "duration") %>% 
  mutate(page_category = case_when( #Make a new variable, "page_category":
    grepl("^b_", page) ~ "pre-intervention", #When page starts with "b_", make it "pre-intervention"...
    grepl("^bn_", page) ~ "intervention", #...otherwise, when page starts with "bn_", make it "intervention"...
    grepl("^pi_", page) ~ "post-intervention" #...otherwise, when page starts with "pi", make it "post-intervention"
  )) %>%
  group_by(ResponseId, page_category) %>%
  summarize(duration = sum(duration, na.rm = T) / 60) %>%
  drop_na(duration) %>%
  ggplot() +
    geom_histogram(aes(duration)) +
    facet_wrap(. ~ page_category, scales = "free")
                    
##Program Feedback Scale

#Mean and sd of each person's sum score
mean(pfs_completers$pi_pfs_sum)
sd(pfs_completers$pi_pfs_sum)

#Individual item scores
pfs_completers %>%
  select(pi_pfs_1:pi_pfs_7) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value))

## Hopelessness
plot(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum)
t.test(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum, paired = TRUE)
b_pi_completers %>%
  select(b_bhs_sum, pi_bhs_sum) %>%
  mutate(bhs_sum_change = pi_bhs_sum - b_bhs_sum) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = bhs_sum_change_mean / bhs_sum_change_sd,
         d_av = bhs_sum_change_mean / ((b_bhs_sum_sd + pi_bhs_sum_sd) / 2))

#Calculate the summary statistics we need to compute effect sizes, and save them in bhs_stats
bhs_stats <- b_pi_completers %>%
  select(b_bhs_sum, pi_bhs_sum) %>%
  mutate(bhs_sum_change = pi_bhs_sum - b_bhs_sum) %>%
  summarize(n = n(),
            across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = bhs_sum_change_mean / bhs_sum_change_sd,
         d_av = bhs_sum_change_mean / ((b_bhs_sum_sd + pi_bhs_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(bhs_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = bhs_stats$bhs_sum_change_mean,
             sddiff = bhs_stats$bhs_sum_change_sd,
             n = bhs_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = bhs_stats$b_bhs_sum_mean,
            m2 = bhs_stats$pi_bhs_sum_mean,
            sd1 = bhs_stats$b_bhs_sum_sd,
            sd2 = bhs_stats$pi_bhs_sum_sd,
            n = bhs_stats$n,
            a = .05) %>%
  pluck("estimate")

## Agency - this is the subscale we messed up!
plot(b_pi_completers$b_shs_agency_sum, b_pi_completers$pi_shs_agency_sum)
t.test(b_pi_completers$b_shs_agency_sum, b_pi_completers$pi_shs_agency_sum, paired = TRUE)
b_pi_completers %>%
  select(b_shs_agency_sum, pi_shs_agency_sum) %>%
  mutate(shs_agency_sum_change = pi_shs_agency_sum - b_shs_agency_sum) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = shs_agency_sum_change_mean / shs_agency_sum_change_sd,
         d_av = shs_agency_sum_change_mean / ((b_shs_agency_sum_sd + pi_shs_agency_sum_sd) / 2))

## Functionality Appreciation Scale
plot(b_pi_completers$b_fas_sum, b_pi_completers$pi_fas_sum)
t.test(b_pi_completers$b_fas_sum, b_pi_completers$pi_fas_sum, paired = TRUE)
b_pi_completers %>%
  select(b_fas_sum, pi_fas_sum) %>%
  mutate(fas_sum_change = pi_fas_sum - b_fas_sum) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = fas_sum_change_mean / fas_sum_change_sd,
         d_av = fas_sum_change_mean / ((b_fas_sum_sd + pi_fas_sum_sd) / 2))

## Body Dissatisfaction 
plot(b_pi_completers$b_bsss_sum, b_pi_completers$pi_bsss_sum)
t.test(b_pi_completers$b_bsss_sum, b_pi_completers$pi_bsss_sum, paired = TRUE)
b_pi_completers %>%
  select(b_bsss_sum, pi_bsss_sum) %>%
  mutate(bsss_sum_change = pi_bsss_sum - b_bsss_sum) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = bsss_sum_change_mean / bsss_sum_change_sd,
         d_av = bsss_sum_change_mean / ((b_bsss_sum_sd + pi_bsss_sum_sd) / 2))


## Demographics

# Sex assigned at birth
b_pi_completers %>%
  count(b_dem_sex) %>%
  mutate(percent = (n / sum(n))*100)

# Gender 
b_pi_completers %>%
  select(starts_with("b_dem_gender")) %>%
  select(-"b_dem_gender_16_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarize(across(everything(), mean)) 
  
#Sexual orientation
b_pi_completers %>%
  count(b_dem_orientation) %>%
  mutate(percent = (n / sum(n))*100)

#Race and ethnicity
b_pi_completers %>%
  select(starts_with("b_dem_race_ethnicity")) %>%
  select(-"b_dem_race_ethnicity_7_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarize(across(everything(), mean))

#Disability
b_pi_completers %>%
  count(b_dem_disability_1) %>%
  mutate(percent = (n / sum(n))*100)

#Age
b_pi_completers %>%
  count(b_dem_age) %>%
  mutate(percent = (n / sum(n))*100)
