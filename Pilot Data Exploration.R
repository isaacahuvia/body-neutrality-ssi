##############################
##  Project Body Neutrality ##
##  Pilot Data Exploration  ##
##############################

####  Startup  ####
## Load packages
library(qualtRics)
library(tidyverse)
library(effsize)

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
  filter(!is.na(pi_pfs_1) & !is.na(pi_pfs_2) & !is.na(pi_pfs_3) & !is.na(pi_pfs_4)
         & !is.na(pi_pfs_5) & !is.na(pi_pfs_6) & !is.na(pi_pfs_7))
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

#Put mean duration for each page into a new data frame
page_durations <- qualtrics_data %>% #change to b_pi_completers dataset? If I do, I get a NaN result for line 120 and I'm not sure why :/
  select(ends_with("Page Submit")) %>%
  pivot_longer(cols = everything(),
               names_to = "page",
               values_to = "duration") %>%
  group_by(page) %>%
  summarise(mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration,  na.rm = T)) #seems odd/incorrect that the values I'm getting for standard deviation are so giant

page_durations$page <- factor(page_durations$page, levels = names_in_order)

#Plot mean duration for each page
ggplot(data = page_durations) +
  geom_col(aes(x = page, y = mean_duration)) +
  coord_flip()

#Identify mean duration for intro and pre-SSI questionnaires, the SSI, and post-SSI questionnaires
b_ssi_pi_mean_durations <- page_durations %>% 
  pivot_wider(names_from = page, values_from = mean_duration) %>% 
  rename_with(~str_replace(.,'tim_Page Submit', 'page_submit')) %>% #didn't need this but leaving for my future reference
  mutate(b_mean_duration_sec = sum(across(starts_with("b_")))) %>%  
  mutate(b_mean_duration_minute = b_mean_duration_sec / 60) %>% 
  mutate(ssi_mean_duration_sec = sum(across(starts_with("bn_")))) %>% #how to account for the fact that not every single person saw every page (e.g., the multiple story options)?
  mutate(ssi_mean_duration_minute = ssi_mean_duration_sec / 60) %>%
  mutate(pi_mean_duration_sec = sum(across(starts_with("pi_")))) %>% 
  mutate(pi_mean_duration_minute = pi_mean_duration_sec / 60)

b_ssi_pi_mean_durations$b_mean_duration_minute #4.112764 minutes
b_ssi_pi_mean_durations$ssi_mean_duration_minute #27.76778 minutes
b_ssi_pi_mean_durations$pi_mean_duration_minute #3.57133 minutes
                        
#should we also do standard deviations? If so, what's the best way? 

##Program Feedback Scale

#Total score (items summed) - how to better explain the difference between this and the one below?
mean(pfs_completers$pi_pfs_sum)
sd(pfs_completers$pi_pfs_sum)

#Overall score (items not summed) - how to better explain the difference between this and the one above?
pi_pfs_all_items <- data.frame(pfs_completers$pi_pfs_1, pfs_completers$pi_pfs_2, pfs_completers$pi_pfs_3,
                            pfs_completers$pi_pfs_4, pfs_completers$pi_pfs_5, pfs_completers$pi_pfs_6, 
                            pfs_completers$pi_pfs_7)
mean(as.matrix(pi_pfs_all_items))
sd(as.matrix(pi_pfs_all_items))

#Individual item scores
mean(pfs_completers$pi_pfs_1)
sd(pfs_completers$pi_pfs_1)
mean(pfs_completers$pi_pfs_2)
sd(pfs_completers$pi_pfs_2)
mean(pfs_completers$pi_pfs_3)
sd(pfs_completers$pi_pfs_3)
mean(pfs_completers$pi_pfs_4)
sd(pfs_completers$pi_pfs_4)
mean(pfs_completers$pi_pfs_5)
sd(pfs_completers$pi_pfs_5)
mean(pfs_completers$pi_pfs_6)
sd(pfs_completers$pi_pfs_6)
mean(pfs_completers$pi_pfs_7)
sd(pfs_completers$pi_pfs_7)

## Hopelessness
plot(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum)
t.test(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum, paired = TRUE)
b_pi_completers %>%
  select(b_bhs_sum, pi_bhs_sum) %>%
  mutate(bhs_sum_change = pi_bhs_sum - b_bhs_sum) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = bhs_sum_change_mean / bhs_sum_change_sd,
         d_av = bhs_sum_change_mean / ((b_bhs_sum_sd + pi_bhs_sum_sd) / 2))

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
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "gender", values_to = "n") %>% 
  mutate(percent_all_selections = (n / sum(n))*100) %>%  #is it correct to use the sum that accounts for people selecting multiple choices (n=146)...
  mutate(percent_of_respondents = (n / 75)*100) #...or should we use the number of respondents (n=75). where to get this n using code?

#Sexual orientation
b_pi_completers %>%
  count(b_dem_orientation) %>%
  mutate(percent = (n / sum(n))*100)

#Race and ethnicity
b_pi_completers %>%
  select(starts_with("b_dem_race_ethnicity")) %>%
  select(-"b_dem_race_ethnicity_7_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "race/ethnicity", values_to = "n") %>% 
  mutate(percent_all_selections = (n / sum(n))*100) %>%  #is it correct to use the sum that accounts for people selecting multiple choices (n=97)...
  mutate(percent_of_respondents = (n / 75)*100) #...or should we use the number of respondents (n=75). where to get this n  using code?

#Disability
b_pi_completers %>%
  count(b_dem_disability_1) %>%
  mutate(percent = (n / sum(n))*100)

#Age
b_pi_completers %>%
  count(b_dem_age) %>%
  mutate(percent = (n / sum(n))*100)
