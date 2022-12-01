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
if(!require(flextable)){install.packages('flextable')}
library(flextable)

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
all_responders_number <- count(all_responders)
eligible_responders_number <- count(eligible_responders)
  #n = 353 responders total
  #n = 224 eligible responders, regardless of completion
eligible_responders_percent <- (eligible_responders_number * 100) / all_responders_number
eligible_responders_percent

#Identify SSI completers
ssi_completers <- eligible_responders %>%
  rename("ssi_submit" = "bn_bp_bn_like_tim_Page Submit") %>%
  filter(!is.na(ssi_submit))
ssi_completers_number <- count(ssi_completers) 
 #n = 88 eligible, SSI completers
ssi_completers_percent <- (ssi_completers_number * 100) / eligible_responders_number
ssi_completers_percent

#Identify Program Feedback Scale completers
pfs_completers <- eligible_responders %>%
  filter(!is.na(pi_pfs_sum))
pfs_completers_number <- count(pfs_completers)
  #n = 81 eligible, Program Feedback Scale completers
pfs_completers_percent <- (pfs_completers_number * 100) / eligible_responders_number
pfs_completers_percent

#Identify Pre-to-Post measures completers
b_pi_completers <- eligible_responders %>%
  filter(!is.na(b_bhs_sum) & !is.na(b_shs_agency_sum) & !is.na(b_fas_sum) & !is.na(b_bsss_sum) 
         & !is.na(pi_bhs_sum) & !is.na(pi_shs_agency_sum) & !is.na(pi_fas_sum) & !is.na(pi_bsss_sum))
b_pi_completers_number <- count(b_pi_completers)
  #n = 75 pre-to-post measure completers
b_pi_completers_percent <- (b_pi_completers_number * 100) / eligible_responders_number
b_pi_completers_percent 

####  Analysis  ####

##Duration

names_in_order <- qualtrics_data %>%
  select(ends_with("Page Submit")) %>%
  names()

#Put mean duration and sd for each page into a new data frame - for completers
page_durations_completers <- b_pi_completers %>% #
  select(ends_with("Page Submit")) %>%
  select(-"b_ineligible_tim_Page Submit")  %>%
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

#Put mean duration and sd for each page into a new data frame - for all eligible respondents
page_durations_all <- eligible_responders %>% 
  select(ends_with("Page Submit")) %>%
  select(-"b_ineligible_tim_Page Submit") %>% 
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

##Dropout

#Count all respondents who submitted each page
page_submit_all <- qualtrics_data %>%
  select(ends_with("Page Submit"))
colSums(!is.na(page_submit_all))

#Count eligible respondents who submitted each page
page_submit_eligible <- eligible_responders %>%
  select(ends_with("Page Submit"))
colSums(!is.na(page_submit_eligible))

#Dropout during study information

study_info_submit <- qualtrics_data %>%
  rename("study_info_submit" = "b_screener_intro_tim_Page Submit") %>%
  filter(!is.na(study_info_submit))
study_info_submit_number <- count(study_info_submit) 
study_info_submit_number 
study_info_dropout_number <- all_responders_number - study_info_submit_number
study_info_dropout_number

#Dropout during screener

screener_submit <- qualtrics_data %>%
  rename("screener_submit" = "b_screener_tim_Page Submit") %>%
  filter(!is.na(screener_submit))
screener_submit_number <- count(screener_submit) 
screener_submit_number
screener_dropout_number <- all_responders_number - screener_submit_number 
screener_dropout_number

#Dropout during pre-SSI outcome measures

b_submit <- eligible_responders %>%
  rename("b_submit" = "b_pre_thanks_tim_Page Submit") %>%
  filter(!is.na(b_submit))
b_submit_number <- count(b_submit) 
b_submit_number
b_dropout_number <- eligible_responders_number - b_submit_number
b_dropout_number

#Dropout during SSI

ssi_submit <- eligible_responders %>%
  rename("ssi_submit" = "bn_bp_bn_like_tim_Page Submit") %>%
  filter(!is.na(ssi_submit))
ssi_submit_number <- count(ssi_submit) 
ssi_submit_number 
ssi_dropout_number <- b_submit_number - ssi_submit_number
ssi_dropout_number

#Dropout during PFS

pfs_submit <- eligible_responders %>%
  rename("pfs_submit" = "pi_pfs_tim_Page Submit") %>%
  filter(!is.na(pfs_submit))
pfs_submit_number <- count(pfs_submit) 
pfs_submit_number 
pfs_dropout_number <- ssi_submit_number - pfs_submit_number 
pfs_dropout_number

#Dropout during post-SSI outcome measures

pi_submit <- eligible_responders %>%
  rename("pi_submit" = "pi_bsss_tim_Page Submit") %>%
  filter(!is.na(pi_submit))
pi_submit_number <- count(pi_submit) 
pi_submit_number 
pi_dropout_number <- pfs_submit_number - pi_submit_number 
pi_dropout_number
                 
##Program Feedback Scale

#Mean and sd of each person's sum score
mean_total_PFS <- mean(pfs_completers$pi_pfs_sum)
mean_total_PFS 
sd_total_PFS <- sd(pfs_completers$pi_pfs_sum)
sd_total_PFS 
mean_of_all_items_PFS <- mean_total_PFS / 7
mean_of_all_items_PFS
sd_of_all_items_PFS <- sd_total_PFS / 7
sd_of_all_items_PFS 

#N and percent of participants with sum score >3, reflect overall perceived SSI acceptability
pi_pfs_sum_scores <- pfs_completers$pi_pfs_sum / 7
pi_pfs_sum_scores_acceptable <- pi_pfs_sum_scores > 3
pi_pfs_sum_scores_acceptable_number <-sum(pi_pfs_sum_scores_acceptable)
pi_pfs_sum_scores_acceptable_number
pi_pfs_sum_scores_acceptable_percent <- (pi_pfs_sum_scores_acceptable_number * 100) / pfs_completers_number
pi_pfs_sum_scores_acceptable_percent

#Individual item scores
pfs_completers %>%
  select(pi_pfs_1:pi_pfs_7) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value))

#N and percent of participants with item score >3, reflect endorsement of the given item

#Item 1
pi_pfs_1_acceptable <- pfs_completers$pi_pfs_1 > 3
pi_pfs_1_acceptable_number <- sum(pi_pfs_1_acceptable)
pi_pfs_1_acceptable_number
pi_pfs_1_acceptable_percent <- (pi_pfs_1_acceptable_number * 100) / pfs_completers_number
pi_pfs_1_acceptable_percent
#Item 2
pi_pfs_2_acceptable <- pfs_completers$pi_pfs_2 > 3
pi_pfs_2_acceptable_number <- sum(pi_pfs_2_acceptable)
pi_pfs_2_acceptable_number
pi_pfs_2_acceptable_percent <- (pi_pfs_2_acceptable_number * 100) / pfs_completers_number
pi_pfs_2_acceptable_percent
#Iten 3
pi_pfs_3_acceptable <- pfs_completers$pi_pfs_3 > 3
pi_pfs_3_acceptable_number <- sum(pi_pfs_3_acceptable)
pi_pfs_3_acceptable_number
pi_pfs_3_acceptable_percent <- (pi_pfs_3_acceptable_number * 100) / pfs_completers_number
pi_pfs_3_acceptable_percent
#Item 4
pi_pfs_4_acceptable <- pfs_completers$pi_pfs_4 > 3
pi_pfs_4_acceptable_number <- sum(pi_pfs_4_acceptable)
pi_pfs_4_acceptable_number
pi_pfs_4_acceptable_percent <- (pi_pfs_4_acceptable_number * 100) / pfs_completers_number
pi_pfs_4_acceptable_percent
#Item 5
pi_pfs_5_acceptable <- pfs_completers$pi_pfs_5 > 3
pi_pfs_5_acceptable_number <- sum(pi_pfs_5_acceptable)
pi_pfs_5_acceptable_number
pi_pfs_5_acceptable_percent <- (pi_pfs_5_acceptable_number * 100) / pfs_completers_number
pi_pfs_5_acceptable_percent
#Item 6
pi_pfs_6_acceptable <- pfs_completers$pi_pfs_6 > 3
pi_pfs_6_acceptable_number <- sum(pi_pfs_6_acceptable)
pi_pfs_6_acceptable_number
pi_pfs_6_acceptable_percent <- (pi_pfs_6_acceptable_number * 100) / pfs_completers_number
pi_pfs_6_acceptable_percent
#Item 7
pi_pfs_7_acceptable <- pfs_completers$pi_pfs_7 > 3
pi_pfs_7_acceptable_number <- sum(pi_pfs_7_acceptable)
pi_pfs_7_acceptable_number
pi_pfs_7_acceptable_percent <- (pi_pfs_7_acceptable_number * 100) / pfs_completers_number
pi_pfs_7_acceptable_percent

#Open-ended positive feedback

pi_pfs_like_responses <- pfs_completers %>% 
  filter(!is.na(pi_pfs_like))
pi_pfs_like_responses_number <- count(pi_pfs_like_responses) 
pi_pfs_like_responses_number 
pi_pfs_like_responses_percent <- (pi_pfs_like_responses_number * 100) / pfs_completers_number
pi_pfs_like_responses_percent

#Open-ended constructive feedback

pi_pfs_change_responses <- pfs_completers %>% 
  filter(!is.na(pi_pfs_change))
pi_pfs_change_responses_number <- count(pi_pfs_change_responses) 
pi_pfs_change_responses_number 
#14 of these comments do not provide any constructive feedback. 
#R_a8BOmDzs0YwtTFv
#R_un1fr6cSF9AFAk1
#R_un1fr6cSF9AFAk1
#R_2Pz2xTN3dfozgU6
#R_1QDSvbXJ2VJ51oC
#R_b4qaP6e1vH0IOpH
#R_3kw9bRWDbI5ubyH
#R_21tXxxhuUgyo70D
#R_3MEAl9RL0EWI6zn
#R_1jZLhhTs2fqYmvL
#R_3dKzFAxsoTszXNF
#R_2qy2KmeBCzyLZG9
#R_24eXPxRMIXM8MKR
#R_3KYKTBQOBBfRIrC
pi_pfs_change_responses_number_revised <- pi_pfs_change_responses_number - 14
pi_pfs_change_responses_number_revised 
pi_pfs_change_responses_percent <- (pi_pfs_change_responses_number_revised * 100) / pfs_completers_number
pi_pfs_change_responses_percent

#Open-ended other feedback

pi_pfs_other_responses <- pfs_completers %>% 
  filter(!is.na(pi_pfs_other))
pi_pfs_other_responses_number <- count(pi_pfs_other_responses) 
pi_pfs_other_responses_number 
pi_pfs_other_responses_percent <- (pi_pfs_other_responses_number * 100) / pfs_completers_number
pi_pfs_other_responses_percent

#Open-ended feedback - all
pi_pfs_all_responses <- pfs_completers %>% 
  filter(!is.na(pi_pfs_like) | !is.na(pi_pfs_change) | !is.na(pi_pfs_other))
pi_pfs_all_responses_number <- count(pi_pfs_all_responses) 
pi_pfs_all_responses_number 
pi_pfs_all_percent <- (pi_pfs_all_responses_number * 100) / pfs_completers_number
pi_pfs_all_responses_percent

#Written feedback in its own dataframe

pfs_qual <- pfs_completers %>% 
  select(ResponseId, pi_pfs_like, pi_pfs_change, pi_pfs_other)
pfs_qual 
flextable::flextable(pfs_qual, cwidth = c(0.5,7,0.5))

## Hopelessness
plot(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum)
t.test(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum, paired = TRUE)

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

#Calculate the summary statistics we need to compute effect sizes, and save them in shs_agency_stats
shs_agency_stats <- b_pi_completers %>%
  select(b_shs_agency_sum, pi_shs_agency_sum) %>%
  mutate(shs_agency_sum_change = pi_shs_agency_sum - b_shs_agency_sum) %>%
  summarize(n = n(),
            across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = shs_agency_sum_change_mean / shs_agency_sum_change_sd,
         d_av = shs_agency_sum_change_mean / ((b_shs_agency_sum_sd + pi_shs_agency_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(shs_agency_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = shs_agency_stats$shs_agency_sum_change_mean,
             sddiff = shs_agency_stats$shs_agency_sum_change_sd,
             n = bhs_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = shs_agency_stats$b_shs_agency_sum_mean,
            m2 = shs_agency_stats$pi_shs_agency_sum_mean,
            sd1 = shs_agency_stats$b_shs_agency_sum_sd,
            sd2 = shs_agency_stats$pi_shs_agency_sum_sd,
            n = shs_agency_stats$n,
            a = .05) %>%
  pluck("estimate")

## Functionality Appreciation Scale
plot(b_pi_completers$b_fas_sum, b_pi_completers$pi_fas_sum)
t.test(b_pi_completers$b_fas_sum, b_pi_completers$pi_fas_sum, paired = TRUE)

#Calculate the summary statistics we need to compute effect sizes, and save them in fas_stats
fas_stats <- b_pi_completers %>%
  select(b_fas_sum, pi_fas_sum) %>%
  mutate(fas_sum_change = pi_fas_sum - b_fas_sum) %>%
  summarize(n = n(),
            across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = fas_sum_change_mean / fas_sum_change_sd,
         d_av = fas_sum_change_mean / ((b_fas_sum_sd + pi_fas_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(fas_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = fas_stats$fas_sum_change_mean,
             sddiff = fas_stats$fas_sum_change_sd,
             n = fas_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = fas_stats$b_fas_sum_mean,
            m2 = fas_stats$pi_fas_sum_mean,
            sd1 = fas_stats$b_fas_sum_sd,
            sd2 = fas_stats$pi_fas_sum_sd,
            n = fas_stats$n,
            a = .05) %>%
  pluck("estimate")

## Body Dissatisfaction 
plot(b_pi_completers$b_bsss_sum, b_pi_completers$pi_bsss_sum)
t.test(b_pi_completers$b_bsss_sum, b_pi_completers$pi_bsss_sum, paired = TRUE)

#Calculate the summary statistics we need to compute effect sizes, and save them in bsss_stats
bsss_stats <- b_pi_completers %>%
  select(b_bsss_sum, pi_bsss_sum) %>%
  mutate(bsss_sum_change = pi_bsss_sum - b_bsss_sum) %>%
  summarize(n = n(),
            across(everything(), list(mean = mean, sd = sd))) %>%
  mutate(d_z = bsss_sum_change_mean / bsss_sum_change_sd,
         d_av = bsss_sum_change_mean / ((b_bsss_sum_sd + pi_bsss_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(bsss_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = bsss_stats$bsss_sum_change_mean,
             sddiff = bsss_stats$bsss_sum_change_sd,
             n = bsss_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = bsss_stats$b_bsss_sum_mean,
            m2 = bsss_stats$pi_bsss_sum_mean,
            sd1 = bsss_stats$b_bsss_sum_sd,
            sd2 = bsss_stats$pi_bsss_sum_sd,
            n = bsss_stats$n,
            a = .05) %>%
  pluck("estimate")

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

b_pi_completers %>%
  select(starts_with("b_dem_gender")) %>%
  select(-"b_dem_gender_16_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "gender", values_to = "n") %>% 
  mutate(percent_of_respondents = (n / 75)*100)

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

b_pi_completers %>%
  select(starts_with("b_dem_race_ethnicity")) %>%
  select(-"b_dem_race_ethnicity_7_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "race/ethnicity", values_to = "n") %>% 
  mutate(percent_of_respondents = (n / 75)*100)

#Disability
b_pi_completers %>%
  count(b_dem_disability_1) %>%
  mutate(percent = (n / sum(n))*100)

#Age
b_pi_completers %>%
  count(b_dem_age) %>%
  mutate(percent = (n / sum(n))*100)




#### New Plots
## Pre-Post Changes
b_pi_completers %>%
  select(b_bhs_sum, pi_bhs_sum,
         b_fas_sum, pi_fas_sum,
         b_bsss_sum, pi_bsss_sum) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            ci = (sd / sqrt(n())) * 1.96) %>%
  mutate(timepoint = str_extract(name, "^[a-z]*"),
         measure = str_extract(name, "(?<=_)[a-z]*(?=_)") %>%
           toupper()) %>%
  ggplot() +
    geom_point(aes(x = timepoint, y = mean, group = measure)) +
    geom_line(aes(x = timepoint, y = mean, group = measure)) +
    geom_errorbar(aes(x = timepoint, ymin = mean - ci, ymax = mean + ci)) +
    scale_y_continuous(name = "Sum Score", limits = c(0, NA)) +
    scale_x_discrete(name = "Timepoint", labels = c("Baseline", "Post-Intervention")) +
    facet_wrap(~ measure, scales = "free_y") +
    theme_classic()


## Stacked Bar of PFS
library(forcats)

pfs_completers %>%
  select(pi_pfs_1:pi_pfs_7) %>%
  pivot_longer(everything()) %>%
  mutate(value = value + 1) %>% #DELETE THIS
  mutate(
    item_label = case_when(
      name == "pi_pfs_1" ~ "Label 1",
      name == "pi_pfs_2" ~ "Label 2",
      T ~ name),
    response_label = factor(
      x = value,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Really Disagree", "Disagree", "Neutral", "Agree", "Really Agree")
    )) %>%
  count(item_label, response_label) %>%
  ggplot() +
    geom_col(aes(x = item_label, y = n, fill = fct_rev(response_label))) +
    scale_x_discrete(name = "Program Feedback Scale Item") +
    scale_fill_discrete(name = "Response") +
    scale_y_continuous(name = "Frequency") +
    theme_classic() +
    ggtitle("PFS Responses")
