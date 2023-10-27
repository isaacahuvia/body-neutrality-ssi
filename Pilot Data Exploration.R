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
if(!require(psych)){install.packages('psych')}
library(psych)
if(!require(psych)){install.packages('forcats')}
library('forcats')
if(!require(effsize)){install.packages('effsize')}
library('effsize')

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
  saveRDS(qualtrics_data, "/Volumes/fsmresfiles/MSS/Schleider_Lab/jslab/Project Body Neutrality/Pilot Study/Data/Qualtrics Pilot Data.rds")
  
} else {
  
  # Read back-up
  qualtrics_data <- readRDS("/Volumes/fsmresfiles/MSS/Schleider_Lab/jslab/Project Body Neutrality/Pilot Study/Data/Qualtrics Pilot Data.rds")
  
}

####  Data Exploration  ####
View(qualtrics_data)
str(qualtrics_data)

####  Data Cleaning  ####

## Create summary variables
all_responders <- qualtrics_data %>%
  #Variables that are summed 
  mutate(b_bhs_sum = b_bhs_4_1 + b_bhs_4_2 + b_bhs_4_3 + b_bhs_4_4,
         b_shs_agency_sum = b_shs_agency_1 + b_shs_agency_2 + b_shs_agency_3,
         b_fas_sum = b_fas_1 + b_fas_2 + b_fas_3 + b_fas_4 + b_fas_5 + b_fas_6 + b_fas_7,
         b_bsss_sum = (6 - b_bsss_1) + (6 - b_bsss_2) + (6 - b_bsss_3) + (6 - b_bsss_4) + (6 - b_bsss_5) + 
           (6 - b_bsss_6) + (6 - b_bsss_7) + (6 - b_bsss_8) + (6 - b_bsss_9) + (6 - b_bsss_10), #bsss is reversed scored
         pi_pfs_1 = pi_pfs_1 + 1, #scale for pfs need to be adjusted by +1
         pi_pfs_2 = pi_pfs_2 + 1,
         pi_pfs_3 = pi_pfs_3 + 1,
         pi_pfs_4 = pi_pfs_4 + 1,
         pi_pfs_5 = pi_pfs_5 + 1,
         pi_pfs_6 = pi_pfs_6 + 1,
         pi_pfs_7 = pi_pfs_7 + 1,
         pi_pfs_sum = pi_pfs_1 + pi_pfs_2 + pi_pfs_3 + pi_pfs_4 + pi_pfs_5 + pi_pfs_6 + pi_pfs_7,
         pi_bhs_sum = pi_bhs_4_1 + pi_bhs_4_2 + pi_bhs_4_3 + pi_bhs_4_4,
         pi_shs_agency_sum = pi_shs_agency_1 + pi_shs_agency_2 + pi_shs_agency_3,
         pi_fas_sum = pi_fas_1 + pi_fas_2 + pi_fas_3 + pi_fas_4 + pi_fas_5 + pi_fas_6 + pi_fas_7,
         pi_bsss_sum = (6 - pi_bsss_1) + (6 - pi_bsss_2) + (6 - pi_bsss_3) + (6 - pi_bsss_4) + (6 - pi_bsss_5) + 
           (6 - pi_bsss_6) + (6 - pi_bsss_7) + (6 - pi_bsss_8) + (6 - pi_bsss_9) + (6 - pi_bsss_10))

## Patters of Use ##

#Identify eligible responders
eligible_responders <- all_responders %>%
  filter(is.na(Eligibility)) %>%
  filter(!is.na(b_dem_age)) #THIS LINE IS NEW 2/14/23, indicates that the screen was completed.
all_responders_number <- count(all_responders)
eligible_responders_number <- count(eligible_responders)
#n = 353 responders total
#n = 156 eligible responders, regardless of completion
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
pfs_completers_number
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

##Pre-post correlations among pre-post completers - NEW 4.13.23
bhs_cor <- cor(b_pi_completers$b_bhs_sum, b_pi_completers$pi_bhs_sum)
fas_cor <- cor(b_pi_completers$b_fas_sum, b_pi_completers$pi_fas_sum)
bsss_cor <- cor(b_pi_completers$b_bsss_sum, b_pi_completers$pi_bsss_sum)

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

#Dropout during screener

screener_submit <- qualtrics_data %>%
  rename("screener_submit" = "b_screener_tim_Page Submit") %>%
  filter(!is.na(screener_submit))
screener_submit_number <- count(screener_submit) 
screener_submit_number
screener_dropout_number <- all_responders_number - screener_submit_number 
screener_dropout_number
screener_dropout_percent <- (screener_dropout_number * 100) / all_responders_number
screener_dropout_percent

#Dropout during pre-SSI outcome measures

b_submit <- eligible_responders %>%
  rename("b_submit" = "b_pre_thanks_tim_Page Submit") %>%
  filter(!is.na(b_submit))
b_submit_number <- count(b_submit) 
b_submit_number
b_dropout_number <- eligible_responders_number - b_submit_number
b_dropout_number
b_dropout_percent <- (b_dropout_number * 100) / eligible_responders_number
b_dropout_percent 

#Dropout during SSI

ssi_submit <- eligible_responders %>%
  rename("ssi_submit" = "bn_bp_bn_like_tim_Page Submit") %>%
  filter(!is.na(ssi_submit))
ssi_submit_number <- count(ssi_submit) 
ssi_submit_number 
ssi_dropout_number <- b_submit_number - ssi_submit_number
ssi_dropout_number
ssi_dropout_percent <- (ssi_dropout_number * 100) / b_submit_number
ssi_dropout_percent

#Dropout during PFS

pfs_submit <- eligible_responders %>%
  rename("pfs_submit" = "pi_pfs_tim_Page Submit") %>% 
  filter(!is.na(pfs_submit))
pfs_submit_number <- count(pfs_submit) 
pfs_submit_number   #note the difference between pfs_subbmit_number (83) and pfs_completers_number (81 - requires no N/A values)
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

#Dropout before completing all the post-SSI questions
pfs_pi_dropout_number <- pfs_dropout_number + pi_dropout_number
pfs_pi_dropout_number
pfs_pi_dropout_percent <- (pfs_pi_dropout_number * 100) / ssi_submit_number
pfs_pi_dropout_percent

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

#Internal consistnecy
pfs_completers %>%
  select(pi_pfs_1, pi_pfs_2, pi_pfs_3, pi_pfs_4, pi_pfs_5, pi_pfs_6, pi_pfs_7) %>%
  alpha()

#N and percent of participants with sum score >3, reflect overall perceived SSI acceptability
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
#Item 3
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
pi_pfs_6_acceptable_percent <- (pi_pfs_6_acceptable_number * 100) / pfs_completers_number
pi_pfs_6_acceptable_percent
#Item 7
pi_pfs_7_acceptable <- pfs_completers$pi_pfs_7 > 3
pi_pfs_7_acceptable_number <- sum(pi_pfs_7_acceptable)
pi_pfs_7_acceptable_number
pi_pfs_7_acceptable_percent <- (pi_pfs_7_acceptable_number * 100) / pfs_completers_number
pi_pfs_7_acceptable_percent

#Written feedback in its own dataframe

pfs_qual <- pfs_completers %>% 
  select(ResponseId, pi_pfs_like, pi_pfs_change, pi_pfs_other)
pfs_qual 
flextable::flextable(pfs_qual, cwidth = c(0.5,7,0.5))

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

#These 14 of these comments do not provide any constructive feedback. 
pi_pfs_change_responses_drop <- pfs_completers %>% 
  select(ResponseId, pi_pfs_change) %>% 
  filter(ResponseId == "R_a8BOmDzs0YwtTFv" | ResponseId == "R_un1fr6cSF9AFAk1"
         | ResponseId == "R_3PBvlSaFGAksUqY" |  ResponseId == "R_2Pz2xTN3dfozgU6"
         |  ResponseId == "R_1QDSvbXJ2VJ51oC" | ResponseId == "R_b4qaP6e1vH0IOpH"
         |  ResponseId == "R_3kw9bRWDbI5ubyH" | ResponseId == "R_21tXxxhuUgyo70D"
         |  ResponseId == "R_3MEAl9RL0EWI6zn" |  ResponseId == "R_1jZLhhTs2fqYmvL"
         |  ResponseId == "R_3dKzFAxsoTszXNF" |  ResponseId == "R_2qy2KmeBCzyLZG9"
         |  ResponseId == "R_24eXPxRMIXM8MKR" | ResponseId == "R_3KYKTBQOBBfRIrC")
pi_pfs_change_responses_drop

pi_pfs_change_responses_number_revised <- pi_pfs_change_responses_number - 14 #subtract the 14 responses identified above
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
pi_pfs_all_responses_percent <- (pi_pfs_all_responses_number * 100) / pfs_completers_number
pi_pfs_all_responses_percent

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
         d_rm = ((b_bhs_sum_mean - pi_bhs_sum_mean) / sqrt((b_bhs_sum_sd^2 + pi_bhs_sum_sd^2 ) - (2 * bhs_cor * b_bhs_sum_sd * pi_bhs_sum_sd))) * sqrt(2 * (1-bhs_cor)),
         d_av = bhs_sum_change_mean / ((b_bhs_sum_sd + pi_bhs_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(bhs_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = bhs_stats$bhs_sum_change_mean,
             sddiff = bhs_stats$bhs_sum_change_sd,
             n = bhs_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(rm) - NEW 4.13.23
d.dep.t.rm(m1 = bhs_stats$b_bhs_sum_mean,
           m2 = bhs_stats$pi_bhs_sum_mean,
           sd1 = bhs_stats$b_bhs_sum_sd,
           sd2 = bhs_stats$pi_bhs_sum_sd,
           r = bhs_cor,
           n = bhs_stats$n,
           a = 0.05)%>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = bhs_stats$b_bhs_sum_mean,
            m2 = bhs_stats$pi_bhs_sum_mean,
            sd1 = bhs_stats$b_bhs_sum_sd,
            sd2 = bhs_stats$pi_bhs_sum_sd,
            n = bhs_stats$n,
            a = .05) %>%
  pluck("estimate")

#Calculate internal consistency

b_pi_completers %>%
  select(b_bhs_4_1, b_bhs_4_2, b_bhs_4_3, b_bhs_4_4) %>%
  alpha()

b_pi_completers %>%
  select(pi_bhs_4_1, pi_bhs_4_2, pi_bhs_4_3, pi_bhs_4_4) %>%
  alpha()

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

#Calculate internal consistency

b_pi_completers %>%
  select(b_shs_agency_1, b_shs_agency_2, b_shs_agency_3) %>%
  alpha()

b_pi_completers %>%
  select(pi_shs_agency_1, pi_shs_agency_2, pi_shs_agency_3) %>%
  alpha()

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
         d_rm = ((b_fas_sum_mean - pi_fas_sum_mean) / sqrt((b_fas_sum_sd^2 + pi_fas_sum_sd^2 ) - (2 * fas_cor * b_fas_sum_sd * pi_fas_sum_sd))) * sqrt(2 * (1-fas_cor)),
         d_av = fas_sum_change_mean / ((b_fas_sum_sd + pi_fas_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(fas_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = fas_stats$fas_sum_change_mean,
             sddiff = fas_stats$fas_sum_change_sd,
             n = fas_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(rm) - NEW 4.13.23
d.dep.t.rm(m1 = fas_stats$b_fas_sum_mean,
           m2 = fas_stats$pi_fas_sum_mean,
           sd1 = fas_stats$b_fas_sum_sd,
           sd2 = fas_stats$pi_fas_sum_sd,
           r = fas_cor,
           n = fas_stats$n,
           a = 0.05)%>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = fas_stats$b_fas_sum_mean,
            m2 = fas_stats$pi_fas_sum_mean,
            sd1 = fas_stats$b_fas_sum_sd,
            sd2 = fas_stats$pi_fas_sum_sd,
            n = fas_stats$n,
            a = .05) %>%
  pluck("estimate")

#Calculate internal consistency

b_pi_completers %>%
  select(b_fas_1, b_fas_2, b_fas_3, b_fas_4, b_fas_5, b_fas_6, b_fas_7) %>%
  alpha()

b_pi_completers %>%
  select(pi_fas_1, pi_fas_2, pi_fas_3, pi_fas_4, pi_fas_5, pi_fas_6, pi_fas_7) %>%
  alpha()

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
         d_rm = ((b_bsss_sum_mean - pi_bsss_sum_mean) / sqrt((b_bsss_sum_sd^2 + pi_bsss_sum_sd^2 ) - (2 * bsss_cor * b_bsss_sum_sd * pi_bsss_sum_sd))) * sqrt(2 * (1-bsss_cor)),
         d_av = bsss_sum_change_mean / ((b_bsss_sum_sd + pi_bsss_sum_sd) / 2))

#Take a look at the summary statistics, as well as the manual effect sizes
print(bsss_stats)

#Calculate d(z)
d.dep.t.diff(mdiff = bsss_stats$bsss_sum_change_mean,
             sddiff = bsss_stats$bsss_sum_change_sd,
             n = bsss_stats$n,
             a = .05) %>%
  pluck("estimate")

#Calculate d(rm) - NEW 4.13.23
d.dep.t.rm(m1 = bsss_stats$b_bsss_sum_mean,
           m2 = bsss_stats$pi_bsss_sum_mean,
           sd1 = bsss_stats$b_bsss_sum_sd,
           sd2 = bsss_stats$pi_bsss_sum_sd,
           r = bsss_cor,
           n = bsss_stats$n,
           a = 0.05)%>%
  pluck("estimate")

#Calculate d(av)
d.dep.t.avg(m1 = bsss_stats$b_bsss_sum_mean,
            m2 = bsss_stats$pi_bsss_sum_mean,
            sd1 = bsss_stats$b_bsss_sum_sd,
            sd2 = bsss_stats$pi_bsss_sum_sd,
            n = bsss_stats$n,
            a = .05) %>%
  pluck("estimate")

#Calculate internal consistency

b_pi_completers %>%
  select(b_bsss_1, b_bsss_2, b_bsss_3, b_bsss_4, b_bsss_5, b_bsss_6, b_bsss_7,b_bsss_8, b_bsss_9, b_bsss_10) %>%
  alpha()

b_pi_completers %>%
  select(pi_bsss_1, pi_bsss_2, pi_bsss_3, pi_bsss_4, pi_bsss_5, pi_bsss_6, pi_bsss_7, pi_bsss_8, pi_bsss_9, pi_bsss_10
  ) %>%
  alpha()

## Demographics

# Sex assigned at birth
b_pi_completers %>%
  count(b_dem_sex) %>%
  mutate(percent = (n / sum(n))*100)

AMAB <- data.frame(b_pi_completers$ResponseId, b_pi_completers$b_dem_sex)
#Confirmed from looking at full response from 1 AMAB participant (R_3freTqgdpmwEz1x) that there are no cisgender men

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

pfs_completers %>% #for ABCT
  select(starts_with("b_dem_gender")) %>%
  select(-"b_dem_gender_16_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarize(across(everything(), mean)) 

pfs_completers %>%
  select(starts_with("b_dem_gender")) %>%
  select(-"b_dem_gender_16_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "gender", values_to = "n") %>% 
  mutate(percent_of_respondents = (n / 81)*100)


#Sexual orientation
b_pi_completers %>%
  count(b_dem_orientation) %>%
  mutate(percent = (n / sum(n))*100)

pfs_completers %>% #for ABCT
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

pfs_completers %>% #for ABCT abstract
  select(starts_with("b_dem_race_ethnicity")) %>%
  select(-"b_dem_race_ethnicity_7_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarize(across(everything(), mean))

pfs_completers %>%
  select(starts_with("b_dem_race_ethnicity")) %>%
  select(-"b_dem_race_ethnicity_7_TEXT") %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "race/ethnicity", values_to = "n") %>% 
  mutate(percent_of_respondents = (n / 81)*100)

#Disability
b_pi_completers %>%
  count(b_dem_disability_1) %>%
  mutate(percent = (n / sum(n))*100)

#Age
b_pi_completers %>%
  count(b_dem_age) %>%
  mutate(percent = (n / sum(n))*100)

#### Within - SSI ####

###Barriers to body positivity 

##Frequency and percent of total for each item selected
pfs_completers %>%
  select(starts_with("bn_bp_barriers")) %>%
  select(-"bn_bp_barriers_1_11_TEXT") %>% #Free response, accounted for with bn_bp_barriers_1_11
  select(-"bn_bp_barriers_2") %>% #Providing definitions of terms
  select(-"bn_bp_barriers_1_12") %>%  #"No, does not apply to me"
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "barriers", values_to = "n") %>% 
  mutate(percent = (n / 81)*100) #81 = number of PFS completers

##Responses for Other (please specify)
print(pfs_completers$bn_bp_barriers_1_11_TEXT)
#"BMI Scale" and "eating disorders?" are the only non-NA responses

###Awareness of body positivty and body neutrality

##Frequency yes/no and percent of total for body positivity
pfs_completers %>%
  count(bn_bp_awareness) %>%
  mutate(percent = (n / sum(n))*100)

##Frequency yes/no and percent of total for body neutrality
pfs_completers %>%
  count(bn_bn_awareness) %>%
  mutate(percent = (n / sum(n))*100)

#McNemar
bn_bp_awareness <- factor(pfs_completers$bn_bp_awareness)
levels(bn_bp_awareness) <- c("No", "Yes")
bn_bn_awareness <- factor(pfs_completers$bn_bn_awareness)
levels(bn_bn_awareness) <- c("No", "Yes")

pfs_completers_awareness <- table(bn_bp_awareness, bn_bn_awareness)
pfs_completers_awareness 

mcnemar.test(pfs_completers_awareness)

###Definitions of body positivity and body neutrality

##n and percent of sample for body positivity open-ended responses
bp_definitions <- pfs_completers %>% 
  filter(!is.na(bn_bp_definition))
bp_definitions_number <- count(bp_definitions) 
bp_definitions_number
bp_definitions_percent <- (bp_definitions_number * 100) / pfs_completers_number
bp_definitions_percent

##n and percent of sample for body neutrality open-ended responses
bn_definitions <- pfs_completers %>% 
  filter(!is.na(bn_bn_definition))
bn_definitions_number <- count(bn_definitions) 
bn_definitions_number
bn_definitions_percent <- (bn_definitions_number * 100) / pfs_completers_number
bn_definitions_percent

##The open-ended responses for definition
within_ssi_definition <- pfs_completers %>% 
  select(ResponseId, bn_bp_definition, bn_bn_definition)
flextable::flextable(within_ssi_definition, cwidth = 3)

###Endorsement of body positivty and body neutrality 

##Means and standard deviations for body positivity
pfs_completers %>% 
  summarize(mean = mean(bn_bp_like),
            sd = sd(bn_bp_like))

#Histogram for body positivity
ggplot(data=pfs_completers, aes(x=bn_bp_like)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#F37068") +  
  geom_vline(aes(xintercept=mean(bn_bp_like)),
             color="#0F606B", linetype="dashed", size=1) +
  theme_classic() +
  ggtitle("Body Positivity Ratings") +
  xlab("Endorsement Rating") +
  theme(plot.title=element_text(hjust=0.5)) + 
  aes(y = after_stat(count)/sum(after_stat(count))) + 
  scale_y_continuous(labels = scales::percent, name = "Percent of Sample")

##Means, standard deviations for body neutrality
pfs_completers %>% 
  summarize(mean = mean(bn_bn_like),
            sd = sd(bn_bn_like))

#Histogram for body neutrality
ggplot(data=pfs_completers, aes(x=bn_bn_like)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#FFC24A") +  
  geom_vline(aes(xintercept=mean(bn_bn_like)),
             color="#0F606B", linetype="dashed", size=1) +
  theme_classic() +
  ggtitle("Body Neutrality Ratings") +
  xlab("Endorsement Rating") +
  theme(plot.title=element_text(hjust=0.5)) + 
  aes(y = after_stat(count)/sum(after_stat(count))) + 
  scale_y_continuous(labels = scales::percent, name = "Percent of Sample")

#t test
t.test(pfs_completers$bn_bn_like, pfs_completers$bn_bp_like, paired = TRUE)
cohen.d(pfs_completers$bn_bn_like, pfs_completers$bn_bp_like, paired = TRUE)

##n and percent of sample for open-ended responses for body positivity
bp_endorsement <- pfs_completers %>% 
  filter(!is.na(bn_bp_like_why))
bp_endorsement_number <- count(bp_endorsement) 
bp_endorsement_number
bp_endorsement_percent <- (bp_endorsement_number * 100) / pfs_completers_number
bp_endorsement_percent

##n and percent of sample for open-ended responses for body neutrality
bn_endorsement <- pfs_completers %>% 
  filter(!is.na(bn_bn_like_why))
bn_endorsement_number <- count(bn_endorsement) 
bn_endorsement_number
bn_endorsement_percent <- (bn_endorsement_number * 100) / pfs_completers_number
bn_endorsement_percent

##The open-ended responses for endorsement
within_ssi_endorsement <- pfs_completers %>% 
  select(ResponseId, bn_bp_like, bn_bp_like_why, bn_bn_like, bn_bn_like_why)
flextable::flextable(within_ssi_endorsement, cwidth = 3)
#For bn_bp_like and bn_bn_like, 1 = not at all, 5 = a lot

####  Plots  ####

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
           toupper(),
         measure = factor(measure, levels = c("BHS", "FAS", "BSSS"))) %>%
  ggplot() +
  geom_point(aes(x = timepoint, y = mean, group = measure)) +
  geom_line(aes(x = timepoint, y = mean, group = measure)) +
  geom_errorbar(aes(x = timepoint, ymin = mean - ci, ymax = mean + ci)) +
  scale_y_continuous(name = "Sum Score", limits = c(0, NA)) +
  scale_x_discrete(name = "Timepoint", labels = c("Baseline", "Post-Intervention")) +
  facet_wrap(~ measure, scales = "free_y",
             labeller = as_labeller(c("BHS" = "Hopelessness",
                                      "FAS" = "Functionality Appreciation",
                                      "BSSS" = "Body Dissatisfaction"))) +
  ggtitle("Pre- to Post-Intervention Changes in Proximal Outcomes",
          "Plots of means and 95% confidence intervals") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

## Stacked Bar of PFS

#Frequency on the y axis
pfs_completers %>%
  select(pi_pfs_1:pi_pfs_7) %>%
  pivot_longer(everything()) %>%
  mutate(
    item_label = case_when(
      name == "pi_pfs_1" ~ "Enjoyed",
      name == "pi_pfs_2" ~ "Understood",
      name == "pi_pfs_3" ~ "Easy to Use",
      name == "pi_pfs_4" ~ "Tried My Hardest",
      name == "pi_pfs_5" ~ "Help Other Kids",
      name == "pi_pfs_6" ~ "Recommend to Friend",
      name == "pi_pfs_7" ~ "Agree with Message",
      T ~ name),
    response_label = factor(
      x = value,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Really Disagree", "Disagree", "Neutral", "Agree", "Really Agree")
    )) %>%
  count(item_label, response_label) %>%
  ggplot() +
  geom_col(aes(x = fct_relevel(item_label,
                               "Enjoyed",
                               "Understood",
                               "Easy to Use",
                               "Tried My Hardest",
                               "Help Other Kids",
                               "Recommend to Friend",
                               "Agree with Message"), y = n, fill = fct_rev(response_label))) +
  scale_x_discrete(name = "Scale Item") +
  scale_fill_brewer(name = "Response", palette = "RdYlBu", direction=-1) +
  scale_y_continuous(name = "Frequency") +
  theme_classic() +
  ggtitle("Program Feedback Scale Responses") +
  theme(plot.title=element_text(hjust=0.5))

#Percent on the y axis
pfs_completers %>%
  select(pi_pfs_1:pi_pfs_7) %>%
  pivot_longer(everything()) %>%
  mutate(
    item_label = case_when(
      name == "pi_pfs_1" ~ "Enjoyed",
      name == "pi_pfs_2" ~ "Understood",
      name == "pi_pfs_3" ~ "Easy to Use",
      name == "pi_pfs_4" ~ "Tried My Hardest",
      name == "pi_pfs_5" ~ "Help Other Kids",
      name == "pi_pfs_6" ~ "Recommend to Friend",
      name == "pi_pfs_7" ~ "Agree with Message",
      T ~ name),
    response_label = factor(
      x = value,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Really Disagree", "Disagree", "Neutral", "Agree", "Really Agree")
    )) %>%
  count(item_label, response_label) %>%
  group_by(item_label) %>%
  mutate(percent = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = fct_relevel(item_label,
                               "Enjoyed",
                               "Understood",
                               "Easy to Use",
                               "Tried My Hardest",
                               "Help Other Kids",
                               "Recommend to Friend",
                               "Agree with Message"), y = percent, fill = fct_rev(response_label))) +
  scale_x_discrete(name = "Scale Item") +
  scale_fill_brewer(name = "Response", palette = "RdYlBu", direction=-1) +
  scale_y_continuous(labels = scales::percent, name = "Percent") +
  theme_classic() +
  ggtitle("Program Feedback Scale Responses") +
  theme(plot.title=element_text(hjust=0.5))

### New analyses requested from peer review

##WCS breakdown per the 156 eligible responders

count(eligible_responders)

#N respondents who scored ≥47

eligible_responders_wcs_47 <- eligible_responders %>% 
  filter(WCS >= 47)
count(eligible_responders_wcs_47) #150

#N respondents who endorsed weight being more important than most things in life or the most important thing in life
eligible_responders_weight_importance <- eligible_responders %>% 
  filter(b_wcs_4 == 66.66 | b_wcs_4 == 100.00) %>% 
  filter(WCS < 47)
count(eligible_responders_weight_importance) #2

#N respondents who endorsed being very afraid of or terrified of gaining three pounds
eligible_responders_fear_gaining <- eligible_responders %>% 
  filter(b_wcs_2 >= 75 | b_wcs_2 == 100)  %>% 
  filter(WCS < 47) 
count(eligible_responders_fear_gaining) #4