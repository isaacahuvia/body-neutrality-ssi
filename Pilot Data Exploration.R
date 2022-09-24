##############################
##  Project Body Neutrality ##
##  Pilot Data Exploration  ##
##############################

####  Startup  ####
## Load packages
library(easypackages)
libraries("tidyverse", "qualtRics")


## Download Pilot Data
# To pull a fresh dataset from Qualtrics, set this to TRUE
# In order to do that, you have to register your Qualtrics credentials with the
# qualtRics package: https://github.com/ropensci/qualtRics#register-your-qualtrics-credentials
pull_fresh_dataset <- T

if(pull_fresh_dataset) {
 
  # Pull data
  qualtrics_data <- fetch_survey("SV_6tDDvqz44aZxhCC", 
                                 force_request = T,
                                 label = F,
                                 convert = F)
  
  # Save back-up
  saveRDS(qualtrics_data, "S:\\Body Neutrality SSI\\Data\\Qualtrics Pilot Data.rds")
  
} else {
  
  # Read back-up
  readRDS("S:\\Body Neutrality SSI\\Data\\Qualtrics Pilot Data.rds")
  
}



####  Data Exploration  ####
View(qualtrics_data)
str(qualtrics_data)


## Demographics
# Sex
count(qualtrics_data, b_dem_sex)

# Gender - this is already dummy-coded
count(qualtrics_data, b_dem_gender_1)
count(qualtrics_data, b_dem_gender_2)
# etc.


## Outcomes
count(qualtrics_data, b_fas_1)



####  Data Cleaning  ####
## Create summary variables
clean_data <- qualtrics_data %>%
  mutate(b_fas_sum = b_fas_1 + b_fas_2 + b_fas_3 + b_fas_4 + b_fas_5 + b_fas_6 + b_fas_7,
         pi_fas_sum = pi_fas_1 + pi_fas_2 + pi_fas_3 + pi_fas_4 + pi_fas_5 + pi_fas_6 + pi_fas_7)



####  Analysis  ####
## Functional Appreciation Scale
plot(clean_data$b_fas_sum, clean_data$pi_fas_sum)
t.test(clean_data$b_fas_sum, clean_data$pi_fas_sum)
