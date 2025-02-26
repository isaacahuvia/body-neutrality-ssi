
df_randomized_imputed_proximal_outcomes_long <- df_randomized_imputed %>% 
  dplyr::select(Participant_ID, treatment_group, 
                b_bhs_sum, pi_bhs_sum, f_bhs_sum, 
                b_shs_agency_sum, pi_shs_agency_sum, f_shs_agency_sum, 
                b_fas_sum, pi_fas_sum, f_fas_sum, 
                b_biss_sum, pi_biss_sum, f_biss_sum) %>%
  pivot_longer(cols = -c(Participant_ID, treatment_group),  
               names_to = c("time", "variable"),
               names_pattern = "^(b|pi|f)_(.*)$",  
               values_to = "value") %>%
  mutate(time = factor(time, levels = c("b", "pi", "f"), 
                       labels = c("Baseline", "Post-Intervention", "Follow-Up"))) %>%
  mutate(time = as.numeric(time) - 1)  # Converts to 0 (Baseline), 1 (Post), 2 (Follow-Up)

#hopelessness predicted by time*treatment_group interaction with random intercept for participant (each participant starts at a different baseline value)
model1_bhs <- lmer(value ~ time * treatment_group + (1 | Participant_ID), 
                   data = df_randomized_imputed_proximal_outcomes_long %>% 
                     filter(variable == "bhs_sum"))
summary(model1_bhs)
#it seems like the difference in changes in hopelessness between treatment groups is significant at post-intervention, but loses significance at follow-up

#hopelessness predicted by time*treatment_group interaction with random intercept for participant and random slope for time (if we expect participants to vary in how they change over time)
model2_bhs <- lmer(value ~ time * treatment_group + (1 + time | Participant_ID), 
                   data = df_randomized_imputed_proximal_outcomes_long %>% 
                     filter(variable == "bhs_sum"))
summary(model2_bhs)

#model 1 for perceived agency
model1_shs_agency <- lmer(value ~ time * treatment_group + (1 | Participant_ID), 
                          data = df_randomized_imputed_proximal_outcomes_long %>% 
                            filter(variable == "shs_agency_sum"))
summary(model1_shs_agency)

#model 2 for perceived agency
model2_shs_agency <- lmer(value ~ time * treatment_group + (1 + time | Participant_ID), 
                          data = df_randomized_imputed_proximal_outcomes_long %>% 
                            filter(variable == "shs_agency_sum"))
summary(model2_shs_agency)

#model 1 for functionality appreciation
model1_fas <- lmer(value ~ time * treatment_group + (1 | Participant_ID), 
                   data = df_randomized_imputed_proximal_outcomes_long %>% 
                     filter(variable == "fas_sum"))
summary(model1_fas)

#model 2 for functionality appreciation
model2_fas <- lmer(value ~ time * treatment_group + (1 + time | Participant_ID), 
                   data = df_randomized_imputed_proximal_outcomes_long %>% 
                     filter(variable == "fas_sum"))
summary(model2_fas)

#model 1 for state body image
model1_biss <- lmer(value ~ time * treatment_group + (1 | Participant_ID), 
                    data = df_randomized_imputed_proximal_outcomes_long %>% 
                      filter(variable == "biss_sum"))
summary(model1_biss)

#model 2 for state body image
model2_biss <- lmer(value ~ time * treatment_group + (1 + time | Participant_ID), 
                    data = df_randomized_imputed_proximal_outcomes_long %>% 
                      filter(variable == "biss_sum"))
summary(model2_biss)

#plotting for hopelessness
ggplot(df_randomized_imputed_proximal_outcomes_long %>% 
         filter(variable == "bhs_sum"), 
       aes(x = time, y = value, color = treatment_group, group = Participant_ID)) +
  geom_line(alpha = 0.1) +  #Individual trajectories
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5) +  #Points for individual trajectories
  stat_summary(aes(group = treatment_group), fun = mean, geom = "line", 
               linewidth = 1.5, position = position_dodge(width = 0.2)) +  #Mean trend lines
  stat_summary(aes(group = treatment_group), fun = mean, geom = "point", 
               size = 3.5, position = position_dodge(width = 0.2)) +  #Mean points
  labs(
    title = "Hopelessness Scores Over Time",
    x = "Timepoint",
    y = "Hopelessness Score",
    color = "Treatment Group"
  ) +
  theme_minimal()

#plotting for perceived agency
ggplot(df_randomized_imputed_proximal_outcomes_long %>% 
         filter(variable == "shs_agency_sum"), 
       aes(x = time, y = value, color = treatment_group, group = Participant_ID)) +
  geom_line(alpha = 0.1) +  
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5) +
  stat_summary(aes(group = treatment_group), fun = mean, geom = "line", 
               linewidth = 1.5, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group = treatment_group), fun = mean, geom = "point", 
               size = 3.5, position = position_dodge(width = 0.2)) +  
  labs(
    title = "Perceived Agency Scores Over Time",
    x = "Timepoint",
    y = "Perceived Agency Score",
    color = "Treatment Group"
  ) +
  theme_minimal()

#plotting for functionality appreciation
ggplot(df_randomized_imputed_proximal_outcomes_long %>% 
         filter(variable == "fas_sum"), 
       aes(x = time, y = value, color = treatment_group, group = Participant_ID)) +
  geom_line(alpha = 0.1) +  
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5) +
  stat_summary(aes(group = treatment_group), fun = mean, geom = "line", 
               linewidth = 1.5, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group = treatment_group), fun = mean, geom = "point", 
               size = 3.5, position = position_dodge(width = 0.2)) +  
  labs(
    title = "Functionality Appreciation Scores Over Time",
    x = "Timepoint",
    y = "Functionality Appreciation Score",
    color = "Treatment Group"
  ) +
  theme_minimal()

#plotting for state body image
ggplot(df_randomized_imputed_proximal_outcomes_long %>% 
         filter(variable == "biss_sum"), 
       aes(x = time, y = value, color = treatment_group, group = Participant_ID)) +
  geom_line(alpha = 0.1) +  
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5) +
  stat_summary(aes(group = treatment_group), fun = mean, geom = "line", 
               linewidth = 1.5, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group = treatment_group), fun = mean, geom = "point", 
               size = 3.5, position = position_dodge(width = 0.2)) +  
  labs(
    title = "State Body Image Scores Over Time",
    x = "Timepoint",
    y = "State Body Image Score",
    color = "Treatment Group"
  ) +
  theme_minimal()