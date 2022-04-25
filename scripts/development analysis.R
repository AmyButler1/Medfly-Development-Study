source("scripts/functions.R") #loads required functions

development_data <- read_excel("data/full_development_data.xlsx")

# clean up column names
development_data <- janitor::clean_names(development_data)

# table(development_data$eclosion_status) found spelling mistakes in data for not_eclosed and partially_eclosed
#group partially_eclosed and not_elcosed together for survival analysis
development_data <- development_data%>%
  mutate(eclosion_status = recode(eclosion_status, 'not elosed' = "not_eclosed", 'partially eclosed' = "not_eclosed", partially_ecllosed = "not_eclosed", partially_eclosed = "not_eclosed"))

####################### pupae to adult survival ####################################################################

development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, eclosion_status) %>% 
  summarise(n=n())


development_data%>%
  group_by(larval_diet, longterm_diet)%>%
  summarise(n=n())%>%
  ggplot(aes(x=longterm_diet, y=(n/sum(n))*100))+
  geom_col(aes(fill=larval_diet), width=0.8, position=position_dodge(width=0.9),
  alpha=0.6)+
  theme_classic()


########## analysis
development_data%>%
  drop_na(eclosion_status)


development_model <- glmer(eclosion_status ~ larval_diet*longterm_diet+(1|fly_line/plate/well)+(1|egg_collection_date), data = development_data, family = binomial(logit))





####################### turn difference in dates to eclosion in days ######################################################
development_data <- development_data %>% 
  mutate(days_to_pupation = as.numeric((pupation_date - egg_collection_date)))


development_data <- development_data %>% mutate(days_to_eclosion = as.numeric((eclosion_date - egg_collection_date)))

development_data <- development_data %>% mutate(days_pupation_to_eclosion = as.numeric((days_to_eclosion - days_to_pupation)))


development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_to_pupation) %>% 
  summarise(n=n())






####################### days to pupation ##################################################################################
development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_to_pupation) %>% 
  summarise(n=n())


development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=longterm_diet, y = days_to_pupation, colour= larval_diet))+
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.4))+
  facet_wrap(~sex)

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_to_pupation, colour= larval_diet, fill = larval_diet))+
  ggdist::stat_halfeye(
    adjust = 0.8,
    width = 0.4, 
    ## 95% CI
    .width = 0,
    justification = -0.4,
    point_colour = "NA",
    size=0.5,
    alpha = 0.8)+
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width = 0.1
      
    )
  )+
  facet_wrap(~sex)+
  theme_minimal()


############### Analysis


development_model <- lm(days_to_pupation ~ larval_diet*longterm_diet*fly_line, data = development_data)


development_model <- glmer(days_to_pupation ~ sex+larval_diet*longterm_diet+(1|fly_line/plate/well)+(1|egg_collection_date), data = development_data, family = poisson(link = "log"))


####################### days to eclosion ###################################################################################
development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_to_eclosion) %>% 
  summarise(n=n())

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=longterm_diet, y = days_to_eclosion, colour= larval_diet))+
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.4))+
  facet_wrap(~sex)

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_to_eclosion, colour= larval_diet, fill = larval_diet))+
  ggdist::stat_halfeye(
    adjust = 0.8,
    width = 0.4, 
    ## 95% CI
    .width = 0,
    justification = -0.4,
    point_colour = "NA",
    size=0.5,
    alpha = 0.8)+
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width = 0.1
      
    )
  )+
  facet_wrap(~sex)+
  theme_minimal()


############### Analysis


development_model <- lm(days_to_eclosion ~ larval_diet*longterm_diet*fly_line, data = development_data)

development_model <- glmer(days_to_eclosion ~ sex+larval_diet*longterm_diet+(1|fly_line/plate/well)+(1|egg_collection_date)+(1|obs), data = development_data, family = poisson(link = "log"))










####################### days to pupation to eclosion #######################################################################

development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_pupation_to_eclosion) %>% 
  summarise(n=n())

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=longterm_diet, y = days_pupation_to_eclosion, colour= larval_diet))+
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.4))+
  facet_wrap(~sex)

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_pupation_to_eclosion, colour= larval_diet, fill = larval_diet))+
  ggdist::stat_halfeye(
    adjust = 0.8,
    width = 0.4, 
    ## 95% CI
    .width = 0,
    justification = -0.4,
    point_colour = "NA",
    size=0.5,
    alpha = 0.8)+
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width = 0.1
      
    )
  )+
  facet_wrap(~sex)+
  theme_minimal()


############### Analysis


development_model <- lm(days_pupation_to_eclosion ~ larval_diet*longterm_diet*fly_line, data = development_data)

development_model <- glmer(days_pupation_to_eclosion ~ sex+larval_diet*longterm_diet+(1|fly_line/plate/well)+(1|egg_collection_date)+(1|obs), data = development_data, family = poisson(link = "log"))











####################### time of eclosion ###################################################################################


development_data$hour_of_event <- hour(development_data$eclosion_time)
development_data <- drop_na(development_data, c(hour_of_event, sex))

ggplot(development_data, aes(x = hour_of_event, fill = sex))+
  geom_histogram(aes(y=..density..),breaks = seq(0,24), position="dodge")+
  theme_minimal()+
  facet_wrap(~larval_diet)+
  annotate("rect", xmin = 0, xmax = 8, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.4)+
  annotate("rect", xmin = 20, xmax = 24, ymin = 0, ymax = Inf, fill = "grey", alpha =0.4)
  
coord_polar(start = 0)



#################### pupae to adult survial ###############################################################################




