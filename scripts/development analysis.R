source("scripts/functions.R") #loads required functions

development_data <- read_excel("data/full_development_data.xlsx")

# clean up column names
development_data <- janitor::clean_names(development_data)

# table(development_data$eclosion_status) found spelling mistakes in data for not_eclosed and partially_eclosed
#group partially_eclosed and not_elcosed together for survival analysis
development_data <- development_data%>%
  mutate(eclosion_status = recode(eclosion_status, 'not elosed' = "not_eclosed", 'partially eclosed' = "not_eclosed", partially_ecllosed = "not_eclosed", partially_eclosed = "not_eclosed"))


#rename m and f - males and females
development_data <- development_data %>%
  mutate(sex = recode(sex, 'f' = "female", 'm' = "male"))

####################### pupae to adult survival ####################################################################

development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, eclosion_status) %>% 
  summarise(n=n())

#class(development_data$eclosion_status) - shows that eclosion_status is character so needs to be changed to factor
development_data$eclosion_status <- as.factor(development_data$eclosion_status)
# levels(development_data$eclosion_status) - shows levels "elosed" and "not_eclosed"


development_data$eclosion_status <- factor(development_data$eclosion_status, levels = c("not_eclosed","eclosed"))

eclosion_model <- glmer(eclosion_status ~ longterm_diet*larval_diet+(1|fly_line/plate/well)+(1|egg_collection_date), data = development_data, family = binomial)
eclosion_model <- glmer(eclosion_status ~ longterm_diet*larval_diet+(1|fly_line)+(1|plate)+(1|well)+(1|egg_collection_date), data = development_data, family = binomial)
means_survival <- emmeans::emmeans(eclosion_model, specs =pairwise ~longterm_diet*larval_diet, type = "response")
#prob of not eclosing:
#ASG,ASG - 0.0676
#Starch ASG, - 0.0730
#ASG, Starch - 0.1137
# Starch Starch - 0.0617
# all contrasts between diets significant except for ASGASG - ASG-Starch
###### suggests starch on starch has highest pupae to adult survival rate then ASG ASG
###### therefore highest survival on own diets
###### the non significance between ASG on own diet and on starch suggests larval starch diet doesn't have negative affect on survival but larval ASG diet does 


summary(eclosion_model)

# very small effect of random effects
# Intercept = -2.62411
# larval_diet starch = 0.57038                                - starch as larval diet increases prob of eclosion (not significant)
# longterm_diet starch = 0.08247                            - longterm starch diet slightly increase prob of eclosion (significant)
# larval_diet starch and longterm_diet starch = -0.74983      - negative interaction between larval/longterm starch diet (not significant)
# therefore longterm starch diet increases survival


sim_survival <- DHARMa::simulateResiduals(eclosion_model)
plot(sim_survival)
####residuals fit and high homogenity of varience






############### CANT FIGURE OUT GRAPH ######################################################################################################
as.data.frame(means_survival$emmeans) %>%
    ggplot(aes(x=longterm_diet, y = emmean, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  geom_point(data = development_data, aes(x = longterm_diet, y = count(eclosion_status), colour = larval_diet ), alpha = 0.2,position=position_dodge(width=0.9))+
  theme_classic()
survival <- as.data.frame(means_survival$emmeans) %>%
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = emmean, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  
  theme_classic()



#####graph to use #####################
survival_graph <- as.data.frame(means_survival$emmeans) %>%
  ggplot(aes(x=longterm_diet, y = prob, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  geom_errorbar(aes(y=prob, ymin=asymp.LCL, ymax=asymp.UCL, colour =larval_diet), width = 0.05, position = position_dodge(width = 0.9))+
  ylim(0.5, 1)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="regime diet", y="probability of eclosion")+
  guides(colour=guide_legend(title = "proximate diet"))+
  theme_minimal()

survival_graph <- survival_graph +theme(axis.title = element_text(size = 15), axis.text = element_text(size=13))

survival_data<-as.data.frame(means_survival$emmeans)
 
development_data %>%
  ggplot(aes(x=longterm_diet, y = count(eclosion_status), colour = larval_diet))+
  geom_point()
  
  geom_point(data = development_data, aes(x = longterm_diet, y = count(eclosion_status), colour = larval_diet ), alpha = 0.2,position=position_dodge(width=0.9))+
  theme_classic()
survival <- as.data.frame(means_survival$emmeans) %>%
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = emmean, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  theme_classic()



survival_plot <- as.data.frame(means_survival$emmeans) %>% 
  ggplot(aes(x=longterm_diet, y = prob, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  geom_boxplot(data = development_data, aes(x = longterm_diet, y = count(eclosion_status), colour = larval_diet, fill=larval_diet), alpha = 0.2, position=position_dodge(width=0.9))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="longterm diet", y="adult female weight (mg)")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 

development_data%>%
  group_by(larval_diet, longterm_diet)%>%
  summarise(n=n())%>%
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = (n/sum(n))*100))+
  geom_col(aes(fill=interaction(longterm_diet, larval_diet), width=0.8, position=position_dodge(width=0.9), alpha=0.6))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x= "diet combination (longterm diet, larval diet)", y = "percentage of pupae eclosed")+
  theme_classic()
  
development_data%>%
  group_by(larval_diet, longterm_diet)%>%
  summarise(n=n())%>%
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = (n/sum(n))*100))+
  geom_col(aes(fill=interaction(longterm_diet,larval_diet)), show.legend = FALSE)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x= "diet combination (longterm diet, larval diet)", y = "percentage of pupae eclosed")+
  theme_classic()


development_data%>%
  group_by(larval_diet, longterm_diet)%>%
  summarise(n=n())%>%
  ggplot(aes(x=longterm_diet, y=(n/sum(n))*100))+
  geom_col(aes(fill=larval_diet), width=0.8, position=position_dodge(width=0.9),
  alpha=0.6)+
  theme_classic()


sruvival_data <- development_data%>%
  ((filter(eclosion_status=="eclosed")%>%
  group_by(interaction(longterm_diet, larval_diet))%>%
  count())/(group_by(interaction(longterm_diet, larval_diet))%>%
              count()))*100

development_data%>%
  filter(eclosion_status=="eclosed")%>%
  group_by(interaction(longterm_diet, larval_diet))%>%
  count()
development_data%>%
  group_by(interaction(longterm_diet, larval_diet))%>%
  count()
#eclosions
# A-A - 2151/2321  - 92.68%
# s-A - 1947/2079  - 93.65
# A-S - 2100/2381 - 88.20
# S-S - 1805/1901 - 95.00










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

pupation <- development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_to_pupation, colour= sex, fill = sex))+
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
      seed = 1, width = 0.1))+
  labs(x= "diet combination (regime diet, proximate diet)", y = "days from egg laid to pupation")+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme(axis.text.x = element_text(size = 8))+
  facet_wrap(~sex)+
  theme_minimal()



############### Analysis


development_model <- lm(days_to_pupation ~ longterm_diet*larval_diet*fly_line, data = development_data)

development_model1 <- glm(days_to_pupation ~sex+longterm_diet*larval_diet, data = development_data, family = poisson(link = "log"))
broom::tidy(development_model2, exponentiate=T, conf.int=T)


#mixed model - some variance caused by random effects so need to use this
development_model2<- glmer(days_to_pupation ~ sex+longterm_diet*larval_diet+(1|fly_line)+(1|egg_collection_date), data = development_data, family = poisson(link = "log"))



emmeans::emmeans(development_model2, specs = ~ sex+longterm_diet*larval_diet, type = "response")
emmeans::emmeans(development_model2, specs = pairwise~ sex+longterm_diet*larval_diet, type = "response")

emmeans::emmeans(development_model2, specs =pairwise ~ longterm_diet*larval_diet, type = "response")
emmeans::emmeans(development_model2, specs = ~ longterm_diet*larval_diet, type = "response")


####################### days to eclosion ###################################################################################
development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_to_eclosion) %>% 
  summarise(n=n())

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=longterm_diet, y = days_to_eclosion, colour= larval_diet))+
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.4))+
  facet_wrap(~sex)

eclosion <- development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_to_eclosion, colour= sex, fill = sex))+
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
    position = position_jitter(seed = 1, width = 0.1))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x= "diet combination (regime diet, proximate diet)", y= "days from egg laid to eclosion")+
  facet_wrap(~sex)+
  theme_minimal()


############### Analysis


development_model3 <- lm(days_to_eclosion ~ larval_diet*longterm_diet*fly_line, data = development_data)

development_model3 <- glmer(days_to_eclosion ~ sex+larval_diet*longterm_diet+(1|fly_line)+(1|plate)+(1|well)+(1|egg_collection_date), data = development_data, family = poisson(link = "log"))

development_model3<- glm(days_to_eclosion ~ sex+longterm_diet*larval_diet, data = development_data, family = poisson(link="log"))
broom::tidy(development_model3, exponentiate=T, conf.int=T)


emmeans::emmeans(development_model3, specs = ~ longterm_diet*larval_diet, type = "response")





####################### days to pupation to eclosion #######################################################################

development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_pupation_to_eclosion) %>% 
  summarise(n=n())

development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=longterm_diet, y = days_pupation_to_eclosion, colour= larval_diet))+
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.4))+
  facet_wrap(~sex)


pupation_eclosion <-development_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x=interaction(longterm_diet, larval_diet), y = days_pupation_to_eclosion, colour= sex, fill = sex))+
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
    position = position_jitter(seed = 1, width = 0.1))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="diet combination (regime diet, proximate diet)", y = "days from pupation to eclosion")+
  theme(axis.text.x = element_text(size = 8))+
  facet_wrap(~sex)+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  theme_minimal()
###### cant figure out how to get rid of facet labels --- strip text and strip backgound isnt working ############





############### Analysis


development_model4 <- lm(days_pupation_to_eclosion ~ longterm_diet*larval_diet*fly_line, data = development_data)



development_model4<- glm(days_pupation_to_eclosion ~ sex+longterm_diet*larval_diet, data = development_data, family = poisson(link="log"))
broom::tidy(development_model4, exponentiate=T, conf.int=T)

# model with random variables
development_model4 <- glmer(days_pupation_to_eclosion ~ sex+longterm_diet*larval_diet+(1|fly_line) +(1|plate)+(1|well)+(1|egg_collection_date), data = development_data, family = poisson(link = "log"))
emmeans::emmeans(development_model4, specs = ~ longterm_diet*larval_diet, type = "response")

################################ pupation and eclosion graphs ###########################
(eclosion+theme(axis.title = element_text(size = 17), axis.text = element_text(size=13)))/((pupation+theme(axis.title = element_text(size = 17), axis.text = element_text(size=13)))+(pupation_eclosion+theme(axis.title = element_text(size = 17), axis.text = element_text(size=13)))+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Effect of longterm diet and larval diet on development times in males and females", tag_levels = "A")+
  theme(plot.title = element_text(size = 20), axis.title = element_text(size=20)
        
eclosion<- eclosion +theme(axis.title = element_text(size = 16), axis.text = element_text(size=13))
pupation<- pupation+theme(axis.title = element_text(size = 16), axis.text = element_text(size=13))
pupation_eclosion<- pupation_eclosion+theme(axis.title = element_text(size = 16), axis.text = element_text(size=13))

eclosion/pupation/pupation_eclosion+
  
  plot_layout(guides = "collect")+
  theme(legend.text = element_text(size=13), legend.title = element_text(size = 15))+
  plot_annotation( tag_levels = "A")+
  theme(plot.tag = element_text(size = 20))







############### first eclosion to first egg laying of group ####################################################################

cage_data <- read_excel("data/final_egg_collections.xlsx")

# clean up column names
cage_data <- janitor::clean_names(cage_data)

# first eclosion to first egg laying
development_data2 <- cage_data %>% 
  mutate(days_eclosion_to_egg_laying = as.numeric((f2_first_egg_laying - f2_first_eclosion_date)))


development_data2 %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_eclosion_to_egg_laying) %>% 
  summarise(n=n())

# plot





meansd<-function(x, ...){
  mean<-mean(x)
  sd<-sd(x)
  c(mean - sd,mean,mean+sd)
}


eclosion_egg_laying <-development_data2 %>%
  ggplot(aes(y=interaction(longterm_diet, larval_diet), x=days_eclosion_to_egg_laying,  fill=interaction(longterm_diet,larval_diet)))+
  geom_density_ridges(quantile_lines = T, quantile_fun = meansd, show.legend = FALSE, alpha=0.4)+
  stat_summary(aes(label=round(..x.., 2)),fun = "mean", geom = "text", size=5, show.legend = FALSE)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="days from first eclosion to first egg laying", y="diet (regime diet, proximate diet)")+
  
  theme_minimal()
eclosion_egg_laying +theme(axis.text = element_text(size=13), axis.title = element_text(size=17))

############### Analysis


development_model5 <- lm(days_eclosion_to_egg_laying ~ longterm_diet*larval_diet*fly_line, data = development_data2)

#account for random vairbale ---- no effect so just use normal glm
development_model5<- glmer(days_eclosion_to_egg_laying ~ longterm_diet*larval_diet+(1|fly_line)+(1|f2_egg_collection_date), data = development_data2, family = poisson(link = "log"))

development_model5<- glm(days_eclosion_to_egg_laying ~ longterm_diet*larval_diet, data = development_data2, family = poisson(link="log"))
broom::tidy(development_model5, exponentiate=T, conf.int=T)
means_egg <-emmeans::emmeans(development_model5, specs = ~longterm_diet*larval_diet, type = "response")


#trend towards . but not signif





############# days from egg laying to egg laying for cage data ###############################################################
development_data2 <- development_data2 %>% 
  mutate(egg_to_egg_laying = as.numeric((f2_first_egg_laying - f2_egg_collection_date)))

development_data2 %>% 
  group_by(fly_line, longterm_diet, larval_diet, egg_to_egg_laying) %>% 
  summarise(n=n())

############### Analysis


development_model6 <- lm(egg_to_egg_laying ~ longterm_diet*larval_diet*fly_line, data = development_data2)

#model with random variables - no effect so use glm
development_model6 <- glmer(egg_to_egg_laying ~ longterm_diet*larval_diet+(1|fly_line)+(1|f2_egg_collection_date), data = development_data2, family = poisson(link = "log"))

development_model6<- glm(egg_to_egg_laying ~ longterm_diet*larval_diet, data = development_data2, family = poisson(link="log"))
broom::tidy(development_model6, exponentiate=T, conf.int=T)

#model with random variables   --- random variables have no effect
development_model6 <- glmer(egg_to_egg_laying ~ longterm_diet*larval_diet+(1|fly_line)+(1|f2_egg_collection_date), data = development_data2, family = poisson(link = "log"))
summary(development_model6)
egg_laying_means <- emmeans::emmeans(development_model6, specs = ~ longterm_diet*larval_diet, type = "response")


egg_means <-as.data.frame(egg_laying_means$emmeans)
egg_means <- as.data.frame(egg_laying_means)

egg_egg_laying <- development_data2 %>%
  ggplot(aes(y=interaction(longterm_diet, larval_diet), x= egg_to_egg_laying, colour = interaction(longterm_diet,larval_diet), fill=interaction(longterm_diet,larval_diet)))+
  geom_density_ridges(show.legend = FALSE, alpha= 0.7)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="days from egg to first egg laying", y="diet combination (longterm diet, larval diet)")+
  theme_minimal()


egg_egg_laying <-development_data2 %>%
  ggplot(aes(y=interaction(longterm_diet, larval_diet), x=egg_to_egg_laying,  fill=interaction(longterm_diet,larval_diet)))+
  geom_density_ridges(quantile_lines = T, quantile_fun = meansd, show.legend = FALSE, alpha=0.4)+
  stat_summary(aes(label=round(..x.., 2)),fun = "mean", geom = "text", size=5, show.legend = FALSE)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="days from egg to first egg laying", y="diet (longterm diet, larval diet)")+
  theme_minimal()



############# egg plots ####################
#for presentation
(eclosion_egg_laying+egg_egg_laying)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Effect of longterm diet and larval diet on development times", tag_levels = "A")+
  theme(plot.title = element_text(size = 20))
#for paper
(eclosion_egg_laying+egg_egg_laying)+
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = "A")


####################### time of eclosion ###################################################################################


development_data$hour_of_event <- hour(development_data$eclosion_time)
development_data <- drop_na(development_data, c(hour_of_event, sex))

ggplot(development_data, aes(x = hour_of_event, fill = sex))+
  geom_histogram(aes(y=..density..),breaks = seq(0,24), position="dodge")+
  theme_minimal()+
  facet_wrap(~interaction(longterm_diet,larval_diet))+
  annotate("rect", xmin = 0, xmax = 8, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.4)+
  annotate("rect", xmin = 20, xmax = 24, ymin = 0, ymax = Inf, fill = "grey", alpha =0.4)+
  labs(x="hour of eclosion", y= "probability of eclosion")+
  xlim(0, 24)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()

  
coord_polar(start = 0)

eclosion_time <- glmer(hour_of_event ~ sex+longterm_diet*larval_diet+(1|fly_line), data = development_data, family = poisson(link = "log"))
summary(eclosion_time)
egg_laying_means <- emmeans::emmeans(development_model6, specs = ~ longterm_diet*larval_diet, type = "response")






