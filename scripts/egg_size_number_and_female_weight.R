source("scripts/functions.R") #loads required functions

###################### egg size #########################################################

### TIDY DATA #######################################################

egg_size <- read_excel("data/final_egg_size.xlsx")
# view(egg_size)
# skim(egg_size)
head(egg_size)


egg_size <- egg_size %>%
  rename(fly_line = "line")


egg_size_model <- lm(egg_volume~ long_term_diet*larval_diet, data = egg_size)
egg_size_model

library(lme4)
library(lmerTest)

## egg volume - cubic micrometers

egg_size_model <- lmer(egg_volume ~ long_term_diet*larval_diet +(1|fly_line)+(1|date_of_parental_egg_collection), data = egg_size)

means_egg_size <- emmeans::emmeans(egg_size_model, specs = pairwise~ long_term_diet*larval_diet, type = "response")
means_egg_size

anova(egg_size_model)

sim_egg_size <- DHARMa::simulateResiduals(egg_size_model)
plot(sim_egg_size)
####residuals fit and high homogenity of varience

as.data.frame(means_egg_size$emmeans) %>% ggplot(aes(x=long_term_diet, y = emmean, colour = larval_diet))+geom_point(size = 4, position=position_dodge(width=0.9))+geom_point(data = egg_size, aes(x = long_term_diet, y = egg_volume, colour = larval_diet ), alpha = 0.2, position=position_dodge(width=0.9))+theme_classic()


egg_plot <- as.data.frame(means_egg_size$emmeans) %>% 
  ggplot(aes(x=long_term_diet, y = emmean, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9), show.legend = FALSE)+
  geom_errorbar(aes(y=emmean, ymin=lower.CL, ymax=upper.CL, colour =larval_diet), width = 0.05, position = position_dodge(width = 0.9), show.legend = FALSE)+
  geom_boxplot(data = egg_size, aes(x = long_term_diet, y = egg_volume, colour = larval_diet, fill=larval_diet), alpha = 0.2, position=position_dodge(width=0.9), show.legend = FALSE)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="long-term diet", y="egg volume(cubic"~mu~"m)")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 
egg_plot

#final plot
egg_plot <- as.data.frame(means_egg_size$emmeans) %>% 
  ggplot(aes(x=long_term_diet, y = emmean,fill =larval_diet), show.legend = FALSE)+
  geom_point(shape=21, colour="black",size = 4, position=position_dodge(width=0.9), )+
  geom_errorbar(aes(y=emmean, ymin=lower.CL, ymax=upper.CL, colour =larval_diet), width = 0.05, show.legend = FALSE,position = position_dodge(width = 0.9))+
  geom_boxplot(data = egg_size, aes(x = long_term_diet, y = egg_volume, fill=larval_diet), alpha = 0.2)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="long-term diet", y="egg volume(cubic"~mu~"m)")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 
egg_plot
###################### body weight #########################################################

### TIDY DATA #######################################################

adult_female <- read_excel("data/adult_fly_weight.xlsx")

adult_female

weight.model <- lm(`weight (mg)`~ longterm_diet*larval_diet, data = adult_female)

library(lme4)
library(lmerTest)

weight.model <- lmer(`weight (mg)`~ longterm_diet*larval_diet +(1|fly_line)+(1|parental_egg_collection_date), data = adult_female)

means <- emmeans::emmeans(weight.model, specs = pairwise~ longterm_diet*larval_diet, type = "response")
means

anova(weight.model)

sim <- DHARMa::simulateResiduals(weight.model)
plot(sim)

as.data.frame(means$emmeans) %>% ggplot(aes(x=longterm_diet, y = emmean, colour = larval_diet))+geom_point(size = 4,position=position_dodge(width=0.9))+geom_point(data = adult_female, aes(x = larval_diet, y = `weight (mg)`, colour = longterm_diet ), alpha = 0.2,position=position_dodge(width=0.9))+theme_classic()


#final plot
body_weight <- as.data.frame(means$emmeans) %>% 
  ggplot(aes(x=longterm_diet, y = emmean,fill =larval_diet), show.legend = FALSE)+
  geom_point(shape=21, colour="black",size = 4, position=position_dodge(width=0.9), )+
  geom_errorbar(aes(y=emmean, ymin=lower.CL, ymax=upper.CL, colour =larval_diet), width = 0.05, show.legend = FALSE,position = position_dodge(width = 0.9))+
  geom_boxplot(data = adult_female, aes(x = longterm_diet, y = `weight (mg)`, fill=larval_diet), alpha = 0.2)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="longterm diet", y="adult female weight (mg)")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 
body_weight





############### egg number #######################################################
egg_number_data <- read_excel("data/final_egg_collections.xlsx")

# clean up column names
egg_number_data <- janitor::clean_names(egg_number_data)

fixed_egg_number_data <- egg_number_data <- egg_number_data %>% 
  mutate(fixed_egg_number = as.numeric((f3_egg_number/female_number)))

# this has t- value (and P-value in regards to t?)
egg_number_model <- lmer(fixed_egg_number ~ longterm_diet*larval_diet +(1|fly_line)+(1|f2_egg_collection_date), data = fixed_egg_number_data)

means_egg_number <- emmeans::emmeans(egg_number_model, specs = pairwise~ longterm_diet*larval_diet, type = "response")
means_egg_number

anova(egg_number_model)

sim_egg_number <- DHARMa::simulateResiduals(egg_number_model)
plot(sim_egg_number)
####residuals fit 

as.data.frame(means_egg_number$emmeans) %>% ggplot(aes(x=longterm_diet, y = emmean, colour = larval_diet))+geom_point(size = 4, position=position_dodge(width=0.9))+geom_point(data = egg_number_data, aes(x = longterm_diet, y = f3_egg_number, colour = larval_diet ), alpha = 0.2, position=position_dodge(width=0.9))+theme_classic()

egg_number_plot <- as.data.frame(means_egg_number$emmeans) %>% 
  ggplot(aes(x=longterm_diet, y = emmean, colour = larval_diet))+
  geom_point(size = 4, position=position_dodge(width=0.9))+
  geom_boxplot(data = egg_number_data, aes(x = longterm_diet, y = fixed_egg_number, colour = larval_diet, fill=larval_diet), alpha = 0.2, position=position_dodge(width=0.9))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="longterm diet", y="number of eggs laid per female")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 
#final plot
egg_number_plot <- as.data.frame(means_egg_number$emmeans) %>% 
  ggplot(aes(x=longterm_diet, y = emmean,fill =larval_diet), show.legend = FALSE)+
  geom_point(shape=21, colour="black",size = 4, position=position_dodge(width=0.9), )+
  geom_errorbar(aes(y=emmean, ymin=lower.CL, ymax=upper.CL, colour =larval_diet), width = 0.05, show.legend = FALSE,position = position_dodge(width = 0.9))+
  geom_boxplot(data = fixed_egg_number_data, aes(x = longterm_diet, y = fixed_egg_number, fill=larval_diet), alpha = 0.2)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="longterm diet", y="adult female weight (mg)")+
  guides(fill=guide_legend(title = "larval diet"))+
  theme_minimal() 
egg_number_plot
  
######### plot patchwork #######
#for presentation
(egg_plot+egg_number_plot)/(body_weight)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Effect of longterm diet and larval diet on egg volume, egg number, and adult female body weight", tag_levels = "A")+
  theme(plot.title = element_text(size = 20)) 
#for paper
(egg_plot+egg_number_plot)/(body_weight)+
  plot_layout(guides = "collect")+
  plot_annotation( tag_levels = "A")

