source("scripts/functions.R") #loads required functions

### TIDY DATA #######################################################

egg_size <- read_excel("data/final_egg_size.xlsx")
# view(egg_size)
# skim(egg_size)
head(egg_size)


egg_size <- egg_size %>%
  rename(fly_line = "line")


egg_size_model <- lm(egg_volume~ larval_diet*long_term_diet, data = egg_size)
egg_size_model

library(lme4)
library(lmerTest)

## egg volume - cubic micrometers
egg_size_model <- lmer(egg_volume ~ larval_diet*long_term_diet +(1|fly_line), data = egg_size)

means_egg_size <- emmeans::emmeans(egg_size_model, specs = pairwise~ larval_diet*long_term_diet, type = "response")
means_egg_size

sim_egg_size <- DHARMa::simulateResiduals(egg_size_model)
plot(sim_egg_size)
####residuals fit and high homogenity of varience

as.data.frame(means_egg_size$emmeans) %>% ggplot(aes(x=larval_diet, y = emmean, colour = long_term_diet))+geom_point(size = 4)+geom_point(data = egg_size, aes(x = larval_diet, y = egg_volume, colour = long_term_diet ), alpha = 0.2)+theme_classic()
as.data.frame(means_egg_size$emmeans) %>% ggplot(aes(x=long_term_diet, y = emmean, colour = larval_diet))+geom_point(size = 4)+geom_point(data = egg_size, aes(x = long_term_diet, y = egg_volume, colour = larval_diet ), alpha = 0.2)+theme_classic()

ggplot(aes( x = egg_length, y= egg_width, colour=larval_diet), data = egg_size)+geom_point()+geom_smooth(method="lm")
###shows very little correlation between egg width and egg length
