source("scripts/functions.R") #loads required functions

### TIDY DATA #######################################################

adult_female <- read_excel("data/adult_fly_weight.xlsx")

adult_female

weight.model <- lm(`weight (mg)`~ pupae_diet*longterm_diet, data = adult_female)

library(lme4)
library(lmerTest)

weight.model <- lmer(`weight (mg)`~ pupae_diet*longterm_diet +(1|fly_line), data = adult_female)

means <- emmeans::emmeans(weight.model, specs = pairwise~ pupae_diet*longterm_diet, type = "response")
means

sim <- DHARMa::simulateResiduals(weight.model)
plot(sim)

as.data.frame(means$emmeans) %>% ggplot(aes(x=pupae_diet, y = emmean, colour = longterm_diet))+geom_point(size = 4)+geom_point(data = adult_female, aes(x = pupae_diet, y = `weight (mg)`, colour = longterm_diet ), alpha = 0.2)+theme_classic()
as.data.frame(means$emmeans) %>% ggplot(aes(x=longterm_diet, y = emmean, colour = pupae_diet))+geom_point(size = 4)+geom_point(data = adult_female, aes(x = pupae_diet, y = `weight (mg)`, colour = longterm_diet ), alpha = 0.2)+theme_classic()

