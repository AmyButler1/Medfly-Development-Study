source("scripts/functions.R") #loads required functions

development_data <- read_excel("data/full development data.xlsx")

# clean up column names
development_data <- janitor::clean_names(development_data)

# turn difference in dates to eclosion in days
development_data <- development_data %>% 
  mutate(days_to_pupation = as.numeric((pupation_date - egg_collection_date))) 

development_data %>% 
  group_by(fly_line, longterm_diet, larval_diet, days_to_pupation) %>% 
  summarise(n=n())

development_data %>% 
  ggplot(aes(x=longterm_diet, y = days_to_pupation, fill= larval_diet))+
  geom_boxplot()+
  facet_wrap(~sex)
