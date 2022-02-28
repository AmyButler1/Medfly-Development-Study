library(tidyverse)
library(lubridate)
library(skimr)
library(rstatix)
library(kableExtra)
library(RColorBrewer)
library(forcats)

##read in data (doesnt work)
eclosions <- read_csv("data/pilot_eclosions.csv")

##rename dataset
eclosions <- eclosions %>% 
rename(eclosion_date = "eclosion date")%>% 
  mutate(eclosion_date = lubridate::dmy(eclosion_date))


##check data imported correctly
### view whole dataset
view(pilot_eclosions_na)
### view if number of rows is correct
nrow(pilot_eclosions_na)
#####correct number of rows
### check if number of columns is correct
length(pilot_eclosions_na)
####correct number of columns
##check data type (plate - factor, well - character, sex - factor,eclosion_date - date, eclosion_status - factor )
skim(pilot_eclosions_na)
##
head(pilot_eclosions_na)
##
str(pilot_eclosions_na)
##
glimpse(pilot_eclosions_na)
##
unique(pilot_eclosions_na)

eclosions %>% group_by(plate)
eclosions %>% group_by(plate) %>% is.na()
eclosions %>% group_by(plate) %>% is.na() %>% sum()
#[1] 331
eclosions %>% group_by(plate) %>% count(eclosion_status)

#filter out missing data
pilot_eclosions<- filter(pilot_eclosions_na, eclosion_status=="eclosed"|eclosion_status=="not_eclosed")
view(pilot_eclosions)

#Testing for normal distribution - eclosion date
#QQ plot
### DOESNT WORK 
pilot_eclosions %>%
  ggplot(aes(sample = eclosion_date))+
  stat_qq()+
  stat_qq_line()
#Shapiro-Wilk test
##DOESNT WORK 
pilot_eclosions%>%
  shapiro_test(eclosion_date)
  
ggplot(pilot_eclosions, aes(x=eclosion_date), breaks=0)+
  geom_histogram()+
  labs(x= "Date of Eclosion")+
  theme_classic()+

ggplot(pilot_eclosions, aes(x=eclosion_date), breaks=0)+
  geom_histogram()+
  labs(x= "Date of Eclosion")+
  theme_classic()+
  facet_wrap(pilot_eclosions$plate)

ggplot(pilot_eclosions, aes(x=eclosion_date), breaks=0)+
  geom_histogram()+
  labs(x= "Date of Eclosion")+
  theme_classic()+
  facet_wrap(pilot_eclosions$sex)

ggplot(eclosions, aes(x=eclosion_date, fill=as.factor(plate)),position=stack)+
  +     geom_density(alpha=0.4)+
  +     labs(x= "Date of Eclosion")

#chi-squared --> is there a significant association between plate and eclosion status (missing data removed)
eclosion_chi <- glm(plate~eclosion_status, family=binomial, data=pilot_eclosions)
summary(eclosion_chi)
kableExtra::kbl(car::Anova(eclosion_chi, type="II")) %>% 
  kableExtra::kable_minimal()
###LR Chisq = 0.0621999, Df = 1, Pr(>Chisq) = 0.8030521
###no significance between eclosion across plates


#chi-squared --> is there a significant asociation between sex and eclosion date 
eclosion_chi_sex <- glm(sex~eclosion_date, family=binomial, data=pilot_eclosions)
summary(eclosion_chi_sex)
kableExtra::kbl(car::Anova(eclosion_chi_sex, type="II")) %>% 
  kableExtra::kable_minimal()
###LR 0.3253942, Df = 1, Pr(>Chisq)= 0.5683838
###no significant association between sex and eclosion date

#is there a signicant diffeence between the amount of error data between plates
###combine factors of eclosion status 'eclosed' and 'not_eclosed' together
eclosion_na_collapse <- fct_collapse(pilot_eclosions_na$eclosion_status,not_error = c("eclosed", "not_eclosed"))
view(eclosion_na_collapse)
#####currently gets rid of rest of table !!!!!!!!!!!!!!!!!!!!
pilot_eclosions_na_chi<- glm(plate~eclosion_status,family = binomial, data = eclosion_na_collapse)


