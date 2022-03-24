source("scripts/functions.R") #loads required functions

### TIDY DATA #######################################################

egg_size2 <- read_csv("data/egg size both.csv")
# view(egg_size)
# skim(egg_size)
head(egg_size2)


egg_size2 <- egg_size2 %>%
  rename(fly_line = "line",
         selection_diet = "line_type")
  
  
  ### PLOTS ############################################################

egg_plots2 <- function( data = egg_size2, egg_measure, line){ggplot(aes(y= .data[[egg_measure]], x= .data[[line]]), data = data)+ geom_boxplot(width=0.1, aes(fill= .data[[line]]))+ scale_color_brewer(palette = "Set2")+scale_fill_brewer(palette = "Set2")+theme_classic()}


egg_plots2(data = egg_size, egg_measure = "egg_length", line = "fly_line")
egg_plots2(data = egg_size, egg_measure = "egg_width", line = "fly_line")


#egg length box plot
egg_size2 %>%
  ggplot(aes(y= egg_length, x= fly_line))+
  geom_boxplot(width=0.1, aes(fill= fly_line))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Fly line", y=expression(paste("Egg length ("~mu~"m)")))+
  theme_classic()

#egg width box plot
egg_size2 %>%
  ggplot(aes(y= egg_width, x= fly_line))+
  geom_boxplot(width=0.1, aes(fill= fly_line))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Fly line", y="Egg width (micrometers)")+
  theme_classic()

#egg volume box plot
egg_size2 %>%
  ggplot(aes(y= egg_volume, x= fly_line))+
  geom_boxplot(width=0.1, aes(fill= fly_line))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Fly line", y="Egg volume (cubic micrometers)")+
  theme_classic()

#egg volume box plot
egg_size2 %>%
  ggplot(aes(y= egg_volume, x= selection_diet))+
  geom_boxplot(width=0.1, aes(fill= selection_diet))+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Fly line", y="Egg volume (cubic micrometers)")+
  theme_classic()

ggplot(aes( x = egg_length, y= egg_width, colour=selection_diet), data = egg_size2)+geom_point()+geom_smooth(method="lm")

### STATISTICS ##############################

#Spearmans rank correlation coefficient for length and width
spearmans<- cor.test(egg_size2$egg_length, egg_size2$egg_width, method="spearman")
spearmans
##p-value .... so significant correlation between length and width

#t-test to check volume between line types

# Rename replicate lines for analysis

egg_ls1 <- lm(egg_volume ~ selection_diet, data = egg_size)
summary(egg_ls1)

egg_ls2 <- lm(egg_volume ~ selection_diet*fly_line, data = egg_size)
summary(egg_ls2)

egg_ls3 <- lm(egg_volume ~ selection_diet/fly_line, data = egg_size)
summary(egg_ls3)
plot(egg_ls3)