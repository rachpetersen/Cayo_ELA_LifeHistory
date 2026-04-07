

##OFFSPRING SURVIVAL

library(tidyverse)
library(lmerTest)
library(ggthemes)

pal <- ggthemes::tableau_color_pal(palette="Green-Orange-Teal")(12)

#load list of females who died naturally and their number of surviving offspring to 4yo
natural_death_moms<-readRDS("natural_death_moms.rds")
Repro_ELA_data<- readRDS("Reproduction_ELA_data.rds")
Repro_fitness<- merge(Repro_ELA_data, natural_death_moms, by="MaternalID.text")


###### ###### ###### ###### ###### ###### ###### ###### 
###### Age first birth and surviving offspring ######
###### ###### ###### ###### ###### ###### ###### ###### 

fitness_afb<-glmer(surviving_offspring ~ AgeFirstBirthYrs + (1|Birth.Season) + (1|NatalGroup), data=Repro_fitness, family=poisson)
fitness_afb2<-glmer(surviving_offspring ~ AgeFirstBirthYrs + I(AgeFirstBirthYrs^2) + (1|Birth.Season) + (1|NatalGroup), data=Repro_fitness, family=poisson)
fitness_afb2_results<-summary(fitness_afb2)
fitness_afb2_df<-fitness_afb2_results$coefficients

AIC(fitness_afb, fitness_afb2) #quadratic model better fit

fitness_afb_plot<-effect_plot(fitness_afb2, pred = AgeFirstBirthYrs, interval = TRUE, plot.points = TRUE, line.colors=pal[3], point.size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Age at first birth"); fitness_afb_plot


###### ###### ###### ###### ###### ###### ###### ###### 
###### Reproductive rate and surviving offspring ######
###### ###### ###### ###### ###### ###### ###### ###### 


Repro_fitness$Prop_Seasons<-Repro_fitness$NumberBirths/Repro_fitness$SeasonSpan

fitness_propbirth <- glmer(surviving_offspring ~ Prop_Seasons + (1|Birth.Season) + (1|NatalGroup), data = Repro_fitness, family=poisson)
fitness_propbirth2 <- glmer(surviving_offspring ~ Prop_Seasons + I(Prop_Seasons^2) +  (1|Birth.Season) + (1|NatalGroup), data = Repro_fitness, family=poisson)
fitness_propbirth2_results<-summary(fitness_propbirth2) 
fitness_propbirth2_df<-fitness_propbirth2_results$coefficients

AIC(fitness_propbirth, fitness_propbirth2) #quadratic model better fit

fitness_propbirth_plot<-effect_plot(fitness_propbirth2, pred = Prop_Seasons, interval = TRUE, plot.points = FALSE, line.colors=pal[3], point.size=0.5) +
  geom_jitter(data= Repro_fitness, aes(x= Prop_Seasons, y=surviving_offspring), width=0.01, size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Proportion of years giving birth"); fitness_propbirth_plot

# At what value of Prop Seasons is survivng_offpsring maximized? 

coefs <- fixef(fitness_propbirth2)  # Extract fixed effects
b1 <- coefs["Prop_Seasons"]
b2 <- coefs["I(Prop_Seasons^2)"]

x_max <- -b1 / (2 * b2)
x_max

#What if we remove females with prop=1, offspring =0

Repro_fitness_filter<-subset(Repro_fitness, !(Prop_Seasons == 1 & surviving_offspring == 0))

fitness_propbirth2_sub <- glmer(surviving_offspring ~ Prop_Seasons + I(Prop_Seasons^2) + (1|Birth.Season) + (1|NatalGroup), data = Repro_fitness_filter, family=poisson)
summary(fitness_propbirth2_sub)

fitness_propbirth_plot<-effect_plot(fitness_propbirth2_sub, pred = Prop_Seasons, interval = TRUE, plot.points = FALSE, line.colors=pal[3], point.size=0.5) +
  geom_jitter(data= Repro_fitness_filter, aes(x= Prop_Seasons, y=surviving_offspring), width=0.01, size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Proportion of years giving birth"); fitness_propbirth_plot

coefs <- fixef(fitness_propbirth2_sub)  # Extract fixed effects
b1 <- coefs["Prop_Seasons"]
b2 <- coefs["I(Prop_Seasons^2)"]

x_max <- -b1 / (2 * b2)
x_max




###### ###### ###### ###### ###### ###### ###### ###### 
###### Body weight and surviving offspring ######
###### ###### ###### ###### ###### ###### ###### ###### 


#merge list of females who died naturally and their number of surviving offspring with morph data

Subadult_residsize<- readRDS("Subadult_residualsize.rds")
Lessthan4_fitness<-merge(Subadult_residsize, natural_death_moms, by.x="AnimalID.text", by.y="MaternalID.text") 

Adult_residsize<- readRDS("Adult_residualsize.rds")
Morethan4_fitness<-merge(Adult_residsize, natural_death_moms, by.x="AnimalID.text", by.y="MaternalID.text") 


#Does subadult residual weight predict surviving offspring
fitness_size_subadult<-glmer(surviving_offspring ~ residual_size + (1|Birth.Season) + (1|NatalGroup), data=Lessthan4_fitness, family=poisson)
summary(fitness_size_subadult) 

fitness_size_subadult2<-glmer(surviving_offspring ~ residual_size + I(residual_size^2) + (1|Birth.Season) + (1|NatalGroup), data=Lessthan4_fitness, family=poisson)

AIC(fitness_size_subadult, fitness_size_subadult2) #same

fitness_size_subadult_plot<-effect_plot(fitness_size_subadult, pred = residual_size, interval = TRUE, plot.points = TRUE, line.colors=pal[2], point.size=0.5) +
  theme_bw()+
  ylab("Number of surviving offspring") +
  xlab("Body size controlled for age"); fitness_size_subadult_plot #n.s.

#Does adult residual weight predict surviving offspring
fitness_size_adult<-glmer(surviving_offspring ~ residual_size + (1|Birth.Season) + (1|NatalGroup), data=Morethan4_fitness, family=poisson)
summary(fitness_size_adult) 

fitness_size_adult2<-glmer(surviving_offspring ~ residual_size + I(residual_size^2) + (1|Birth.Season) + (1|NatalGroup), data=Morethan4_fitness, family=poisson)

AIC(fitness_size_adult, fitness_size_adult2) #linear better fit

fitness_size_adult_plot<-effect_plot(fitness_size_adult, pred = residual_size, interval = TRUE, plot.points = TRUE, line.colors=pal[4], point.size=0.5) +
  ylab("Number of surviving offspring") +
  xlab("Adult residual body weight"); fitness_size_adult_plot #n.s.

