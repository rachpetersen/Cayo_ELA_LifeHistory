

##OFFSPRING SURVIVAL


pal <- ggthemes::tableau_color_pal(palette="Green-Orange-Teal")(12)

#to visualize the colors:
df <- data.frame(color = factor(pal, levels = pal))
ggplot(df, aes(x = color, fill = color)) +
  geom_bar(stat = "count") +
  scale_fill_identity()


#load list of females who died naturally and their number of surviving offspring to 4yo
natural_death_moms<-readRDS("natural_death_moms.rds")
Repro_ELA_data<- readRDS("Reproduction_ELA_data.rds")
Repro_fitness<- merge(Repro_ELA_data, natural_death_moms, by="MaternalID.text")


###### ###### ###### ###### ###### ###### ###### ###### 
###### Age first birth and surviving offspring ######
###### ###### ###### ###### ###### ###### ###### ###### 


fitness_afb<-lmer(surviving_offspring ~ AgeFirstBirthYrs + (1|Birth.Season) + (1|NatalGroup), data=Repro_fitness)
fitness_afb_results<-summary(fitness_afb) 
fitness_afb_df<-fitness_afb_results$coefficients
#write.csv(fitness_afb_df, "fitness_afb_results.csv")

fitness_afb2<-lmer(surviving_offspring ~ AgeFirstBirthYrs + I(AgeFirstBirthYrs^2) + (1|Birth.Season) + (1|NatalGroup), data=Repro_fitness)
summary(fitness_afb2)

AIC(fitness_afb, fitness_afb2)

fitness_afb_plot<-effect_plot(fitness_afb, pred = AgeFirstBirthYrs, interval = TRUE, plot.points = TRUE, line.colors=pal[3], point.size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Age at first birth"); fitness_afb_plot
#ggsave("fitness_afb_plot.pdf", fitness_afb_plot, height=3.5, width=3.5)



###### ###### ###### ###### ###### ###### ###### ###### 
###### Reproductive rate and surviving offspring ######
###### ###### ###### ###### ###### ###### ###### ###### 


Repro_fitness$Prop_Seasons<-Repro_fitness$NumberBirths/Repro_fitness$SeasonSpan

fitness_propbirth <- lmer(surviving_offspring ~ Prop_Seasons +  (1|Birth.Season) + (1|NatalGroup), data = Repro_fitness)
summary(fitness_propbirth)

fitness_propbirth2 <- lmer(surviving_offspring ~ Prop_Seasons + I(Prop_Seasons^2) +  (1|Birth.Season) + (1|NatalGroup), data = Repro_fitness)
summary(fitness_propbirth2)
fitness_propbirth2_results<-summary(fitness_propbirth2) 
fitness_propbirth2_df<-fitness_propbirth2_results$coefficients
#write.csv(fitness_propbirth2_df, "fitness_propbirth2_results.csv")

AIC(fitness_propbirth, fitness_propbirth2) #squared term is better fit

fitness_propbirth_plot<-effect_plot(fitness_propbirth2, pred = Prop_Seasons, interval = TRUE, plot.points = FALSE, line.colors=pal[3], point.size=0.5) +
  geom_jitter(data= Repro_fitness, aes(x= Prop_Seasons, y=surviving_offspring), width=0.01, size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Proportion of years giving birth"); fitness_propbirth_plot

#ggsave("fitness_propbirth_plot.pdf", fitness_propbirth_plot, height=3.5, width=3.5)


# At what value of Prop Seasons is survivng_offpsring maximized? 

coefs <- fixef(fitness_propbirth2)  # Extract fixed effects
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

allage_fitness<-rbind(Morethan4_fitness, Lessthan4_fitness)
fitness_size_all<-lmer(surviving_offspring ~ residual_size + (1|Birth.Season) + (1|NatalGroup), data=allage_fitness)
summary(fitness_size_all)
fitness_size_results<-summary(fitness_size_all) 
fitness_size_df<-fitness_size_results$coefficients
#write.csv(fitness_size_df, "fitness_size_results.csv")

fitness_size_all2<-lmer(surviving_offspring ~ residual_size + I(residual_size^2) + (1|Birth.Season) + (1|NatalGroup), data=allage_fitness)
summary(fitness_size_all2) 

AIC(fitness_size_all, fitness_size_all2) #dAIC is 1.5 

fitness_size_all_plot<-effect_plot(fitness_size_all, pred = residual_size, interval = TRUE, plot.points = TRUE, line.colors=pal[5], point.size=0.5) +
  theme_bw(14) +
  ylab("Number of surviving offspring") +
  xlab("Body weight controlling for age"); fitness_size_all_plot

#ggsave("fitness_size_all_plot.pdf", fitness_size_all_plot, height=3.5, width=3.5)
