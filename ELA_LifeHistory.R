

######## Section 1: How does ELA influence life history ############


library(Matrix)
library(ggthemes)
library(sjPlot)
library(coxme)
library(jtools)
library(interactions)
library(ggpubr)

pal <- ggthemes::tableau_color_pal(palette="Green-Orange-Teal")(12)

#to visualize the colors:
df <- data.frame(color = factor(pal, levels = pal))
ggplot(df, aes(x = color, fill = color)) +
  geom_bar(stat = "count") +
  scale_fill_identity()


############## ############## ############## ############## 
     ############## Subadult body weight #############
############## ############## ############## ############## 


Morph_weights_ELA<- readRDS("Morph_ELA_data.rds") %>%
  mutate(across(c(RankIndex, PrimpIndex, CompetingSibIndex, MomAllLossType), as.factor))

Lessthan4<-subset(Morph_weights_ELA, trapped_age < 4) #613 observations
plot(Lessthan4$trapped_age, Lessthan4$body_weight_kg)

Lessthan4_IDs<- Lessthan4 %>%
  distinct(AnimalID.text); dim(Lessthan4_IDs) #508 distinct individuals

# Run interaction model

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

for (predictor in predictors) {
  formula <- as.formula(paste("body_weight_kg ~", predictor, "* trapped_age + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  
  model <- lmer(formula, data = Lessthan4_edit)
  model_name <- paste("earlyweight", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[4, 1]     
  se <- fixed_effects[4, 2]        
  pvalue <- fixed_effects[4, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}

model <- lmer(body_weight_kg ~ RankIndex * trapped_age + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = Lessthan4_edit)
model_name <- "earlyweight_RankIndex_model"
assign(model_name, model)

fixed_effects <- summary(earlyweight_RankIndex_model)$coefficients
n <- nobs(earlyweight_RankIndex_model)
beta <- fixed_effects[5, 1]     
se <- fixed_effects[5, 2]        
pvalue <- fixed_effects[5, 5]
predictor <- "RankIndexMid"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))


fixed_effects <- summary(earlyweight_RankIndex_model)$coefficients
n <- nobs(earlyweight_RankIndex_model)
beta <- fixed_effects[6, 1]     
se <- fixed_effects[6, 2]        
pvalue <- fixed_effects[6, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))
results$padjust<-p.adjust(results$pvalue, method="BH")

#write.table(results, "EarlylifeGrowth_int_modelresults_21Apr25.txt", quote=F) 


#Make a forest plot with all the subadult body weight models

results$padjust<-p.adjust(results$pvalue, method="BH")

results$Predictor<-factor(results$Predictor, levels=rev(c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndexMid", "RankIndexLow", "CumulIndex7")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Group size", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Cumulative adversity")))
results$significance<-'ns'
results$significance[which(results$pvalue<0.05)]<-'p<0.05'
results$significance[which(results$padjust<0.1)]<-'FDR<0.1'
results$significance <- factor(results$significance, levels = c("ns", "p<0.05", "FDR<0.1"))

sigCOLS= c('grey',pal[6],pal[5])

SubAdultweight_forest<-ggplot(data= results, aes(x= Predictor, y=beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2], "FDR<0.1"=sigCOLS[3])) +
  xlab("") +ylab("Effect size");SubAdultweight_forest

#ggsave("Subadult_Weight_forest.pdf", SubAdultweight_forest,  width=5.5, height=3.5) 


# plot group size result
Lessthan4_edit$age_group <- cut(Lessthan4_edit$trapped_age,
                                breaks = c(0, 1, 2, Inf),
                                labels = c("0-1", "1-2", "2+"),
                                right = FALSE) 

Lessthan4_edit$s_grpsize_group <- cut(Lessthan4_edit$s_grpsize,
                                      breaks = quantile(Lessthan4_edit$s_grpsize, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                      labels = c("-0.6", "0.2", "2.17"),
                                      include.lowest = TRUE)

Lessthan4_edit2 <- Lessthan4_edit %>%
  filter(!is.na(s_grpsize))

rep_vals <- c(0.5, 1.5, 2.5)

quantile(Lessthan4_edit$s_grpsize, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

newdata <- expand.grid(
  s_grpsize = c(-0.6, 0.2, 2.17),
  age_group = c("0-1", "1-2", "2+")
)

# Add continuous values based on group
newdata$trapped_age <- rep(rep_vals, each = 3)

preds <- predict(earlyweight_s_grpsize_model, newdata = newdata, re.form = NA, se.fit = TRUE)

newdata$fit<-preds$fit
newdata$lower <- preds$fit - 1.96 * preds$se.fit
newdata$upper <- preds$fit + 1.96 * preds$se.fit

newdata$s_grpsize <-as.factor(newdata$s_grpsize)


earlyweight_grpsize_plot<- ggplot(newdata, aes(x = as.factor(age_group), y = fit, color = as.factor(s_grpsize), group=as.factor(s_grpsize))) +
  geom_point(data=Lessthan4_edit2, aes(x=as.factor(age_group), y=body_weight_kg, color = as.factor(s_grpsize_group), group=as.factor(s_grpsize_group)), size=0.5, alpha=0.4, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.80)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), size=1,  width=0.7, position = position_dodge(width = 0.8)) +
  geom_point(size=2.5, position = position_dodge(width = 0.8)) +
  labs(
    x = "Age (years)",
    y = "Body weight (kg)",
    color = "Early life\ngroup size"
  ) +
  theme_bw(14) +
  theme(legend.position = "right") +
  scale_color_manual(values=c(pal[6], pal[5], "#2f7f8f"), labels=c("-0.6"= "Small", "0.2" = "Medium", "2.17"= "Large"));earlyweight_grpsize_plot

#ggsave("earlyweight_grpsize_plot.pdf", earlyweight_grpsize_plot, width=3.5, height=3.5)



############## ############## ############## ############## 
   ############# Age First Birth Models ##############
############## ############## ############## ############## 


Repro_ELA<- readRDS("Reproduction_ELA_data.rds") %>%
  mutate(across(c(RankIndex, PrimpIndex, CompetingSibIndex, MomAllLossType), as.factor))


## Model each ELA seperately ##

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "HurricaneYear1", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

for (predictor in predictors) {
  formula <- as.formula(paste("AgeFirstBirthYrs ~", predictor, "+ (1|Birth.Season) + (1|NatalGroup)"))
  model <- lmer(formula, data = Repro_ELA)
  model_name <- paste("AFB", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


fixed_effects <- summary(AFB_RankIndex_model)$coefficients
n <- nobs(AFB_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")

#write.table(results, "AFB_lmer_modelresults_8Apr25.txt", quote=F)

#Make a forest plot with effect sizes from all AFB models
results$Predictor<-factor(results$Predictor, levels=rev(c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "RankIndexLow", "HurricaneYear1", "CumulIndex7")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Group size", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane", "Cumulative adversity")))
results$significance<-'ns'
results$significance[which(results$pvalue<0.05)]<-'p<0.05'
results$significance[which(results$padjust<0.1)]<-'FDR<0.1'
results$significance <- factor(results$significance, levels = c("ns", "p<0.05", "FDR<0.1"))

sigCOLS= c('grey',pal[4],pal[3])

AFB_forest<-ggplot(data= results, aes(x= Predictor, y=beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2], "FDR<0.1"=sigCOLS[3])) +
  xlab("") +ylab("Effect size");AFB_forest

ggsave("AFB_forest.pdf", AFB_forest,  width=5.5, height=3.5) 

#Plot cumulative adversity result
AgeFirstBirth_CI<-effect_plot(AFB_CumulIndex7_model, pred = CumulIndex7, interval = TRUE, plot.points = FALSE, line.colors=pal[3], line.thickness = 1.5) +
  geom_jitter(data= Repro_ELA, aes(y=AgeFirstBirthYrs, x=CumulIndex7), size=0.5, alpha=0.3) +
  ylab("Age at First Birth (years)") +
  xlab("Cumulative ELA") +
  theme_bw(14)+
  ylim(2.5,7.5); AgeFirstBirth_CI
#ggsave("AgeFirstBirth_CI.pdf",AgeFirstBirth_CI, width=3.5, height=3.5) 


#Predict AFB when cumulative adversity is 0 versus 4
new_data <- data.frame(CumulIndex7 = 4, Birth.Season = 1993, NatalGroup="F")  
predict(AFB_CumulIndex7_model, newdata = new_data, re.form = NA) #4.433

new_data <- data.frame(CumulIndex7 = 0, Birth.Season = 1993, NatalGroup="F") 
predict(AFB_CumulIndex7_model, newdata = new_data, re.form = NA) #4.175
#0.258 years later (3 months later)



#Predict AFB for high and low group size 
new_data <- data.frame(s_grpsize = -0.757, Birth.Season = 1993, NatalGroup="F")  
predict(AFB_s_grpsize_model, newdata = new_data, re.form = NA) #4.243

new_data <- data.frame(s_grpsize = 1.908, Birth.Season = 1993, NatalGroup="F") 
predict(AFB_s_grpsize_model, newdata = new_data, re.form = NA) #4.439
#0.196 years later (2.4 months later)


#Predict AFB for maternal loss 
new_data <- data.frame(MomAllLossType = 0, Birth.Season = 1993, NatalGroup="F")
new_data$MomAllLossType<- as.factor(new_data$MomAllLossType)
predict(AFB_MomAllLossType_model, newdata = new_data, re.form = NA) #4.285

new_data <- data.frame(MomAllLossType = 1, Birth.Season = 1993, NatalGroup="F")
new_data$MomAllLossType<- as.factor(new_data$MomAllLossType)
predict(AFB_MomAllLossType_model, newdata = new_data, re.form = NA) #4.353
#0.068 years later (2.4 months later)


#Predict AFB for rank
new_data <- data.frame(RankIndex = 0, Birth.Season = 1993, NatalGroup="F")
new_data$RankIndex<- as.factor(new_data$RankIndex)
predict(AFB_RankIndex_model, newdata = new_data, re.form = NA) #4.228

new_data <- data.frame(RankIndex = 1, Birth.Season = 1993, NatalGroup="F")
new_data$RankIndex<- as.factor(new_data$RankIndex)
predict(AFB_RankIndex_model, newdata = new_data, re.form = NA) #4.387
#0.159 years later (2 months later)


####################################
#### Adult body weight/size ####
####################################

Morph_weights_ELA<- readRDS("Morph_ELA_data.rds") %>%
  mutate(across(c(RankIndex, PrimpIndex, CompetingSibIndex, MomAllLossType), as.factor))
adult_morph<-subset(Morph_weights_ELA, trapped_age > 4) #729


### BODY WEIGHT ###

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "HurricaneYear1", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

aicresults<-data.frame(df = numeric(), 
                       AIC = numeric(),
                       ELA= character())

for (predictor in predictors) {
  formula_quad <- as.formula(paste("log_body_weight ~", predictor, "+ log_age + I(log_age^2) + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model_quad <- lmer(formula, data = adult_morph)
  
  formula_lin <- as.formula(paste("log_body_weight ~", predictor, "+ log_age + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model_lin<-lmer(formula_lin, data = adult_morph)
  aic <- AIC(model_lin, model_quad)
  aic$ELA <- predictor
  aicresults<-rbind(aic, aicresults)
  
  model_name <- paste("weight", predictor, "model", sep="_")
  assign(model_name, model_quad)
  
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


fixed_effects <- summary(weight_RankIndex_model)$coefficients
n <- nobs(weight_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")


#Make a forest plot with all the adult body weight models
results$Predictor<-factor(results$Predictor, levels=rev(c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "RankIndexLow", "HurricaneYear1", "CumulIndex7")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Group size", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane", "Cumulative adversity")))
results$significance<-'ns'
results$significance[which(results$pvalue<0.05)]<-'p<0.05'
results$significance[which(results$padjust<0.1)]<-'FDR<0.1'
results$significance <- factor(results$significance, levels = c("ns", "p<0.05", "FDR<0.1"))

sigCOLS= c('grey',pal[6],pal[5])

Adultweight_forest<-ggplot(data= results, aes(x= Predictor, y=beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2], "FDR<0.1"=sigCOLS[3])) +
  xlab("") +ylab("Effect size");Adultweight_forest
#ggsave("Weight_forest.pdf", Adultweight_forest,  width=5.5, height=3.5) 


#Plot the significant terms
Weight_rank<-effect_plot(weight_RankIndex_model, pred = RankIndex, interval = TRUE, plot.points = FALSE, line.colors=pal[5], points.on.top=FALSE) +
  geom_jitter(data= adult_morph, aes(y=log_body_weight, x=as.factor(RankIndex)), size=0.5, alpha=0.2, width=0.2) +
  ylab("log-transformed body weight") +
  xlab("Matriline rank") +
  theme_bw(14) + 
  scale_x_discrete(limits = c("0", "0.5", "1"), labels=c("High", "Middle", "Low")); Weight_rank 

#ggsave("Weight_rank.pdf", Weight_rank, width=3.5, height=3.5)


#predict body weight for low and high ranking individuals
new_data <- data.frame(RankIndex = 0, s_KinCountGrowthR= -0.011911, s_groupsize_growth= 0.224848, Birth.Season = 1993, NatalGroup="F", log_age=1.009, AnimalID.text= "'01R") 
new_data$RankIndex<-as.factor(new_data$RankIndex)
predict(weight_RankIndex_model, newdata = new_data, re.form = NA, type="response") #0.921183
#log transform 10^0.921183= 8.34kg

new_data <- data.frame(RankIndex = 1, s_KinCountGrowthR= -0.011911, s_groupsize_growth= 0.224848, Birth.Season = 1993, NatalGroup="F", log_age=1.009, AnimalID.text= "'01R") 
new_data$RankIndex<-as.factor(new_data$RankIndex)
predict(weight_RankIndex_model, newdata = new_data, re.form = NA, type="response") #0.889
#7.74 kg

#low ranking is 0.6kg lighter (1.3 pounds)


### ARM LENGTH ###

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex",  "HurricaneYear1", "CumulIndex7")
#Removed hurricane and cumulative index because they failed to converge
#AIC lower for linear models without quadratic term

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

aicresults<-data.frame(df = numeric(), 
                       AIC = numeric(),
                       ELA= character())
for (predictor in predictors) {
  print(predictor)
  

  formula <- as.formula(paste("log_total_arm ~", predictor, "+ log_age + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model<-lmer(formula, data = adult_morph)
  
  model_name <- paste("arm", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


fixed_effects <- summary(arm_RankIndex_model)$coefficients
n <- nobs(arm_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")
#write.table(results, "arm_lmer_modelresults_29Jul25.txt", quote=F)




### LEG LENGTH ###

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "HurricaneYear1", "CumulIndex7")
#Primp Index failed to converge

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

aicresults<-data.frame(df = numeric(), 
                       AIC = numeric(),
                       ELA= character())

for (predictor in predictors) {
  print(predictor)
  
  formula <- as.formula(paste("log_total_leg ~", predictor, "+ log_age + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model <- lmer(formula, data = adult_morph)
  
  model_name <- paste("leg", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


fixed_effects <- summary(leg_RankIndex_model)$coefficients
n <- nobs(leg_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")

#write.table(results, "leg_lmer_modelresults_29Jul25.txt", quote=F)



### TRUNK LENGTH ###

adult_morph$HurricaneYear1 <- as.factor(adult_morph$HurricaneYear1)

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex",  "HurricaneYear1", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

aicresults<-data.frame(df = numeric(), 
                       AIC = numeric(),
                       ELA= character())

for (predictor in predictors) {
  print(predictor)
  formula_quad <- as.formula(paste("log_trunk_length ~", predictor, "+ log_age + I(log_age^2) + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model_quad <- lmer(formula_quad, data = adult_morph)
  
  formula_lin <- as.formula(paste("log_trunk_length ~", predictor, "+ log_age + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model_lin <- lmer(formula_lin, data = adult_morph)
  
  aic <- AIC(model_lin, model_quad)
  aic$ELA <- predictor
  aicresults<-rbind(aic, aicresults)
  
  
  model_name <- paste("trunk", predictor, "model", sep="_")
  assign(model_name, model_quad)
  fixed_effects <- summary(model_quad)$coefficients
  n <- nobs(model_quad)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 5]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


fixed_effects <- summary(trunk_RankIndex_model)$coefficients
n <- nobs(trunk_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 5]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")

#write.table(results, "trunk_lmer_modelresults_29Jul25.txt", quote=F)


####################################
   #### Reproductive rate ####
####################################

Repro_ELA<-readRDS("Reproduction_ELA_data.rds") %>%
  mutate(across(c(RankIndex, PrimpIndex, CompetingSibIndex, MomAllLossType), as.factor))

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "HurricaneYear1", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

for (predictor in predictors) {
  
  
  formula <- as.formula(paste("cbind(NumberBirths, SeasonSpan - NumberBirths) ~", predictor, " + s_mean_ibi_group + s_mean_ibi_kin + (1|Birth.Season) + (1|NatalGroup)"))
  model <- glmer(formula, data = Repro_ELA, family = binomial(link = "logit"))
  model_name <- paste("Birthskip", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[2, 1]     
  se <- fixed_effects[2, 2]        
  pvalue <- fixed_effects[2, 4]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}

fixed_effects <- summary(Birthskip_RankIndex_model)$coefficients
n <- nobs(Birthskip_RankIndex_model)
beta <- fixed_effects[3, 1]     
se <- fixed_effects[3, 2]        
pvalue <- fixed_effects[3, 4]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

results$padjust<-p.adjust(results$pvalue, method="BH")
#write.table(results, "Birthskip_glmer_modelresults_8Apr25.txt", quote=F, row.names=F)


#Make a forest plot with all the reproductive rate models
results$Predictor<-factor(results$Predictor, levels=rev(c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "RankIndexLow", "HurricaneYear1", "CumulIndex7")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Group size", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane", "Cumulative adversity")))
results$significance<-'ns'
results$significance[which(results$pvalue<0.05)]<-'p<0.05'
results$significance[which(results$padjust<0.1)]<-'FDR<0.1'
results$significance <- factor(results$significance, levels = c("ns", "p<0.05", "FDR<0.1"))

sigCOLS= c('grey',pal[4],pal[3])

Birthskip_forest<-ggplot(data= results, aes(x= Predictor, y=beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  labs(color = "") +
  theme(legend.position="top", axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2], "FDR<0.1"=sigCOLS[3])) +
  xlab("") +ylab("Effect size");Birthskip_forest

#ggsave("Birthskip_forest.pdf", Birthskip_forest, width=5.5, height=3.5) 

#plot group size results
Birthskip_grpsize<-effect_plot(Birthskip_s_grpsize_model, pred = s_grpsize, interval = TRUE, plot.points = FALSE, line.colors=pal[3], line.thickness = 1.5) +
  geom_jitter(data= Repro_ELA, aes(y=NumberBirths/SeasonSpan, x=s_grpsize), size=0.5, alpha=0.3) +
  ylab("Proportion of years giving birth") +
  xlab("Early life group size (std)") +
  theme_bw(14); Birthskip_grpsize

#ggsave("ProbBirth_grpsize.pdf",Birthskip_grpsize,  width=3.5, height=3.5) 
