
##########################
####### Trade-offs #######
##########################

library(interactions)
library(jtools)
library(ggthemes)


Subadult_repro_growth<- readRDS("Subadult_repro_growth.rds")

# Does residual body weight predict age at first birth
bodysize_AFB<-lmer(AgeFirstBirthYrs ~ residual_size + (1|Birth.Season) + (1|NatalGroup), data=Subadult_repro_growth)
summary(bodysize_AFB)
bodysize_AFB_results<-summary(bodysize_AFB) 
bodysize_AFB_df<-bodysize_AFB_results$coefficients
#write.csv(bodysize_AFB_df, "bodysize_AFB_results.csv")

#plot negative relationship
bodysize_AFB_plot<-effect_plot(bodysize_AFB, pred = residual_size, interval = TRUE, plot.points = TRUE, line.colors=pal[1], point.size=0.5) +
  labs(y="Age at first birth (years)", x="Sub-adult residual body weight", color="")+
  theme_bw(14); bodysize_AFB_plot
#ggsave("bodysize_AFB_plot.pdf", bodysize_AFB_plot, height=3.5, width=3.5)


## Is this moderated by ELA??? #Can't test hurricane, no one experienced a hurricane
predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

for (predictor in predictors) {
  formula <- as.formula(paste("AgeFirstBirthYrs ~", predictor, "*residual_size + (1|Birth.Season) + (1|NatalGroup)"))
  model <- lmer(formula, data = Subadult_repro_growth)
  model_name <- paste("ReproGrowthInt", predictor, "model", sep="_")
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


predictor="RankIndex"
formula <- as.formula(paste("AgeFirstBirthYrs ~", predictor, "*residual_size + (1|Birth.Season) + (1|NatalGroup)"))
model <- lmer(formula, data = Subadult_repro_growth)
model_name <- paste("ReproGrowthInt", predictor, "model", sep="_")
assign(model_name, model)
fixed_effects <- summary(model)$coefficients
n <- nobs(model)
beta <- fixed_effects[5, 1]     
se <- fixed_effects[5, 2]        
pvalue <- fixed_effects[5, 5]
predictor <- "RankIndexMid"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

fixed_effects <- summary(ReproGrowthInt_RankIndex_model)$coefficients
n <- nobs(ReproGrowthInt_RankIndex_model)
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
#write.table(results, "ReproGrowthInt_modelresults_22Apr25.txt", quote=F)

#plot group size results
groupsize_inter_plot<-interact_plot(ReproGrowthInt_s_grpsize_model, data= Subadult_repro_growth, pred = residual_size, modx= s_grpsize, interval = TRUE, plot.points = TRUE, line.colors="blue", point.size=0.5) +
  theme_bw(); groupsize_inter_plot

#Plot as binned 
# calculate mean +/- 1 SD for size
mean_weight <- mean(Subadult_repro_growth$residual_size, na.rm=T)
sd_weight <- sd(Subadult_repro_growth$residual_size, na.rm=T)
weight_values <- c(mean_weight + (1*sd_weight), mean_weight, mean_weight - (1*sd_weight))


# calculate mean +/- 1 SD for group size
mean_groupsize <- mean(Subadult_repro_growth$s_grpsize, na.rm=T)
sd_groupsize <- sd(Subadult_repro_growth$s_grpsize, na.rm=T)
groupsize_values <- c(mean_groupsize + (1*sd_groupsize), mean_groupsize, mean_groupsize - (1*sd_groupsize))

# create a grid of body weight and group size values
new_data <- expand.grid(
  s_grpsize = groupsize_values,
  residual_size = weight_values
)

# add age first birth predictions
pred <- predict(ReproGrowthInt_s_grpsize_model, newdata = new_data, re.form = NA, type="response", se.fit=TRUE)

new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit
new_data$lower <- pred$fit - pred$se.fit
new_data$upper <- pred$fit + pred$se.fit

# add labels for the binned body weight and group size values
new_data$binsize <- factor(
  new_data$residual_size,
  labels = c("small", "average", "large")
)

new_data$binGS <- factor(
  new_data$s_grpsize,
  labels = c("small group", "average", "large group")
)

#filter data to plot
real_data <- Subadult_repro_growth %>%
  filter(is.na(Birth.Season) == F & is.na(NatalGroup) == F & is.na(s_grpsize) == F) %>%
  mutate(binGS = ifelse(s_grpsize >= mean_groupsize + sd_groupsize, "large group", ifelse(s_grpsize <= mean_groupsize - sd_groupsize,  "small group", "average")))
real_data <- real_data %>%
  mutate(binsize = ifelse(residual_size >= mean_weight + sd_weight, "large", ifelse(residual_size <= mean_weight + sd_weight, "small", "average"))) %>%
  filter(binGS == "large group" | binGS == "small group") %>%
    filter(binsize == "large" | binsize == "small")
real_data$binsize <- factor(
  real_data$binsize,
  levels = c("small", "large"))
real_data$binGS <- factor(
  real_data$binGS,
  levels = c("large group", "small group"))

new_data<-new_data %>%
  filter(binGS == "small group" | binGS == "large group") %>%
  filter(binsize == "small" | binsize == "large")

Groupsize_interaction<-ggplot(data=new_data, aes(y=fit, x=binsize, color=binsize)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), size=1.5,  width=0.3, position = position_dodge(width = 0.75))+
  geom_point(size=2, position = position_dodge(width = 0.75)) +
  geom_jitter(data=real_data, aes(y=AgeFirstBirthYrs, x=binsize), color="black", width=0.2, height=0.2, size=0.3) +
  facet_wrap(~binGS) +
  theme_minimal()+
  ggtitle("Group size") +
  theme_bw(14) +
  theme(plot.title= element_text(hjust=0.5), strip.text=element_text(size=12, face="bold"), legend.position="none") +
  scale_color_manual(values=c(pal[2], "#336a35")) +
  labs(y="Age at first birth (years)", x="Sub-adult residual body weight", color=""); Groupsize_interaction

ggsave("Groupsize_interaction.pdf", Groupsize_interaction, width=3.5, height=4)


#Relationship between adult body size and reproductive rate

Repro_growth_morethan4<- readRDS("Adult_repro_growth.rds")

# are proportion of seasons giving birth and residual body weight associated across everyone?

model <- glmer(cbind(NumberBirths, SeasonSpan - NumberBirths) ~ residual_size + s_group_size_DOM + s_KinCountGrowthR + (1|Birth.Season) + (1|NatalGroup), data = Repro_growth_morethan4, family = binomial(link = "logit"))
summary(model)
bodysize_propbirth_results<-summary(model) 
bodysize_propbirth_df<-bodysize_propbirth_results$coefficients
#write.csv(bodysize_propbirth_df, "bodysize_propbirth_results.csv")

#plot
bodysize_PropBirth_plot<-effect_plot(model, pred = residual_size, interval = TRUE, plot.points = FALSE, line.colors=pal[1]) +
  geom_jitter(data= Repro_growth_morethan4, aes(y=NumberBirths/SeasonSpan, x=residual_size), size=0.4, alpha=1) +
  ylab("Proportion of years giving birth") +
  xlab("Adult residual body weight") +
  theme_bw(14); bodysize_PropBirth_plot
#ggsave("bodysize_PropBirth_plot.pdf", bodysize_PropBirth_plot, height=3.5, width=3.5)



# Is this moderated by ELA??? #Can't test hurricane, no one experienced a hurricane

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "CumulIndex7")

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

for (predictor in predictors) {
  formula <- as.formula(paste("cbind(NumberBirths, SeasonSpan - NumberBirths) ~ ", predictor, "*residual_size + (1|Birth.Season) + (1|NatalGroup)"))
  model <- glmer(formula, data = Repro_growth_morethan4, family = binomial(link = "logit"))
  model_name <- paste("ReproAdultweightInt", predictor, "model", sep="_")
  assign(model_name, model)
  fixed_effects <- summary(model)$coefficients
  n <- nobs(model)
  beta <- fixed_effects[4, 1]     
  se <- fixed_effects[4, 2]        
  pvalue <- fixed_effects[4, 4]   
  
  results <- rbind(results, data.frame(Predictor = predictor,
                                       n = n,
                                       beta = beta,
                                       SE = se,
                                       pvalue = pvalue))
}


predictor="RankIndex"
formula <- as.formula(paste("cbind(NumberBirths, SeasonSpan - NumberBirths) ~ ", predictor, "*residual_size + (1|Birth.Season) + (1|NatalGroup)"))
model <- glmer(formula, data = Repro_growth_morethan4, family = binomial(link = "logit"))
model_name <- paste("ReproAdultweightInt", predictor, "model", sep="_")
assign(model_name, model)
fixed_effects <- summary(model)$coefficients
n <- nobs(model)
beta <- fixed_effects[5, 1]     
se <- fixed_effects[5, 2]        
pvalue <- fixed_effects[5, 4]
predictor <- "RankIndexMid"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))

fixed_effects <- summary(ReproAdultweightInt_RankIndex_model)$coefficients
n <- nobs(ReproGrowthInt_RankIndex_model)
beta <- fixed_effects[6, 1]     
se <- fixed_effects[6, 2]        
pvalue <- fixed_effects[6, 4]
predictor <- "RankIndexLow"

results <- rbind(results, data.frame(Predictor = predictor,
                                     n = n,
                                     beta = beta,
                                     SE = se,
                                     pvalue = pvalue))


results$padjust<-p.adjust(results$pvalue, method="BH")

#write.table(results, "ReproAdultweightInt_modelresults_22Apr25.txt", quote=F)


sib_adult_inter_plot<-interact_plot(ReproAdultweightInt_CompetingSibIndex_model, data= Repro_growth_morethan4, pred = residual_size, modx= CompetingSibIndex, interval = TRUE, plot.points = FALSE, line.colors="blue", point.size=0.5); sib_adult_inter_plot

ml_adult_inter_plot<-interact_plot(ReproAdultweightInt_MomAllLossType_model, data= Repro_growth_morethan4, pred = residual_size, modx= MomAllLossType, interval = TRUE, plot.points = FALSE, line.colors="blue", point.size=0.5); ml_adult_inter_plot

primp_adult_inter_plot<-interact_plot(ReproAdultweightInt_PrimpIndex_model, data= Repro_growth_morethan4, pred = residual_size, modx= PrimpIndex, interval = TRUE, plot.points = FALSE, line.colors="blue", point.size=0.5); primp_adult_inter_plot


#Plot Competing sibling results as binned

### Model predictions
# calculate mean +/- 1 SD for body weight
mean_weight <- mean(Repro_growth_morethan4$residual_size, na.rm=T)
sd_weight <- sd(Repro_growth_morethan4$residual_size, na.rm=T)
weight_values <- c(mean_weight + (1*sd_weight), mean_weight, mean_weight - (1*sd_weight))
#gives residual size values for big medium small


# Create a grid of mean, +/- 1 SD for size ELA 0 or 1
new_data <- expand.grid(
  CompetingSibIndex = c(0,1),
  residual_size = weight_values)

# Add predictions
pred <- predict(ReproAdultweightInt_CompetingSibIndex_model, newdata = new_data, re.form = NA, type="response", se.fit=TRUE)
new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit
new_data$lower <- pred$fit - pred$se.fit
new_data$upper <- pred$fit + pred$se.fit

# Add labels for the binned body weight values
new_data$binsize <- factor(
  new_data$residual_size,
  labels = c("small", "average", "large")
)

real_data <- Repro_growth_morethan4 %>%
  filter(is.na(Birth.Season) == F & is.na(NatalGroup) == F & is.na(s_grpsize) == F)
real_data <- real_data %>%
  mutate(binsize = ifelse(residual_size >= mean_weight + sd_weight, "large", ifelse(residual_size <= mean_weight - sd_weight, "small", "average"))) %>%
  filter(binsize == "large" | binsize == "small")
real_data$binsize <- factor(
  real_data$binsize,
  levels = c("small", "large"))

new_data<-new_data %>%
  filter(binsize == "small" | binsize == "large")

custom_labels <- c("0" = "No", "1" = "Yes")

SibAdult_interaction<-ggplot(data=new_data, aes(y=fit, x=binsize, color=binsize)) +
  geom_errorbar(data=new_data, aes(ymin=lower, ymax=upper), size=1.5,  width=0.3, position = position_dodge(width = 0.75))+
  geom_point(data=new_data, aes(y=fit, x=binsize, color=binsize), size=3, position = position_dodge(width = 0.75)) +
  geom_jitter(data=real_data, aes(y=NumberBirths/SeasonSpan, x=binsize), color="black", width=0.2, height=0.01, size=0.3) +
  facet_wrap(~CompetingSibIndex, labeller=labeller(CompetingSibIndex = custom_labels)) +
  theme_minimal()+
  ggtitle("Competing Sibling") +
  theme_bw(14) +
  theme(plot.title= element_text(hjust=0.5), strip.text=element_text(size=12, face="bold"), legend.position="none") +
  scale_color_manual(values=c(pal[2], "#336a35")) +
  labs(y="Proportion of years giving birth", x="Adult residual body weight", color=""); SibAdult_interaction

ggsave("Sib_adult_interaction.pdf", SibAdult_interaction, width=3.5, height=4)




#Plot Maternal loss results as binned

# calculate mean +/- 1 SD for size
mean_weight <- mean(Repro_growth_morethan4$residual_size, na.rm=T)
sd_weight <- sd(Repro_growth_morethan4$residual_size, na.rm=T)
weight_values <- c(mean_weight + (1*sd_weight), mean_weight, mean_weight - (1*sd_weight))
#gives residual size values for big medium small


#  a grid of mean, +/- 1 SD for size ELA 0 or 1
new_data <- expand.grid(
  MomAllLossType = c(0,1),
  residual_size = weight_values)

# Add predictions
pred <- predict(ReproAdultweightInt_MomAllLossType_model, newdata = new_data, re.form = NA, type="response", se.fit=TRUE)

new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit
new_data$lower <- pred$fit - pred$se.fit
new_data$upper <- pred$fit + pred$se.fit

# Add labels for the binned body weightvalues
new_data$binsize <- factor(
  new_data$residual_size,
  labels = c("small", "average", "large")
)

real_data <- Repro_growth_morethan4 %>%
  filter(is.na(Birth.Season) == F & is.na(NatalGroup) == F & is.na(s_grpsize) == F)
real_data <- real_data %>%
  mutate(binsize = ifelse(residual_size >= mean_weight + sd_weight, "large", ifelse(residual_size <= mean_weight - sd_weight, "small", "average"))) %>%
  filter(binsize=="large" | binsize == "small")
real_data$binsize <- factor(
  real_data$binsize,
  levels = c("small", "large"))

new_data<-new_data %>%
  filter(binsize == "small" | binsize == "large")

custom_labels <- c("0" = "No", "1" = "Yes")

MLAdult_interaction<-ggplot(data=new_data, aes(y=fit, x=binsize, color=binsize)) +
  geom_errorbar(data=new_data, aes(ymin=lower, ymax=upper), size=1.5,  width=0.3, position = position_dodge(width = 0.75))+
  geom_point(data=new_data, aes(y=fit, x=binsize, color=binsize), size=3, position = position_dodge(width = 0.75)) +
  geom_jitter(data=real_data, aes(y=NumberBirths/SeasonSpan, x=binsize), color="black", width=0.2, height=0.01, size=0.3) +
  facet_wrap(~MomAllLossType, labeller=labeller(MomAllLossType=custom_labels)) +
  theme_minimal()+
  ggtitle("Maternal Loss") +
  theme_bw(14) +
  theme(plot.title= element_text(hjust=0.5), strip.text=element_text(size=12, face="bold"), legend.position="none") +
  scale_color_manual(values=c(pal[2], "#336a35")) +
  labs(y="Proportion of years giving birth", x="Adult residual body weight", color=""); MLAdult_interaction

ggsave("ML_adult_interaction.pdf", MLAdult_interaction, width=3.5, height=4)

