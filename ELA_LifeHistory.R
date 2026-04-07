

######## Section 1: How does ELA influence life history ############

library(ggthemes)
library(tidyverse)
library(lmerTest)
library(nlme)
library(jtools)

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
  
  model <- lmer(formula, data = Lessthan4)
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

model <- lmer(body_weight_kg ~ RankIndex * trapped_age + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = Lessthan4)
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


#Make a forest plot with all the subadult body weight linear interaction models

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


#plot competing sib results using age categories
Lessthan4$age_group <- cut(Lessthan4$trapped_age,
                           breaks = c(0, 1, 2, Inf),
                           labels = c("0-1", "1-2", "2+"),
                           right = FALSE) 

newdata <- expand.grid(
  CompetingSibIndex = factor(c(0, 1), levels = c(0, 1)),
  age_group = c("0-1", "1-2", "2+")
)

rep_vals <- c(0.5, 1.5, 2.5)
newdata$trapped_age <- rep(rep_vals, each = 2)

preds <- predict(earlyweight_CompetingSibIndex_model, newdata = newdata, re.form = NA, se.fit = TRUE)

newdata$fit<-preds$fit
newdata$lower <- preds$fit - 1.96 * preds$se.fit
newdata$upper <- preds$fit + 1.96 * preds$se.fit

newdata$CompetingSibIndex <-as.factor(newdata$CompetingSibIndex)

earlyweight_sib_plot<- ggplot(newdata, aes(x = as.factor(age_group), y = fit, color = as.factor(CompetingSibIndex))) +
  geom_point(data = Lessthan4, mapping= aes(x = as.factor(age_group), y = body_weight_kg, group=as.factor(CompetingSibIndex)),
             color = "black",
             size = 0.5,
             alpha = 0.1,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.80),
             inherit.aes = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper, group=CompetingSibIndex), linewidth=1.2,  width=0.6, position = position_dodge(width = 0.8)) +
  geom_point(size=2, position = position_dodge(width = 0.8)) +
  geom_point(size=2, position = position_dodge(width = 0.75)) +
  labs(
    x = "Age (years)",
    y = "Body weight (kg)",
    color = "Competing Sibling"
  ) +
  theme_bw(15) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=c(pal[6], pal[5]), labels=c("0"="No", "1"= "Yes")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE));earlyweight_sib_plot

### Additive model with a gompertz function model
predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "CumulIndex7")

results_slope <- data.frame(Predictor = character(),
                            n = integer(),
                            beta = numeric(),
                            SE = numeric(),
                            pvalue = numeric())


results_asymp <- data.frame(Predictor = character(),
                            n = integer(),
                            beta = numeric(),
                            SE = numeric(),
                            pvalue = numeric())

Lessthan4$AnimalID.text   <- factor(Lessthan4$AnimalID.text)
Lessthan4$Birth.Season    <- factor(Lessthan4$Birth.Season)
Lessthan4$NatalGroup     <- factor(Lessthan4$NatalGroup)
Lessthan4$trapped_age     <- as.numeric(Lessthan4$trapped_age)
Lessthan4$body_weight_kg  <- as.numeric(Lessthan4$body_weight_kg)

# Fit a simple nls() to get starting values
nls_start <- nls(
  body_weight_kg ~ SSgompertz(trapped_age, Asym, b2, b3),
  data = Lessthan4
)
start_vals <- coef(nls_start)
print(start_vals) 

#run gompertz model
for (predictor in predictors) {
  # Ensure correct variable types
  if (predictor %in% c("s_grpsize", "s_kinBirthR", "CumulIndex7")) {
    Lessthan4$predictorvar  <- as.numeric(as.character(Lessthan4[[predictor]]))  # continuous predictor → numeric
  } else {
    Lessthan4$predictorvar  <- factor(Lessthan4[[predictor]]) # binary predictor → factor
  }   
  
  # Drop rows with NAs (nlme doesn't handle them well)
  model_data <- Lessthan4[complete.cases(Lessthan4[,c("body_weight_kg", "trapped_age",
                                                      "predictorvar", "AnimalID.text",
                                                      "Birth.Season", "NatalGroup")]), ]
  
  # Define the Gompertz function 
  gompertz_fun <- function(trapped_age, Asym, b2, b3) {
    Asym * exp(-b2 * b3^trapped_age)
  }
  
  # Determine number of extra start values needed
  n_levels <- ifelse(
    predictor %in% c("s_grpsize", "s_kinBirthR", "CumulIndex7"),
    1,                                        # continuous: just 1 slope
    nlevels(Lessthan4$predictorvar) - 1       # factor: n levels - 1 (reference dropped)
  )
  
  fit_gompertz_both <- tryCatch( nlme(
    body_weight_kg ~ gompertz_fun(trapped_age, Asym, b2, b3),
    data = model_data,
    
    # Fixed effects ─────────────────────────────────────────────────────────────
    # Both Asym and b3 vary by ELA
    fixed = list(
      Asym ~ predictorvar,
      b2   ~ 1,
      b3   ~ predictorvar
    ),
    
    # Random effects (unchanged) ─────────────────────────────────────────────────
    random = list(
      NatalGroup   = pdDiag(Asym ~ 1),
      Birth.Season  = pdDiag(Asym ~ 1),
      AnimalID.text = pdDiag(Asym ~ 1)
    ),
    
    groups = ~ NatalGroup / Birth.Season / AnimalID.text,
    
    # Starting values ─────────────────────────────────────────────────────────────
    # Asym: 2 values (intercept + ELA shift)
    # b2:   1 value
    # b3:   2 values (intercept + ELA shift)
    
    start = list(
      fixed = c(
        Asym = c(start_vals["Asym"], rep(0, n_levels)),
        b2   = start_vals["b2"],
        b3   = c(start_vals["b3"], rep(0, n_levels))
      )
    ),
    
    control = nlmeControl(
      maxIter      = 300,
      pnlsMaxIter  = 30,
      msMaxIter    = 300,
      tolerance    = 1e-4,
      returnObject = TRUE
    )
  ),
  error = function(e) {
    message("Model failed for predictor: ", predictor, "\n", e)
    return(NULL)  # return NULL instead of crashing
  }
  )
  
  if (!is.null(fit_gompertz_both)) {
    # extract estimates
    
    s <- summary(fit_gompertz_both)
    coef_table <- s$tTable
    ints <- intervals(fit_gompertz_both, which = "fixed")$fixed
    
    model_name <- paste("earlyweight", predictor, "Gompertz_model", sep="_")
    assign(model_name, fit_gompertz_both)
    
    if (predictor == "RankIndex") {
      
      results_slope <- rbind(results_slope, data.frame(Predictor = "RankIndex0.5",
                                                       n = nobs(fit_gompertz_both),
                                                       beta = coef_table["b3.predictorvar0.5", "Value"],
                                                       SE = coef_table["b3.predictorvar0.5", "Std.Error"],
                                                       conf.low = ints["b3.predictorvar0.5", "lower"],
                                                       conf.high= ints["b3.predictorvar0.5", "upper"],
                                                       pvalue = coef_table["b3.predictorvar0.5", "p-value"]))
      results_asymp<- rbind(results_asymp, data.frame(Predictor = "RankIndex0.5",
                                                      n = nobs(fit_gompertz_both),
                                                      beta = coef_table["Asym.predictorvar0.5", "Value"],
                                                      SE = coef_table["Asym.predictorvar0.5", "Std.Error"],
                                                      conf.low = ints["Asym.predictorvar0.5", "lower"],
                                                      conf.high= ints["Asym.predictorvar0.5", "upper"],
                                                      pvalue = coef_table["Asym.predictorvar0.5", "p-value"]))
      
      results_slope <- rbind(results_slope, data.frame(Predictor = "RankIndex1",
                                                       n = nobs(fit_gompertz_both),
                                                       beta = coef_table["b3.predictorvar1", "Value"],
                                                       SE = coef_table["b3.predictorvar1", "Std.Error"],
                                                       conf.low = ints["b3.predictorvar1", "lower"],
                                                       conf.high= ints["b3.predictorvar1", "upper"],
                                                       pvalue = coef_table["b3.predictorvar1", "p-value"]))
      results_asymp<- rbind(results_asymp, data.frame(Predictor = "RankIndex1",
                                                      n = nobs(fit_gompertz_both),
                                                      beta = coef_table["Asym.predictorvar1", "Value"],
                                                      SE = coef_table["Asym.predictorvar1", "Std.Error"],
                                                      conf.low = ints["Asym.predictorvar1", "lower"],
                                                      conf.high= ints["Asym.predictorvar1", "upper"],
                                                      pvalue = coef_table["Asym.predictorvar1", "p-value"]))
    }else{
      results_slope <- rbind(results_slope, data.frame(Predictor = predictor,
                                                       n = nobs(fit_gompertz_both),
                                                       beta = coef_table[5, "Value"],
                                                       SE = coef_table[5, "Std.Error"],
                                                       conf.low = ints[5, "lower"],
                                                       conf.high= ints[5, "upper"],
                                                       pvalue = coef_table[5, "p-value"]))
      results_asymp<- rbind(results_asymp, data.frame(Predictor = predictor,
                                                      n = nobs(fit_gompertz_both),
                                                      beta = coef_table[2, "Value"],
                                                      SE = coef_table[5, "Std.Error"],
                                                      conf.low = ints[2, "lower"],
                                                      conf.high= ints[2, "upper"],
                                                      pvalue = coef_table[2, "p-value"]))
    }
    
  }  
}

results_slope$padjust<-p.adjust(results_slope$pvalue, method="BH")
results_asymp$padjust<-p.adjust(results_asymp$pvalue, method="BH")

# plot results for competing sibling
# prediction grid
age_grid <- seq(min(Lessthan4$trapped_age, na.rm=TRUE), max(Lessthan4$trapped_age, na.rm=TRUE), length.out = 200)
newdata <- expand.grid(trapped_age = age_grid, CompetingSibIndex = c(0,1))

# Calculate Asym and b3 for each group
fe <- fixef(earlyweight_CompetingSibIndex_Gompertz_model)

newdata$pred <- ifelse(
  newdata$CompetingSibIndex == 0,
  # Group 0: intercepts only
  gompertz_fun(newdata$trapped_age,
               Asym = fe["Asym.(Intercept)"],
               b2   = fe["b2"],
               b3   = fe["b3.(Intercept)"]),
  # Group 1: intercept + shift
  gompertz_fun(newdata$trapped_age,
               Asym = fe["Asym.(Intercept)"] + fe["Asym.predictorvar1"],
               b2   = fe["b2"],
               b3   = fe["b3.(Intercept)"]   + fe["b3.predictorvar1"])
)

# label for plotting
newdata$exposed <- factor(newdata$CompetingSibIndex, levels = c(0,1), labels = c("Unexposed","Exposed"))

earlyweight_gompertz_sib<-ggplot(Lessthan4, aes(x = trapped_age, y = body_weight_kg, color = factor(CompetingSibIndex))) +
  geom_jitter(aes(alpha=CompetingSibIndex), width = 0.1, height = 0, size = 0.5) +
  geom_line(data = newdata, aes(x = trapped_age, y = pred, color = factor(CompetingSibIndex)), size = 1.1) +
  scale_color_manual(name = "Competing sibling", values = c("0" = "black", "1" = pal[5]), labels = c("No","Yes")) +
  scale_alpha_manual(name = "Competing sibling", values = c("0" = 0.2, "1" = 1), labels = c("No","Yes")) +
  theme_bw(15) +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Body weight (kg)"); earlyweight_gompertz_sib


# Run all ELAs in one linear interaction model:
model <- lmer(body_weight_kg ~ MomAllLossType*trapped_age + PrimpIndex*trapped_age + CompetingSibIndex*trapped_age + s_kinBirthR*trapped_age + RankIndex*trapped_age + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = Lessthan4)
fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")

fixed_effects<-fixed_effects[-c(1:8),]
fixed_effects$n <- nobs(model)
fixed_effects$Predictor <- rownames(fixed_effects)
fixed_effects$significance<-'ns'
fixed_effects$significance[which(fixed_effects$pvalue<0.05)]<-'p<0.05'

fixed_effects$Predictor<-factor(fixed_effects$Predictor, levels=rev(c("MomAllLossType1:trapped_age", "trapped_age:PrimpIndex1", "trapped_age:CompetingSibIndex1", "trapped_age:s_kinBirthR", "trapped_age:RankIndex0.5", "trapped_age:RankIndex1", "HurricaneYear1")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane")))

empty_row<- c(NA, NA, NA, NA, NA, NA, "Hurricane", NA)
fixed_effects<- rbind(fixed_effects, empty_row)
fixed_effects$SE<-as.numeric(fixed_effects$SE)
fixed_effects$beta<-as.numeric(fixed_effects$beta)

sigCOLS= c('grey',pal[6])

Earlygrow_forest_1mod<-ggplot(data= fixed_effects, aes(x= Predictor, y= beta, color=significance)) +
  geom_point(size=2, na.rm=T) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1, na.rm=T) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2]), na.value = NA, na.translate = FALSE) +
  xlab("") +ylab("Effect size");Earlygrow_forest_1mod

#Run cumulative adversity model without group size
Lessthan4$CumulIndex6_nogs <- as.numeric(Lessthan4$MomAllLossType) + as.numeric(Lessthan4$PrimpIndex) + as.numeric(Lessthan4$CompetingSibIndex) + Lessthan4$s_kinBirthR + as.numeric(Lessthan4$RankIndex) + as.numeric(Lessthan4$HurricaneYear1)
model <- lmer(body_weight_kg ~ CumulIndex6_nogs*trapped_age + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = Lessthan4)
fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")
fixed_effects$n <- nobs(model)



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

#Plot cumulative adversity result
AgeFirstBirth_CI<-effect_plot(AFB_CumulIndex7_model, pred = CumulIndex7, interval = TRUE, plot.points = FALSE, line.colors=pal[3], line.thickness = 1.5) +
  geom_jitter(data= Repro_ELA, aes(y=AgeFirstBirthYrs, x=CumulIndex7), size=0.5, alpha=0.3) +
  ylab("Age at First Birth (years)") +
  xlab("Cumulative ELA") +
  theme_bw(14)+
  ylim(2.5,7.5); AgeFirstBirth_CI


#### Run with all ELAs in a single model
model <- lmer(AgeFirstBirthYrs ~ MomAllLossType + PrimpIndex + CompetingSibIndex + s_kinBirthR + RankIndex + HurricaneYear1 + (1|Birth.Season) + (1|NatalGroup), data = Repro_ELA)

fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")
fixed_effects<-fixed_effects[-1,]
fixed_effects$n <- nobs(model)
fixed_effects$Predictor <- rownames(fixed_effects)


fixed_effects$Predictor<-factor(fixed_effects$Predictor, levels=rev(c("MomAllLossType1", "PrimpIndex1", "CompetingSibIndex1", "s_kinBirthR", "RankIndex0.5", "RankIndex1", "HurricaneYear1")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane")))
fixed_effects$significance<-'ns'
fixed_effects$significance[which(fixed_effects$pvalue<0.05)]<-'p<0.05'

sigCOLS= c('grey',pal[4])

AFB_forest_1mod<-ggplot(data= fixed_effects, aes(x= Predictor, y= beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2])) +
  xlab("") +ylab("Effect size");AFB_forest_1mod

#Run cumulative adversity model without group size
Repro_ELA$CumulIndex6_nogs <- as.numeric(Repro_ELA$MomAllLossType) + as.numeric(Repro_ELA$PrimpIndex) + as.numeric(Repro_ELA$CompetingSibIndex) + Repro_ELA$s_kinBirthR + as.numeric(Repro_ELA$RankIndex) + as.numeric(Repro_ELA$HurricaneYear1)
model <- lmer(AgeFirstBirthYrs ~ CumulIndex6_nogs + (1|Birth.Season) + (1|NatalGroup), data = Repro_ELA)
fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")
fixed_effects$n <- nobs(model)


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


for (predictor in predictors) {
  formula_quad <- as.formula(paste("log_body_weight ~", predictor, "+ log_age + I(log_age^2) + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup)"))
  model_quad <- lmer(formula_quad, data = adult_morph)
  
  model_name <- paste("weight", predictor, "model", sep="_")
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

#Plot the effect of matriline rank
Weight_rank<-effect_plot(weight_RankIndex_model, pred = RankIndex, interval = TRUE, plot.points = FALSE, line.colors=pal[5], points.on.top=FALSE) +
  geom_jitter(data= adult_morph, aes(y=log_body_weight, x=as.factor(RankIndex)), size=0.5, alpha=0.2, width=0.2) +
  ylab("log-transformed body weight") +
  xlab("Matriline rank") +
  theme_bw(14) + 
  scale_x_discrete(limits = c("0", "0.5", "1"), labels=c("High", "Middle", "Low")); Weight_rank 

# Run all ELAs in one model:
model <- lmer(body_weight_kg ~ MomAllLossType + PrimpIndex + CompetingSibIndex + s_kinBirthR + RankIndex + HurricaneYear1 + log_age + I(log_age^2) + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = adult_morph)
fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")

fixed_effects<-fixed_effects[c(2:8),]
fixed_effects$n <- nobs(model)
fixed_effects$Predictor <- rownames(fixed_effects)
fixed_effects$significance<-'ns'
fixed_effects$significance[which(fixed_effects$pvalue<0.05)]<-'p<0.05'

fixed_effects$Predictor<-factor(fixed_effects$Predictor, levels=rev(c("MomAllLossType1", "PrimpIndex1", "CompetingSibIndex1", "s_kinBirthR", "RankIndex0.5", "RankIndex1", "HurricaneYear1")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane")))

sigCOLS= c('grey',pal[6])

Weight_forest_1mod<-ggplot(data= fixed_effects, aes(x= Predictor, y= beta, color=significance)) +
  geom_point(size=2, na.rm=T) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1, na.rm=T) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2]), na.value = NA, na.translate = FALSE) +
  xlab("") +ylab("Effect size");Weight_forest_1mod


#Run cumulative adversity model without group size
adult_morph$CumulIndex6_nogs <- as.numeric(adult_morph$MomAllLossType) + as.numeric(adult_morph$PrimpIndex) + as.numeric(adult_morph$CompetingSibIndex) + adult_morph$s_kinBirthR + as.numeric(adult_morph$RankIndex) + as.numeric(adult_morph$HurricaneYear1)

model <- lmer(body_weight_kg ~ CumulIndex6_nogs + log_age + I(log_age^2) + s_KinCountGrowthR + s_groupsize_growth + (1|AnimalID.text) + (1|Birth.Season) + (1|NatalGroup), data = adult_morph)
fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "df", "tvalue", "pvalue")
fixed_effects$n <- nobs(model)

### ARM LENGTH ###

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex",  "HurricaneYear1", "CumulIndex7")
#Remove hurricane and cumulative index because they failed to converge
#AIC lower for linear models without quadratic term

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())

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

### LEG LENGTH ###

predictors<- c("MomAllLossType", "PrimpIndex", "CompetingSibIndex", "s_grpsize", "s_kinBirthR", "RankIndex", "HurricaneYear1", "CumulIndex7")
#Primp Index failed to converge

results <- data.frame(Predictor = character(),
                      n = integer(),
                      beta = numeric(),
                      se = numeric(),
                      pvalue = numeric())


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


#plot group size results
Birthskip_grpsize<-effect_plot(Birthskip_s_grpsize_model, pred = s_grpsize, interval = TRUE, plot.points = FALSE, line.colors=pal[3], line.thickness = 1.5) +
  geom_jitter(data= Repro_ELA, aes(y=NumberBirths/SeasonSpan, x=s_grpsize), size=0.5, alpha=0.3) +
  ylab("Proportion of years giving birth") +
  xlab("Early life group size (std)") +
  theme_bw(14); Birthskip_grpsize


#Include everything in one model
model <- glmer(cbind(NumberBirths, SeasonSpan - NumberBirths) ~ MomAllLossType + PrimpIndex + CompetingSibIndex + s_kinBirthR + RankIndex +  HurricaneYear1 + s_mean_ibi_group + s_mean_ibi_kin + (1|Birth.Season) + (1|NatalGroup), data = Repro_ELA, family = binomial(link = "logit"))

fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "zvalue", "pvalue")
fixed_effects<-fixed_effects[-c(1,9,10),]
fixed_effects$n <- nobs(model)
fixed_effects$Predictor <- rownames(fixed_effects)


fixed_effects$Predictor<-factor(fixed_effects$Predictor, levels=rev(c("MomAllLossType1", "PrimpIndex1", "CompetingSibIndex1",  "s_kinBirthR", "RankIndex0.5", "RankIndex1", "HurricaneYear1")), labels=rev(c("Maternal Loss", "Maternal Primiparity", "Competing Sibling", "Kin network size", "Maternal dominance: mid", "Maternal dominance: low", "Hurricane")))
fixed_effects$significance<-'ns'
fixed_effects$significance[which(fixed_effects$pvalue<0.05)]<-'p<0.05'

sigCOLS= c('grey',pal[4])

Birthskip_forest_1mod<-ggplot(data= fixed_effects, aes(x= Predictor, y= beta, color=significance)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=beta-1.96*SE, ymax=beta+1.96*SE, x=Predictor), linewidth=1) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_bw(14) +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.y=element_text(color="black", size=14))+
  scale_color_manual(values=c("ns"=sigCOLS[1], "p<0.05"= sigCOLS[2])) +
  xlab("") +ylab("Effect size");Birthskip_forest_1mod

#cumulative adversity 6 no group size
Repro_ELA$CumulIndex6_nogs <- as.numeric(Repro_ELA$MomAllLossType) + as.numeric(Repro_ELA$PrimpIndex) + as.numeric(Repro_ELA$CompetingSibIndex) + Repro_ELA$s_kinBirthR + as.numeric(Repro_ELA$RankIndex) + as.numeric(Repro_ELA$HurricaneYear1)

model <- glmer(cbind(NumberBirths, SeasonSpan - NumberBirths) ~ CumulIndex6_nogs + s_mean_ibi_group + s_mean_ibi_kin + (1|Birth.Season) + (1|NatalGroup), data = Repro_ELA, family = binomial(link = "logit"))

fixed_effects <- as.data.frame(summary(model)$coefficients)
colnames(fixed_effects)<- c("beta", "SE", "zvalue", "pvalue")
fixed_effects$n <- nobs(model)

