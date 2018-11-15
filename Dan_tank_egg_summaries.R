setwd("C:/Users/Daniel/Desktop/Summer2018TankData")


tanks = read.csv("Summer_2018_Tank_Experiment_Data_Eggs.csv")

head(tanks)

Egg.mass.counts = aggregate(Eggs_in_egg_mass ~ Week*Tank, data=tanks, FUN=length, drop=FALSE)

Egg.mass.avg.size = aggregate(Eggs_in_egg_mass ~ Week*Tank, data=tanks, FUN=mean, drop=FALSE)

Egg.total = aggregate(Eggs_in_egg_mass ~ Week*Tank, data=tanks, FUN=sum, drop=FALSE)
Egg.total

#calculate the average size of each eggmass per tank per week

Egg.mass.avg.size



subset(tanks, Tank == 1)

tank_labels = read.csv("Summer2018TankIdentity.csv")
head(tank_labels)

plant_ids = as.vector(rep(tank_labels$Plant, times=15))
nutrient_ids = as.vector(rep(tank_labels$Nutrient, times=15))

Egg.summary = data.frame("Plant" = plant_ids, "Nutrient" = nutrient_ids, "Week" = Egg.mass.counts$Week, "Egg_Masses" = Egg.mass.counts$Eggs_in_egg_mass,
                         "Egg_mass_avg_size" = Egg.mass.avg.size$Eggs_in_egg_mass, "Total_Eggs" = Egg.total$Eggs_in_egg_mass)
head(Egg.summary)

mean.TotalEggs = aggregate(Total_Eggs ~ Nutrient*Plant*Week, data=Egg.summary, FUN=mean)
head(mean.TotalEggs)

SEM = function(x){
  sd(x)/sqrt(length(na.omit(x)))
}

SE.TotalEggs = aggregate(Total_Eggs ~ Nutrient*Plant*Week, data=Egg.summary, FUN=SEM)
head(SE.TotalEggs)

mean.TotalEggs = cbind(mean.TotalEggs, "SE_Eggs" = SE.TotalEggs$Total_Eggs)
head(mean.TotalEggs)

plot(Total_Eggs ~ Week, data=mean.TotalEggs, pch=ifelse(Nutrient=="Y", 21, 22), bg=Plant)

Egg.grand.total = aggregate(Eggs_in_egg_mass ~ Tank, data=tanks, FUN=sum, drop=FALSE)

grand.total = cbind(tank_labels, "Eggs" = Egg.grand.total$Eggs_in_egg_mass)
head(grand.total)

plot(log(Eggs) ~ Plant, data=grand.total, pch=21,  bg=Nutrient)




library(MASS)
m1 = glm(Eggs ~ Plant*Nutrient, data=grand.total, family=poisson)
m2 = glm.nb(Eggs ~ Plant*Nutrient, data=grand.total)

library(bbmle)
AICtab(m1, m2, delta=T, sort=T, weights=T)

###
#  dAIC    df weight
#m2     0.0 11 1     
#m1 10848.6 10 <0.001
###

summary(m2)

# Call:
#   glm.nb(formula = Eggs ~ Plant * Nutrient, data = grand.total, 
#          init.theta = 1.747726436, link = log)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4387  -1.0948  -0.2603   0.4405   1.6767  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        7.0788     0.3785  18.703  < 2e-16 ***
#   PlantD            -1.1989     0.5357  -2.238  0.02522 *  
#   PlantH            -3.3837     0.5408  -6.256 3.94e-10 ***
#   PlantL            -1.2661     0.5358  -2.363  0.01812 *  
#   PlantR            -1.0212     0.5786  -1.765  0.07756 .  
# NutrientY         -0.4497     0.5354  -0.840  0.40089    
# PlantD:NutrientY   1.2737     0.7574   1.682  0.09265 .  
# PlantH:NutrientY   2.0993     0.7617   2.756  0.00585 ** 
#   PlantL:NutrientY   0.2290     0.7579   0.302  0.76252    
# PlantR:NutrientY   0.1158     0.7702   0.150  0.88054    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for Negative Binomial(1.7477) family taken to be 1)
# 
# Null deviance: 85.645  on 39  degrees of freedom
# Residual deviance: 43.547  on 30  degrees of freedom
# AIC: 563.39
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  1.748 
# Std. Err.:  0.363 
# 
# 2 x log-likelihood:  -541.391


Egg.Agg.mean = aggregate(Eggs ~ Plant*Nutrient, data = grand.total, FUN = mean)
Egg.Agg.SEM = aggregate(Eggs ~ Plant*Nutrient, data = grand.total, FUN = SEM)
Egg.Agg.SEM

Egg.Agg = cbind(Egg.Agg.mean, "Eggs" = Egg.Agg.SEM$Eggs)
head(Egg.Agg)
colnames(Egg.Agg)[4] <- "EggsSEM"
colnames(Egg.Agg)[3] <- "Eggsmean"
head(Egg.Agg)

library(ggplot2)
# Basic barplot
eggplot<-ggplot(data=Egg.Agg, aes(x=Plant, y=Eggsmean, fill = Nutrient)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  geom_errorbar(aes(ymin=Eggsmean-EggsSEM, ymax=Eggsmean+EggsSEM), width=.2,
            position=position_dodge(.9)) +
  scale_fill_manual(values=c('black','lightgray')) +
  theme_classic() + labs(title="Cummulative Egg Counts Across Plant Type", 
                       x="Plant", y = "Cummulative Egg Count") +
  scale_fill_brewer(palette="Blues")


eggplot



Egg.Agg





