---
title: "FinanceSocial-Subjectivity"
author: "JoanneStasiak"
date: "3/8/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Introduce the packages we need
```{r}
library(simr)
library(tidyr)#for changing from long to wide//vice versa
library(ggpubr)
library(lme4)
library(sjPlot)#for interactions
library(ggridges)
library(jtools)
library(ggplot2)#for pretty plots
library(viridis)
library(dplyr)#for recoding df
library(interactions)#for interaction plots
library(effects)
library(hrbrthemes)## for glmer & lmer power analysis

```


Transform some of the data
```{r}
#Read in the data 
longDF <- read.csv("FinanceS_SubjectiveObjective_LongData_NA.csv")#this is just bringing in those csv's and giving it a shorter name (longDF and wideDF), this file^ only has the subjectivity data, not the other individual difference measures

wideDF <- read.csv("Qualtrics_TotalScores.csv")#this file has the individual difference measures and the averaged behavioral activity 


#Convert to factors - we're only doing this to the columns with words/phrases that do not have numerical value
longDF$PID <- as.factor(longDF$PID)
longDF$Gender <- as.factor(longDF$Gender)
longDF$Ethnicity <- as.factor(longDF$Ethnicity)
longDF$Activity <- as.factor(longDF$Activity)
longDF$Info <- as.factor(longDF$Info)

wideDF$PID <- as.factor(wideDF$ProlificPID)
wideDF$Gender <- as.factor(wideDF$Gender)
wideDF$Ethnicity <- as.factor(wideDF$Ethnicity)
wideDF$WorkingOut_Info <- as.factor(wideDF$WorkingOut_Info)
wideDF$EatingHealthy_Info <- as.factor(wideDF$EatingHealthy_Info)
wideDF$ControllingFeelings_Info <- as.factor(wideDF$ControllingFeelings_Info)
wideDF$RunningMile_Info <- as.factor(wideDF$RunningMile_Info)
wideDF$DrawingBicycle_Info <- as.factor(wideDF$DrawingBicycle_Info)
wideDF$LearnLanguage_Info <- as.factor(wideDF$LearnLanguage_Info)
wideDF$SavingMoney_Info <- as.factor(wideDF$SavingMoney_Info)
wideDF$Driving_Info <- as.factor(wideDF$Driving_Info)
wideDF$Caring_Info <- as.factor(wideDF$Caring_Info)
wideDF$ActEthic_Info <- as.factor(wideDF$ActEthic_Info)
wideDF$DoingMath_Info <- as.factor(wideDF$DoingMath_Info)

wideDF #this displays the data
longDF
```


Let's first look only at the subjectivity data
```{r}
#We can use some descriptive statistics to see how people on average assess the different activities

#Get mean of all ratings and activities
data <- longDF %>%
  group_by(Activity) %>%
  get_summary_stats(Ability, Confidence, Experience, Effort, Subjectivity, type = "mean_se")#this also gets "se" or standard error - we need that for the error bars in our bar plot
data#shows the new, summarized data
data$n <- NULL #unnecessary 
data <- data %>% #rename one of the columns
  rename(
    RatingType = variable,
    )
data


#Ratings Meanings
#1=Below average, 7=above average
#1=Not at ll confident, 7=Very confident
#1=No experience, 7=Very much experience
#1=No effort, 7=Very much effort
#1=Entirely subjective, 7=Entirely objective

# Error bars represent standard error of the mean
ggplot(data, aes(x=Activity, y=mean, fill=RatingType)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))

#transform from long to wide format, with Rating Type as the dependent variable
data$se <- NULL
data_wide_RT <- spread(data, Activity, mean)
data_wide_RT


#transform from long to wide format, with Activity as the dependent variable
data_wide_Act <- spread(data, RatingType, mean)#I haven't used this in analyses, it's just another way to visualize our data
data_wide_Act#display the transformed data


#Look at one activity separately - in this case we're looking at "Acting Ethically" - you can sub out the activity for any one you like! Just note the weird ` ` objects surrounding the y variable 
data_wide_RT$RatingType <- as.factor(data_wide_RT$RatingType)
p<-ggplot(data_wide_RT, aes(x=RatingType, y=`Acting Ethically`)) + 
    geom_bar(position=position_dodge(), stat="identity", fill = "skyblue") + ylab("Acting Ethically") 
p+ylim(0,7)


#Look at the ratings grouped by the activity
p<-ggplot(data, aes(x=Activity, y=mean, fill=RatingType)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
p+facet_grid(~Activity, scales = "free_x")
ggsave(file="gridRatingsxActivity.pdf", width=20, height=4, dpi=300)



#Let's look at the data grouped by rating type
ratings_plot<-ggplot(data, aes(x=RatingType, y=mean, fill=Activity)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
ratings_plot
```


Let's do some analyses! Are some ratings associated with others? Do some ratings greatly differ from others? 
``` {r}
#Let's first run some easy correlations
#How may one's perceived ability and one's confidence of one's same ability be associated?
cor.test(longDF$Confidence, longDF$Ability)#it doesn't matter what order we list our variables, we just want to see if any relationship exists between them


#run the above correlation as a regression- this will give us essentially the same output, it is just more legit (we are asking how confident participants are in their assessments of their ability, so we can't treat the two measures as independent, which is what correlations do)

model<-lm(Confidence~Ability, data=longDF)#here the order matters a little more - we are asking how one variable predicts another, in this case, how does one's reported ability affect how confident they are in that assessment?
#the above model is the same as (y ~ x), or "y as predicted by x", or "the dependent variable as predicted by the independent variable"
summary(model) #this will give us the stats for that above model (Intercept, slope, p-value, t-value)
plot(effect("Ability", model, main=""))#this will give us a really basic plot of the model, note that this does not account for repeated observations!


#So let's account for repeated observations! Now we can use a linear mixed effects regression (lmer) to address the multiple data points from each participant. We only need to add a "+ (1|PID)" to the model, as seen below
model_lmer <- lmer(Confidence~Ability + (1|PID), data=longDF)
summary(model_lmer)#We can use these stats to create a visualization of this data! We need the stats from the Intercept and the Slope - the slope is the number directly below the intercept value

longDF$Predictions <- NA #this creates a new column in our data frame
longDF$Predictions <- predict(model_lmer) #this fills that column using the model we previously made
confidenceXability <- ggplot(aes(x = Ability, y = Predictions, color = as.factor(PID)), data = longDF) +
  geom_line(size=.5) +
  geom_point(aes(y = Confidence)) +
  geom_abline(slope= 0.08210, intercept=5.13572,lwd= 1.5,col = 'black') +#this is where we have to update the stats, we need to add the values of the slope and the intercept of the model we ran so that we can make an accurate graph!
  xlab("Ability") + #whatever you want to call the x -axis -- this does not have to be syntactically the same as your x variable, for example, we could call this "Self-Reported Ability at Activities"
  ylab("Confidence") +#same deal for the y axis!
  ggthemes::theme_calc()+
  theme(legend.position = "none")
confidenceXability#this displays the graph

```

How does activity type play a role?
```{r}
#Now we can start to run interaction analyses - while we may have two measures that are associated to each other writ large, there is probably a lot of variance based on the activity that is being reported on!

model_lmer_interaction <- lmer(Confidence~Ability * Activity + (1|PID), data=longDF)# We can easily add a "*Activity " to our previous model to account for the different responses for each activity
summary(model_lmer_interaction)#gives us the stats for the model
sim_slopes(model_lmer_interaction, pred = Ability, modx =Activity, johnson_neyman = FALSE)#get the slopes of all of the different relationships
plot_model(model_lmer_interaction, type = "int", colors = "Set2")+theme_classic()#this gives us a messy plot of all of the slopes laid on top of each other - but it is hard to differentiate all of them!

qplot(x = Ability, y = Confidence, facets = ~Activity, data = longDF) +
  geom_smooth(method = "lm")#this divides that same plot into multiple squares so we can see how all those slopes vary

```

Repeat with the other measures
```{r}
#Ability and Subjectivity

able_Subj <- lmer(Subjectivity~Ability + (1|PID), data=longDF)
summary(able_Subj)

longDF$Predictions <- NA
longDF$Predictions <- predict(able_Subj)
subjectivityXability <- ggplot(aes(x = Confidence, y = Predictions, color = as.factor(PID)), data = longDF) +
  geom_line(size=.5) +
  geom_point(aes(y = Ability)) +
  geom_abline(slope= -0.03641, intercept=4.44162,lwd= 1.5,col = 'black') +
  xlab("Ability") +
  ylab("Subjectivity") +
  ggthemes::theme_calc()+
  theme(legend.position = "none")
subjectivityXability

```


```{r}
#Run more Moderation analyses
ableSubj_interaction <- lmer(Subjectivity~Ability * Activity + (1|PID), data=longDF)
summary(ableSubj_interaction)
sim_slopes(ableSubj_interaction, pred = Ability, modx =Activity, johnson_neyman = FALSE)#get the slopes of all of the different relationships
plot_model(ableSubj_interaction, type = "int", colors = "Set2")+theme_classic()

qplot(x = Ability, y = Subjectivity, facets = ~Activity, data = longDF) +
  geom_smooth(method = "lm")
```

Other potential analyses with this subset of data:

Confidence x Subjectivity
Experience x Ability
Subjectivity x Effort
Subjectivity x Experience
Effort x Ability
...and infinitely more

All possible vars: Ability, Effort, Experience, Confidence, Subjectivity, Info Source

~~~~

Sources of Info
```{r}

#Make a bar plot for the information source
counts <- table(longDF$Info, longDF$Activity)#this is just getting the number of endorsements for the different sources of information
barplot(counts, main="Information Source", xlab="Counts", las=2,cex.names=0.5, col=c("lightblue","lavender","pink", "lightgreen",  "skyblue","light yellow"),legend=rownames(counts),ylim=c(0,50), beside=TRUE)
#I LOVE the amount of variability in this graph!!


infoSubj <- aov(Subjectivity ~ Info, data=longDF)#we're running an Anova here because one of our variables is a factor, that is, we're looking at how the different sources of information may play a role in assessments of subjectivity
summary(infoSubj)
TukeyHSD(infoSubj)# this is similar to the simple slopes analysis we ran with lmer, but the anova equivalent

ggline(longDF, x = "Info", y = "Subjectivity", 
       add = c("mean_se"),color="orange",
       ylab = "Subjectivity", xlab = "Information Source")#this is a bit messy, but we can see how average assessments of subjectivity tend to differ when based on different sources of information!


```


~~~~~

How may these assessments track with individual differences? - - This is now using the 'wideDF' dataframe instead of longDF! 
```{r}
#Run some analyses - this one is for general self-efficacy
#So here we're looking at the interaction of self-efficacy on the relationship between Confidence and Subjectivity
mlm_Objectivity_Confidence_GSE <- lm(SubjectivityAvg~ConfidenceAvg * General.Self.Efficacy, data =wideDF)
summary(mlm_Objectivity_Confidence_GSE)#insignificant, womp womp

ModerationPlot <- interact_plot(mlm_Objectivity_Confidence_GSE, pred = ConfidenceAvg, modx = General.Self.Efficacy, x.label= "Confidence Rating", y.label = "Subjectivity", legend.main="Self-Efficacy")
ModerationPlot#we can see this relationship visualized, while it is usually a good sign that the lines cross, there is probably too much variance in the model, which is making it insignificant

sim_slopes(mlm_Objectivity_Confidence_GSE, pred = ConfidenceAvg, modx =General.Self.Efficacy, johnson_neyman = FALSE)#get the slopes of all of the different relationships

```

Other potential analyses:

DOSPERT_Financial x Saving Money Ability
IRI.Total x Caring for Others?
DERS x Controlling Feelings

Possible Individual Differences to look at:
Impulsiveness (BIS, CFC), Self-Efficacy (General, Academic), Empathy (IRI), Risk (DOSPERT), Difficulties in Emotion Regulation (DERS)

```{r}
#Run some more analyses - now we're looking at how Difficulties in Emotion Regulation (DERS) may predict being able to control one's feelings
mlm_DERS_ControlFeelings<- lm(ControllingFeelings_Ability~DERS_total, data =wideDF)
summary(mlm_DERS_ControlFeelings)#this is significant!!And a negative association! Which makes sense!It is saying that individuals who have greater difficulty regulating their emotions, also tend to report not being good at controlling their feelings - it is great to have consistency across measures!

wideDF$Predictions <- NA
wideDF$Predictions <- predict(mlm_DERS_ControlFeelings)
dersXcontrolFeelings <- ggplot(aes(x = DERS_total, y = Predictions, color = as.factor(PID)), data = wideDF) +
  geom_line(size=.5) +
  geom_point(aes(y = ControllingFeelings_Ability)) +
  geom_abline(slope= -0.038548, intercept=7.564176,lwd= 1.5,col = 'black') +
  xlab("Difficulties in Emotion Rejection") +
  ylab("Reported Ability of Controlling One's Feelings") +
  ggthemes::theme_calc()+
  theme(legend.position = "none")
dersXcontrolFeelings


```

Run analyses with Behavioral Data!

Possibly:
Finance Tips_FinanceName x Caring Ability // Acting Ethically Ability
controlling feelings and iri (empathy)...

~~~~

Shay's Analyses!
```{r}
#Run some analyses - this one is for empathy and caring for others
CaringAbility_IRI <- lm(Caring_Ability~ IRI.Total, data =wideDF)
summary(CaringAbility_IRI)#Significant!! Awesome!!! People who tend to be more empathic also report being better at caring for others!

#This is not supposed to have slopes for each participant (which I forgot!!) -- there are multiple slopes only when each participant has multiple observations (i.e., responses) for the same variable - in this analysis each participant has one observation for each variable (1. their self-reported ability of caring for others, and 2. their scored measure of empathy)
wideDF$Predictions <- NA
wideDF$Predictions <- predict(CaringAbility_IRI)
caringXiri <- ggplot(aes(x = IRI.Total, y = Predictions, color = as.factor(PID)), data = wideDF) +
  geom_line(size=.5) +
  geom_point(aes(y = Caring_Ability)) +
  geom_abline(slope= 0.04167, intercept=1.12817,lwd= 1.5,col = 'black') +
  xlab("Empathy Measure (IRI)") +
  ylab("Reported Ability of Caring for Others") +
  ggthemes::theme_calc()+
  theme(legend.position = "none")
caringXiri
```




