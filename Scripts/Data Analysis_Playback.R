library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(aod)
library(effectsize)
library(ggtext)
library(pbkrtest)    # <- parametric bootstrapping
library(ROCR)
library(sjPlot)

# Read in data

data<-read.csv("Data/Processed Data_Playbacks.csv")

# Look at data

glimpse(data)

## Set the maximum time for duration of vigilance to 5 minutes. After this we conclude that the animal has permanently shifted beavhioural state 

data<-data %>% mutate(Duration_Vigilance = if_else(Duration_Vigilance>300, 300, Duration_Vigilance)) %>%
  mutate(Duration_Vigilance_NoZeros = if_else(Duration_Vigilance_NoZeros>300, 300, Duration_Vigilance_NoZeros))

# Drop any observations that have not been scored

data <- data %>% drop_na(Reaction_Stimuli)

# Reorder levels so bird is on the left and dog is on the right

data<- data %>%
  mutate(Stimulus = fct_relevel(Stimulus, 
                                "Bird", "Cattle", "Sheep", 
                                "Human", "Dog"))

# =====================================
# Data Visualisation
# =====================================

### Firstly duration of response, only where there is a reaction ####
ggplot(data, aes(x=Duration_Vigilance_NoZeros)) + geom_histogram(binwidth=10,alpha=0.5, position="identity") +
  xlab("Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +ylab("Count") + theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))
### Now Including where there is no reaction ####
ggplot(data, aes(x=Duration_Vigilance)) + geom_histogram(binwidth=10,alpha=0.5, position="identity") +
  xlab("Duration of Vigilance Response (s) \n(Note: Non-responses included)") +ylab("Count") + theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))
### Now Time to React ####
ggplot(data, aes(x=Time_to_Reaction)) + geom_histogram(binwidth=0.5,alpha=0.5, position="identity") +
  xlab("Time to Reaction (s)") +ylab("Count") + theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))

### Lets look at the breakdown of responses by Stimuli ####

ggplot(data, aes(x=Stimulus, fill=as.factor(Reaction_Stimuli))) + geom_bar(width=0.7) +
  scale_color_brewer(palette="Dark2") +
  xlab("Stimulus") +ylab("Count") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette="Dark2", labels=c("0" = "No Reaction", "1"="Reaction"), name = "Legend") +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank())



###  CUT OUT Windspeed appears a potentially important factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Max.Wind.Speed..m.s.,y=Duration_Vigilance_NoZeros))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Wind Speed (m/s)", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Max.Wind.Speed..m.s.,y=Reaction_Stimuli))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Wind Speed (m/s)", y ="Probability of Reaction") +
  theme_bw(base_size = 20)

###  Location appears a negligible factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Location,y=Duration_Vigilance_NoZeros))+ geom_boxplot() +
  labs(x="Location", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Location, fill=as.factor(Reaction_Stimuli))) + geom_bar(width=0.7) +
  scale_color_brewer(palette="Dark2") +
  xlab("Location") +ylab("Count") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette="Dark2", labels=c("0" = "No Reaction", "1"="Reaction"), name = "Legend") +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank())

###  Group Size appears a potentially important factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Group.Size,y=Duration_Vigilance_NoZeros))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Group Size", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Group.Size,y=Reaction_Stimuli))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Group Size", y ="Probability of Reaction") +
  theme_bw(base_size = 20)
###  Presence of Young appears a potentially important factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Presence.of.Y,y=Duration_Vigilance_NoZeros))+ geom_boxplot() +
  labs(x="Presence of Young", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Presence.of.Y, fill=as.factor(Reaction_Stimuli))) + geom_bar(width=0.7) +
  scale_color_brewer(palette="Dark2") +
  xlab("Presence of Young") +ylab("Count") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette="Dark2", labels=c("0" = "No Reaction", "1"="Reaction"), name = "Legend") +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank())
###  Sex appears a negligible factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Sex,y=Duration_Vigilance_NoZeros))+ geom_boxplot() +
  labs(x="Sex", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Sex, fill=as.factor(Reaction_Stimuli))) + geom_bar(width=0.7) +
  scale_color_brewer(palette="Dark2") +
  xlab("Sex") +ylab("Count") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette="Dark2", labels=c("0" = "No Reaction", "1"="Reaction"), name = "Legend") +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank())

###  Distance appears a potentially important factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Distance..m.,y=Duration_Vigilance_NoZeros))+ 
  geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Distance", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Distance..m.,y=Reaction_Stimuli))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Distance", y ="Probability of Reaction") +
  theme_bw(base_size = 20)


cdplot(as.factor(Reaction_Stimuli) ~ Distance..m., data=data)

###  Distance from cover appears a negligible factor factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=Distance.to.Cover..m.,y=Duration_Vigilance_NoZeros))+ 
  geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Distance from Cover", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=Distance.to.Cover..m.,y=Reaction_Stimuli))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Distance from Cover", y ="Probability of Reaction") +
  theme_bw(base_size = 20)
###  Playback nUmber appears a negligible factor factor affecting Vigilance from initial inspection of data ####
ggplot(data, aes(x=ï...Playback.Number,y=Duration_Vigilance_NoZeros))+ 
  geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Playback number", y ="Duration of Vigilance Response (s) \n(Note: Non-responses removed)") +
  theme_bw(base_size = 20)

ggplot(data, aes(x=ï...Playback.Number,y=Reaction_Stimuli))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Playback Number", y ="Probability of Reaction") +
  theme_bw(base_size = 20)

cdplot(as.factor(Reaction_Stimuli) ~ ï...Playback.Number, data=data)




# =====================================
# Graph Making
# =====================================

# Making Doughnut Graphs ####

# Create dataframe of reaction probability for each stimuli

Stimuli_Reaction_Prob<-data %>% 
  group_by(Stimulus) %>%
  summarise(mean(Reaction_Stimuli)) 


# First Birds ####
# Compute percentages


Bird <- (Stimuli_Reaction_Prob[1,])
Bird[2,1] <- "Bird"
Bird[2,2] <- (1-Bird[1,2])
Bird[,3] <- c("Reaction","No Reaction")
colnames(Bird) <- c("Stimulus","Probability","category")

# Compute the cumulative percentages (top of each rectangle)
Bird$ymax = cumsum(Bird$Probability)

# Compute the bottom of each rectangle
Bird$ymin = c(0, head(Bird$ymax, n=-1))

# Make the plot
ggplot(Bird, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.title = element_blank(),legend.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#C1C3D9","#030F75"))


# Now Cattle ####
# Compute percentages


Cattle <- (Stimuli_Reaction_Prob[2,])
Cattle[2,1] <- "Cattle"
Cattle[2,2] <- (1-Cattle[1,2])
Cattle[,3] <- c("Reaction","No Reaction")
colnames(Cattle) <- c("Stimulus","Probability","category")

# Compute the cumulative percentages (top of each rectangle)
Cattle$ymax = cumsum(Cattle$Probability)

# Compute the bottom of each rectangle
Cattle$ymin = c(0, head(Cattle$ymax, n=-1))

# Make the plot
ggplot(Cattle, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.title = element_blank(),legend.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#C1C3D9","#226C0B"))


# Now Sheep ####
# Compute percentages

Sheep <- (Stimuli_Reaction_Prob[3,])
Sheep[2,1] <- "Sheep"
Sheep[2,2] <- (1-Sheep[1,2])
Sheep[,3] <- c("Reaction","No Reaction")
colnames(Sheep) <- c("Stimulus","Probability","category")

# Compute the cumulative percentages (top of each rectangle)
Sheep$ymax = cumsum(Sheep$Probability)

# Compute the bottom of each rectangle
Sheep$ymin = c(0, head(Sheep$ymax, n=-1))

# Make the plot
ggplot(Sheep, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.title = element_blank(),legend.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#C1C3D9","#7400C4"))

# Now Human ####
# Compute percentages

Human <- (Stimuli_Reaction_Prob[4,])
Human[2,1] <- "Human"
Human[2,2] <- (1-Human[1,2])
Human[,3] <- c("Reaction","No Reaction")
colnames(Human) <- c("Stimulus","Probability","category")

# Compute the cumulative percentages (top of each rectangle)
Human$ymax = cumsum(Human$Probability)

# Compute the bottom of each rectangle
Human$ymin = c(0, head(Human$ymax, n=-1))

# Make the plot
ggplot(Human, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.title = element_blank(),legend.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#C1C3D9","#F77516"))

# Now Dog ####
# Compute percentages

Dog <- (Stimuli_Reaction_Prob[5,])
Dog[2,1] <- "Dog"
Dog[2,2] <- (1-Dog[1,2])
Dog[,3] <- c("Reaction","No Reaction")
colnames(Dog) <- c("Stimulus","Probability","category")

# Compute the cumulative percentages (top of each rectangle)
Dog$ymax = cumsum(Dog$Probability)

# Compute the bottom of each rectangle
Dog$ymin = c(0, head(Dog$ymax, n=-1))

# Make the plot
ggplot(Dog, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.title = element_blank(),legend.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#C1C3D9","#B70000"))


# Making boxplots of vigilance vs stimulus ####

# Now for duration of vigilance response (including zeros, i.e. non-response) ####

ggplot(data, aes(x=Stimulus, y=Duration_Vigilance)) + geom_boxplot() 
xlab("Stimulus") + ylab("Seconds Spent Vigilance \nFollowing Stimulus") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))

### Duration of vigilance response with non-responses taken out ####

ggplot(data, aes(x=Stimulus, y=Duration_Vigilance_NoZeros, colour = Stimulus, fill=Stimulus)) + geom_boxplot() + geom_jitter() +
  xlab("Stimulus") + ylab("Seconds Spent Vigilance \nFollowing Stimulus") +
  scale_colour_manual(values=c("#030F75","#226C0B", "#7400C4", "#F77516","#B70000")) +
  scale_fill_manual(values=c("#4B5079","#569244", "#9A5CC4", "#F79F5F","#BD6464")) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_text(size = 18, vjust = -0.5),
        axis.title.y = element_text(size = 18, vjust = 2),
        legend.title = element_blank()) 

## Time to React by Stimuli ####


ggplot(data, aes(x=Stimulus, y=Time_to_Reaction,  colour = Stimulus, fill=Stimulus)) + geom_boxplot() + geom_jitter() + 
  xlab("Stimulus") + ylab("Time to React (s)") +
  scale_colour_manual(values=c("#030F75","#226C0B", "#7400C4", "#F77516","#B70000")) +
  scale_fill_manual(values=c("#4B5079","#569244", "#9A5CC4", "#F79F5F","#BD6464")) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_text(size = 18, vjust = -0.5),
        axis.title.y = element_text(size = 18, vjust = 2),
        legend.title = element_blank())




# =====================================
# Data Analysis - Logit Model
# =====================================

# Let's scale and centre the variables we want to use in the model data ####

data <- data %>%
  mutate(
    Group.Size = c(scale(Group.Size, center = TRUE)),
    Distance..m. = c(scale (Distance..m., center = TRUE))
  )

# Convert character strings to factors
data <- data %>%
  mutate(
    Presence.of.Y = as.factor(Presence.of.Y),
    Location = as.factor(Location),
    Sex = as.factor(Sex)
  )

data<- data[-c(23,31),] # Drop data with missing data CHECK THESE!!!

# Lets do some analysis - logit GLM ####

logit.model <- glm(Reaction_Stimuli ~ Stimulus + Location + Group.Size + Sex
                   + Distance..m. + Presence.of.Y, data = data, family = "binomial")

logit.model_StimuliDropped <- glm(Reaction_Stimuli ~ Location + Group.Size + Sex
                                  + Distance..m. + Presence.of.Y, data = data, family = "binomial")

# Inspect the model & Evaluate Significance of Stimuli ####

summary(logit.model) ## All levels of Stimulus are significantly different from the control, even after controlling for the effect of other parameterssummary(logit.model_StimuliDropped)

car::Anova(logit.model, type = 3, test.statistic = "LR") # Stimulus is highly significant

PBmodcomp(logit.model, logit.model_StimuliDropped)

## Let's look at the odds ratio

effectsize::standardize_parameters(logit.model, exp = TRUE) # All non-control Stimuli are much more likely to get a response, but dog is vastly more likely

## Is dog significantly different to the other variables? 

# Let's look compare human  to dogs

l <- cbind(0, 0, 0, 1, -1, 0, 0, 0, 0, 0) # We create a vector to remove all terms from the model except dog and cattle
wald.test(b = coef(logit.model), Sigma = vcov(logit.model), L = l) ### Yes, the difference in coefficients between dog and cattle is significant

# What is the overall significance of Stimulus to probability of response? 

wald.test(b = coef(logit.model), Sigma = vcov(logit.model),Terms = 2:5) # Unsurprisingly, stimuli is overwhelmingly significant



# Making a graph of predicted probabilities of reaction by Stimulus

prediction.data_logit <- data.frame(Stimulus = c("Bird","Cattle","Sheep","Human","Dog"),Location = "Gaysay",
                      Group.Size = 0, Sex= "F",
                      Distance..m. = 0, Presence.of.Y = "N")

predicted_probability <- predict.glm(logit.model, newdata=prediction.data_logit, type = "response")
prediction.data_logit$Pred<-predicted_probability

ggplot(prediction.data_logit, aes(x=Stimulus,y=predicted_probability))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) 

plot_model(logit.model, type="pred", terms = "Stimulus")

## Calculating AUC of the model

AUC.data_logit <- data %>% slice(round(runif(n=10000, min=1, max=161), 0))
p <- predict(logit.model, newdata=AUC.data_logit, type="response")
pr <- prediction(p, AUC.data_logit$Reaction_Stimuli)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.7865451 Pretty Good

# Extract CIs and make Forest Plot ####

summary_logit <- summary(logit.model)
coefs.logit <- summary_logit$coefficients
CI_logit <- confint(logit.model, level = 0.95, method = "Wald")

Forest_plot_data <- data.frame(
  parameter = rownames(coefs),
  estimate = coefs[, "Estimate"],
  lower = CI_logit[, "2.5 %"],
  upper = CI_logit[, "97.5 %"]
)

ggplot(Forest_plot_data[-1,], aes(x = estimate, y = parameter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  labs(x = "Coefficient Estimate", y = "Predictors") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()


# Check Diagnostics for logit model ####

deviance_residuals<- residuals(logit.model, type = "deviance")
fitted_values <- fitted(logit.model)
plot(fitted_values, deviance_residuals)
abline(h=0,lty=2,col="red")


plot(data$Stimulus, deviance_residuals)
abline(h = 0, col = "red")


# =====================================
# Data Analysis - Count Model
# =====================================

### Lets do some more analysis - Gamma regression on the duration of the vigilance response

count.data <- data %>% drop_na(Duration_Vigilance_NoZeros)


count.model <- glm(Duration_Vigilance_NoZeros ~ Stimulus + Location + Group.Size + Sex
                   + Distance..m. + Presence.of.Y, data = count.data, family = Gamma(link=log))

summary(count.model)

car::Anova(count.model, type = 3, test.statistic = "LR") # Stimulus is highly significant


plot_model(count.model, type = "pred", terms = "Stimulus")


## Check that the above result holds even with major outliers removed ####

Q <- quantile(count.data$Duration_Vigilance_NoZeros, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(count.data$Duration_Vigilance_NoZeros)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(count.data, count.data$Duration_Vigilance_NoZeros > (Q[1] - 1.5*iqr) & count.data$Duration_Vigilance_NoZeros < (Q[2]+1.5*iqr))
eliminated.model <- glm(Duration_Vigilance_NoZeros ~ Stimulus + Location + Group.Size + Max.Wind.Speed..m.s. + Distance..m. + Presence.of.Y, data = eliminated, family = Gamma(link=log))
summary(eliminated.model)

#


l <- cbind(0, 1, 0, 0, -1, 0, 0, 0, 0, 0) # We create a vector to remove all terms from the model except dog and cattle
wald.test(b = coef(count.model), Sigma = vcov(count.model), L = l) ### Yes, the difference in coefficients between dog and cattle is significant
wald.test(b = coef(count.model), Sigma = vcov(count.model),Terms = 2:5) # Unsurprisingly, stimuli is overwhelmingly significant


#

summary_count <- summary(count.model)
coefs.count <- summary_count$coefficients
CI_count <- confint(count.model, level = 0.95, method = "Wald")

Forest_plot_data <- data.frame(
  parameter = rownames(coefs),
  estimate = coefs[, "Estimate"],
  lower = CI_count[, "2.5 %"],
  upper = CI_count[, "97.5 %"]
)

ggplot(Forest_plot_data[-1,], aes(x = estimate, y = parameter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  labs(x = "Coefficient Estimate", y = "Predictors") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()

ggplot(Forest_plot_data[,], aes(x = estimate, y = parameter)) +
  geom_line() 

### Lets do some more analysis - poisson regression on the duration of the vigilance response ####

count.model_2 <- glm(Time_to_Reaction ~ Stimulus + Location + Group.Size + Sex +
                       Distance..m. + Presence.of.Y, data = count.data, family = Gamma(link=log))

summary(count.model_2)

wald.test(b = coef(count.model_2), Sigma = vcov(count.model),Terms = 2:5) 

CI_count2 <- confint(count.model_2, level = 0.95, method = "Wald")


plot_model(count.model_2, type = "pred", terms = "Stimulus")
plot_model(count.model_2, type = "pred", terms = "Distance..m.")


####
FID <- read.csv("Data/FID Raw Data.csv")
glimpse(FID)


FID <- FID[,1:17]

ggplot(FID, aes(x=Flight.Distance)) + geom_histogram(binwidth=5,alpha=0.5, position="identity") +
  xlab("Flight Distance") +ylab("Count") + theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))

ggplot(FID, aes(x=Presence.of.Young.,y=Flight.Distance))+ geom_jitter() +
  labs(x="Presence of Young", y ="Flight Initiation Distance") +
  theme_bw(base_size = 20) # No visual evidence of relationship

ggplot(FID, aes(x=Group.Size,y=Flight.Distance))+ geom_point() +
  labs(x="Group Size", y ="Flight Initiation Distance") +
  theme_bw(base_size = 20) # No visual evidence of relationship

ggplot(FID, aes(x=Distance.to.Cover,y=Flight.Distance))+ geom_point() +
  labs(x="Distance to Cover", y ="Flight Initiation Distance") +
  theme_bw(base_size = 20) # No visual evidence of relationship

ggplot(FID, aes(x=Standing.,y=Flight.Distance))+ geom_jitter() +
  labs(x="Distance to Cover", y ="Flight Initiation Distance") +
  theme_bw(base_size = 20) # No visual evidence of relationship




FID <- FID %>%
  mutate(
    Group.Size = c(scale(Group.Size, center = TRUE)),
    Distance.to.Cover = c(scale(Distance.to.Cover, center = TRUE))
  )

FID <- FID %>%
  mutate(
    Location = as.factor(Location),
    Presence.of.Young. = as.factor(Presence.of.Young.),
    Standing. = as.factor(Standing.)
  )


FID<- FID %>%
  mutate(Location = fct_relevel(Location, 
                                "Lodge", "Gaysay", "Web "))


FID.model <- glm(Flight.Distance ~ Location  + Presence.of.Young. + Distance.to.Cover , data = FID, family = "poisson")

summary(FID.model)
CI_FID <- confint(FID.model, level = 0.95, method = "Wald")

l <- cbind(0, 1, -1, 0, 0) # We create a vector to remove all terms from the model except Gaysay and Web
wald.test(b = coef(FID.model), Sigma = vcov(FID.model), L = l) ### Yes, the difference in coefficients between Gaysay and Web is significant
wald.test(b = coef(FID.model), Sigma = vcov(FID.model),Terms = 2:3) # Unsurprisingly, Location is overwhelmingly significant


library(sjPlot)

plot_model(FID.model,type= "pred", terms= c("Distance.to.Cover","Presence.of.Young.","Location"))



prediction.data_FID<- data.frame(Location = c("Lodge","Gaysay","Web "), 
                                 Distance.to.Cover = 0,
                                 Presence.of.Young. = "N")

predicted_FID <- predict.glm(FID.model, newdata=prediction.data_FID, type = "response")
prediction.data_FID$Pred<-predicted_FID

ggplot(prediction.data_logit, aes(x=Stimulus,y=predicted_probability))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) 




### Time Lost to Grazing Per Encounter

Odds.logit<-data.frame()
for (i in 1:4) {
Odds.logit[i,1]<-exp(coefs.logit[1,1]+coefs.logit[i+1,1])
Odds.logit[i,2]<-exp(coefs.logit[1,1]+CI_logit[i+1,1])
Odds.logit[i,3]<-exp(coefs.logit[1,1]+CI_logit[i+1,2])
}

Probability <- Odds.logit/(1+Odds.logit)

Predicted_duration<-data.frame()
for (i in 1:4) {
  Predicted_duration[i,1]<-exp(coefs.count[1,1]+coefs.count[i+1,1])
  Predicted_duration[i,2]<-exp(coefs.count[1,1]+CI_count[i+1,1])
  Predicted_duration[i,3]<-exp(coefs.count[1,1]+CI_count[i+1,2])
}    


Predicted_seconds_per_encounter <- Predicted_duration*Probability

colnames(Predicted_seconds_per_encounter) <- c("Estimate", "2.5 %", "97.5 %") 
rownames(Predicted_seconds_per_encounter) <- c("Cattle", "Sheep", "Human", "Dog") 


Forest_plot_data <- data.frame(
  parameter = rownames(Predicted_seconds_per_encounter),
  estimate = Predicted_seconds_per_encounter[, "Estimate"],
  lower = Predicted_seconds_per_encounter[, "2.5 %"],
  upper = Predicted_seconds_per_encounter[, "97.5 %"]
)

ggplot(Forest_plot_data, aes(x = estimate, y = parameter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  labs(x = "Time Lost to Grazing Per Encounter", y = "Stimuli") +
  theme_bw()



Cattle.rate <- Predicted_seconds_per_encounter[1]*c(1:50)
Sheep.rate <- Predicted_seconds_per_encounter[2]*c(1:50)
Human.rate <- Predicted_seconds_per_encounter[3]*c(1:50)
Dog.rate <- Predicted_seconds_per_encounter[4]*c(1:50)

Time_Lost_to_Vigilance.dataframe <- as.data.frame(cbind(Cattle=Cattle.rate,
                                                  Sheep=Sheep.rate,
                                                  Human=Human.rate,
                                                  Dog=Dog.rate,
                                                  n=c(1:50)))

library(reshape)

Time_Lost_to_Vigilance.dataframe <- melt(Time_Lost_to_Vigilance.dataframe, id.vars = "n")



ggplot(data = Time_Lost_to_Vigilance.dataframe, aes(x = n, y = value, color = variable)) + geom_line(lwd = 1.2) +
  scale_color_manual(values = c("#226C0B","#7400C4","#F77516","#B70000")) + theme_bw() +
  ylab("Predicted Number of Seconds of  \n Grazing lost to Vigilance") +
  xlab("Number of Encounters") + theme(legend.title=element_blank())
  
