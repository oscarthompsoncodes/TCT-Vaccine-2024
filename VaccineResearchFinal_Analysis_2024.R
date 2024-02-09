library(usethis)
library(gptstudio)
library(openai)
library(tidyr)
library(psych)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(broom)
library(lme4)
library(ggplot2)
library(gtsummary)
library(corrtable)
library(Hmisc)
library(sjPlot)
library(jtools)
library(forestplot)

#### Vaccine project, Oscar Thompson, Monica Tamariz, Mioara Cristea

############################################################################
# 1. DATA FORMATTING AND CLEARNING
############################################################################

setwd("/Users/oscar/OneDrive/Uni/Carnegie Trust/Data-analysis/Rstudios-Projects")  
vax <- read.csv("VaxJan23.csv")  # complete data

vax = vax[-c(1:2),]    # remove rows

cols <- c(1:12, 14:22, 24:25, 32:34, 51:52) # make these variables factors
vax[cols] <- lapply (vax[cols],as.factor)

cols <- c(13, 23, 26:31, 35:50) # make these variables numeric
vax[cols] <- lapply (vax[cols],as.numeric)

vax = vax[-c(2:10,51)] # remove some columns we don't need

# name variables 
names(vax) <- c("DateTime","Consent", "StudentStatus", "Age", "Gender", "Country1", "Country2", "Ethnicity", "Shielded", 
  "Carer", "HadCOVID", "OfferedVaccine", "VaccineStatus", "VaccineIntention", "HaveMother", "HaveFather", "RelatWithMother", 
  "RelatWithFather", "SelfImportant", "SelfEffective", "SelfPositive", "Selfsafe", "MotherVaccineStatus", "FatherVaccineStatus", 
  "FriendVaccineStatus", "MotherImportant", "MotherEffective", "MotherPositive", "MotherSafe", "FatherImportant", "FatherEffective", 
  "FatherPositive", "FatherSafe", "FriendImportant", "FriendEffective", "FriendPositive", "FriendSafe", 
  "Self_general_opinion_vaccines_Important", "Self_general_opinion_vaccines_Effective", "Self_general_opinion_vaccines_Positive", 
  "Self_general_opinion_vaccines_Safe","StudentID")


# SOME DESCRIPTIVES
print(paste("A total of", dim(vax)[1], "participants accessed the survey." ))

# Remove those who did not agree and are not students
vax <- subset(vax, vax$Consent == "I Agree" & vax$StudentStatus == "Yes" & vax$Age <= 35 )

print(paste("A total of", dim(vax)[1], "participants agreed to take part, were students and under 35." ))

# separate Date from Time into 2 different variables
vax <- vax %>% separate_wider_delim(DateTime, delim = " ", names = c("Date", "time"))
vax <- as.data.frame(vax)

vax <- vax[,-2]            # remove the time of day
vax$Date <- as.Date(vax$Date)
hist(vax$Date, breaks = 15)

str(vax)

############################################################################
# Collapse the 4 attitudes for each person (mother, father, friend, self)
vax$Self_Attitude_Covid_Vaccine <- rowMeans(vax[,c(19:22)])
vax$Mother_Attitude_Covid_Vaccine <- rowMeans(vax[,c(26:29)])
vax$Father_Attitude_Covid_Vaccine <- rowMeans(vax[,c(30:33)])
vax$Friend_Attitude_Covid_Vaccine <- rowMeans(vax[,c(34:37)])
vax$Self_Attitude_Any_Vaccine <- rowMeans(vax[,c(38:41)])


#######################################
# Transform text answers to vaccination status into yes/no
##(there is another alternative: to turn this into an ordinal variable - see below)

####  1. Agents' vaccinated vs. un-vaccinated (behaviour put into a binary category, yes or no)
vax <- mutate(vax, Self_Vaccine_Status = ifelse(grepl("Yes,", VaccineStatus), 1,
                                     ifelse(grepl("No", VaccineStatus), 0, NA)))
                                            
vax <- mutate(vax, Mother_Vaccine_Status = ifelse(grepl("Yes,", MotherVaccineStatus), 1,
                                                ifelse(grepl("No", MotherVaccineStatus), 0, NA)))

vax <- mutate(vax, Father_Vaccine_Status = ifelse(grepl("Yes,", FatherVaccineStatus), 1,
                                                  ifelse(grepl("No", FatherVaccineStatus), 0, NA)))

vax <- mutate(vax, Friend_Vaccine_Status = ifelse(grepl("Yes,", FriendVaccineStatus), 1,
                                                  ifelse(grepl("No", FriendVaccineStatus), 0, NA)))

#### 2. Agents vaccinated vs. un-vaccinated - categorical variable 


vax <- mutate(vax, Self_Vaccine_Status_Categorical = ifelse(grepl("Yes,", VaccineStatus), "vaccinated",
                                     ifelse(grepl("No", VaccineStatus), "unvaccinated", NA)))
                                            
vax <- mutate(vax, Mother_Vaccine_Status_Categorical = ifelse(grepl("Yes,", MotherVaccineStatus), "vaccinated",
                                                ifelse(grepl("No", MotherVaccineStatus), "unvaccinated", NA)))

vax <- mutate(vax, Father_Vaccine_Status_Categorical = ifelse(grepl("Yes,", FatherVaccineStatus), "vaccinated",
                                                  ifelse(grepl("No", FatherVaccineStatus), "unvaccinated", NA)))

vax <- mutate(vax, Friend_Vaccine_Status_Categorical = ifelse(grepl("Yes,", FriendVaccineStatus), "vaccinated",
                                                  ifelse(grepl("No", FriendVaccineStatus), "unvaccinated", NA)))

cols <- c(52:55)
vax[cols] <- lapply(vax[cols], as.factor)


names(vax)
#######################################
# Remove unnecessary variables

vax <- vax[,-c(2,3,4,6:10,12:16,19:41)] 
# removed most participant data - basically only keeping those variables used in analyses 
# good for simplicity!
vax <- vax[,-c(1)]

## 1. make reverse attitudes to use main data-set 
vax <- vax %>% mutate(Mother_attitude_reversed =  11 - Mother_Attitude_Covid_Vaccine,
                      Father_attitude_reversed = 11 - Father_Attitude_Covid_Vaccine,
                      Friend_attitude_reversed = 11 - Friend_Attitude_Covid_Vaccine,
                      Student_attitude_reversed =  11 - Self_Attitude_Covid_Vaccine)

names(vax)

############################################################################
###         H1: ATT ~ ATT
############################################################################


### 1. TRANSFORM DATA

att <- vax[,c(6:9)] # attitudes
att_ <- 11-att   # subtract values from 11, so a value of 10 will become 1 and a value of 1 will become 10, etc.

names(att_)

### 2. CHECKING DATA 

mean(att_$Mother_Attitude_Covid_Vaccine, na.rm = TRUE)
var(att_$Mother_Attitude_Covid_Vaccine, na.rm = TRUE)

### 3. MODELLING

glma <-  glm(Student_attitude_reversed ~ 
                Mother_attitude_reversed + 
                Father_attitude_reversed + 
                Friend_attitude_reversed, 
              vax, family = poisson(link = "log"))
summary(glma)
tab_model(glma)

glmg <-  glm(Student_attitude_reversed ~ 
               Mother_attitude_reversed + 
               Father_attitude_reversed*Gender + 
               Friend_attitude_reversed, 
             vax, family = poisson(link = "log"))
summary(glmg) 
tab_model(glmg)


glmaq <-  glm(Self_Attitude_Covid_Vaccine ~ 
               Mother_Attitude_Covid_Vaccine + 
               Father_Attitude_Covid_Vaccine + 
               Friend_Attitude_Covid_Vaccine, 
             att_, family = quasipoisson(link = "log"))
summary(glmaq) # dispersion 0.6,  < 1; not over-dispersed 

### 3.a Including parental relationships

attrel <-  glm(Student_attitude_reversed ~ 
               Mother_attitude_reversed*RelatWithMother + 
               Father_attitude_reversed*RelatWithFather + 
               Friend_attitude_reversed, 
             vax, family = poisson(link = "log"))
summary(attrel) # reg summary
tab_model(attrel, transform = NULL) # more useful, puts values as log-means 

attrel <-  glm(Student_attitude_reversed ~ 
                 Mother_attitude_reversed*RelatWithMother + 
                 Father_attitude_reversed*RelatWithFather + 
                 Friend_attitude_reversed, 
               vax, family = quasipoisson(link = "log"))
summary(attrel) # reg summary
tab_model(attrel, transform = NULL) # 

### 4. GRAPHICAL REPRESENTATION

# first make the general APA theme in the text
apa_theme <- theme(
  plot.margin = unit(c(1, 1, 1, 1), "cm"),
  plot.background = element_rect(fill = "white", color = NA),
  plot.title = element_text(size = 22, face = "bold",
                            hjust = 0.5,
                            margin = margin(b = 15)),
  axis.line = element_line(color = "black", linewidth = .5), # had to change size to linewidth
  axis.title = element_text(size = 10, color = "black",
                            face = "bold"),
  axis.text = element_text(size = 10, color = "black"),
  axis.text.x = element_text(margin = margin(t = 10)),
  axis.title.y = element_text(margin = margin(r = 10)),
  axis.ticks = element_line(linewidth = .5), # size to linewidth 
  panel.grid = element_blank(),
  legend.position = c(0.20, 0.8),
  legend.background = element_rect(color = "black"),
  legend.text = element_text(size = 12),
  legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
  legend.key = element_rect(color = NA, fill = NA),
  panel.background = element_rect(fill = "white")
)
    

ggh1 <- ggplot(data = att_, aes(y = Self_Attitude_Covid_Vaccine)) +
        geom_jitter(aes(x = Mother_Attitude_Covid_Vaccine), color = "#E41A1C", width = 0.2, height = 0, alpha = 0.5) +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), 
                    aes(x = Mother_Attitude_Covid_Vaccine, color = "Mother"), se = TRUE) +
        geom_jitter(aes(x = Father_Attitude_Covid_Vaccine), color = "#4DAF4A", width = 0.2, height = 0, alpha = 0.5) +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), 
                    aes(x = Father_Attitude_Covid_Vaccine, color = "Father"), se = TRUE) +
        geom_jitter(aes(x = Friend_Attitude_Covid_Vaccine), color = "#377EB8", width = 0.2, height = 0, alpha = 0.5) +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), 
                    aes(x = Friend_Attitude_Covid_Vaccine, color = "Friend"), se = TRUE) +
        scale_colour_manual("Others' Vaccine Attitude", 
                            breaks = c("Mother", "Father", "Friend"),
                            values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
        xlab("Others' Mean COVID-19 Vaccination Attitude (Reversed)") +
        ylab("Students' Mean COVID-19 Vaccination Attitude (Reversed)")

+ 
  # adding APA theme 
  apa_theme + 
  theme(legend.position = c(0.5, 0.9), # this is to change legend position 
        legend.box.just = "top", 
        panel.background = element_rect(fill = "white"))   +# background to white
  scale_x_continuous(limits = c(1, 12), breaks = seq(2, 12, by = 2)) + 
  scale_y_continuous(limits = c(1, 12), breaks = seq(2, 12, by = 2)) # set x,y to same values 
  
ggh1

ggsave("Poisson_apa_graph.png", 
       ggh1, 
       width = 6, 
       height = 6.5, 
       dpi = 300)


############################################################################
###         H2: ATT ~ BEH; whose vaccine status is the best predictor
###                        of attitudes?
############################################################################

### MODEL OVERDISPERSED
H2q <- glm(Student_attitude_reversed  ~ 
             Mother_Vaccine_Status + 
             Father_Vaccine_Status + 
             Friend_Vaccine_Status, 
           data = vax, family = quasipoisson(link = "log"))
summary(H2q)
tab_model(H2q)  # dispersion = 1.62, > 1. hence use the overdispersed model 

H2qg <- glm(Student_attitude_reversed  ~ 
             Mother_Vaccine_Status*Gender + 
             Father_Vaccine_Status + 
             Friend_Vaccine_Status, 
           data = vax, family = quasipoisson(link = "log"))
summary(H2qg)
tab_model(H2qg)  # dispersion = 1.62, > 1. hence use the overdispersed model 


H2qcat <- glm(Student_attitude_reversed  ~ 
            Mother_Vaccine_Status_Categorical + 
            Father_Vaccine_Status_Categorical + 
            Friend_Vaccine_Status_Categorical, 
          data = vax, family = quasipoisson(link = "log"))
summary(H2qcat)
tab_model(H2qcat)  # dispersion = 1.62, > 1. hence use the overdispersed model 

contrasts(vax$Mother_Vaccine_Status_Categorical)

############################################################################
###        COVARIANCE  
############################################################################

### 5. checking covariates

H2b <- glm(Student_attitude_reversed ~ 
             Mother_Vaccine_Status*RelatWithMother + 
             Father_Vaccine_Status*RelatWithFather + 
             Friend_Vaccine_Status, 
           data = vax, family = poisson(link = "log"))
summary(H2b)
tab_model(H2b, transform = NULL) 

H2bcat <- glm(Student_attitude_reversed ~ 
                Mother_Vaccine_Status_Categorical*RelatWithMother + 
                Father_Vaccine_Status_Categorical*RelatWithFather + 
                Friend_Vaccine_Status_Categorical, 
              data = vax, family = poisson(link = "log"))
summary(H2bcat)
tab_model(H2bcat, transform = NULL) 

summ(H2b) # jtools function that very helpfully summarises the model. great for the future without a doubt

#### graph 

par(mfrow = c(2,2))

h2c <- glm(Student_attitude_reversed ~ 
             Mother_Vaccine_Status_Categorical*RelatWithMother,
             data = vax, family = poisson(link = "log"))
summary(h2c)

## Important - to get the X-axis correctly labelled in capitals
# I had to change that in the original variable
# but I changed that back as other code keeps it uncapitalsied

# make the plot
rel <- plot_model(h2c, 
                  type = "int",
                  axis.labels = # SJPlot feature for renaming axes - no GGPlot needed!
                    c("Unvaccinated", 
                      "Vaccinated")) +
  
  apa_theme  +  # Rotate and adjust x-axis labels 
  xlab("Mother's COVID-19 Vaccine Uptake") +
  ylab("Students' Mean COVID-19 Vaccination Attitude (Reversed)") + 
  
  theme(legend.position = c(0.6, 0.9), # this is to change legend position 
        
        legend.box.just = "top", # position of legend
        
        panel.background = element_rect(fill = "white"), # makes background white
        
        plot.title = element_blank(), # removes the plot title 
        
        axis.text = element_text(size=12),
        
        axis.title=element_text(size=11,face="bold")) + # changes size of axis text (vaccinated/unvaccinated)
  
  scale_y_continuous(limits = c(0, 12), breaks = seq(2, 12, by = 2)) + # axis ticks on y adjustment 
  

  guides(color = guide_legend(title = "Mother Relationship Quality")) # legend title

rel


ggsave("Figure_5_att_relat_qual.png", plot = rel,width = 4, height = 5)


# I tried to change the legend labels with this:
    #scale_fill_discrete(name = "Mother Relationship Quality", 
                    # labels = c("Very Negative", "Very Positive"))

# but because the value is not a factor its a numeric it doesn't work 


# Create the interaction plot using ggplot2


# this plots the relationship with mother 
# what does it use for interaction terms?
  # 'type = "int" will automatically plot the interaction terms, 
  # however, using mdrt.values = "minmax" as default -
  # which means that as a deafult it will use 1 and 5 

plot_model(h2c, type = "int",  mdrt.values = "meansd")


############################################################################
###                   H2 TABLES
############################################################################

d <- vax %>% select(Self_Attitude_Covid_Vaccine, 
                    Mother_Vaccine_Status_Categorical, 
                    Father_Vaccine_Status_Categorical,
                      Friend_Vaccine_Status_Categorical)


aggregate(Self_Attitude_Covid_Vaccine ~ 
            Friend_Vaccine_Status_Categorical,
                           data = d,
                           FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)))

means_and_sd <- function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
summary_table <- tapply(d$Self_Attitude_Covid_Vaccine, d$Friend_Vaccine_Status_Categorical, means_and_sd)

print(summary_table)

means_and_ci <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  ci <- t.test(x, na.action = na.omit)$conf.int
  c(Mean = mean_val, Lower_CI = ci[1], Upper_CI = ci[2])
}

summary_table <- tapply(d$Self_Attitude_Covid_Vaccine, d$Friend_Vaccine_Status_Categorical, means_and_ci)

print(summary_table)

############################################################################
###                   H2 Graphs: student attitudes 
###                     predicted by others's vaccine status
############################################################################

# check code book, 17th July 23, for the code and your notes understanding this, and how you adapted the graph 
# more explanation on 15h august

CI <- function(x) {( qnorm(0.975) * sd(x/sqrt(length(x))))}  # Finction to calculate the 95% confidence interval (function)used in the plot below)

# 1. separate vaccinated from vaccinated for mother father and friend

mv <- subset(vax, Mother_Vaccine_Status_Categorical == "vaccinated")
mu <- subset(vax, Mother_Vaccine_Status_Categorical == "unvaccinated")

fv <- subset(vax, Father_Vaccine_Status_Categorical == "vaccinated")
fu <- subset(vax, Father_Vaccine_Status_Categorical == "unvaccinated")

bv <- subset(vax, Friend_Vaccine_Status_Categorical == "vaccinated")
bu <- subset(vax, Friend_Vaccine_Status_Categorical == "unvaccinated")


# 2. For each subset of the person's vaccine status, include the student's vaccine attitudes

mva <- mv$Self_Attitude_Covid_Vaccine[!is.na(mv$Self_Attitude_Covid_Vaccine)]
mua <- mu$Self_Attitude_Covid_Vaccine[!is.na(mu$Self_Attitude_Covid_Vaccine)]

fva <- fv$Self_Attitude_Covid_Vaccine[!is.na(fv$Self_Attitude_Covid_Vaccine)]
fua <- fu$Self_Attitude_Covid_Vaccine[!is.na(fu$Self_Attitude_Covid_Vaccine)]

bva <- bv$Self_Attitude_Covid_Vaccine[!is.na(bv$Self_Attitude_Covid_Vaccine)]
bua <- bu$Self_Attitude_Covid_Vaccine[!is.na(bu$Self_Attitude_Covid_Vaccine)]

# 3. Create a data-frame with means and CI of the student's attitudes, for each person by their vaccine status 

dat2 <- data.frame(Vaccine_status = c("Vaccinated","Vaccinated","Vaccinated","Unvaccinated","Unvaccinated","Unvaccinated"),
                   Influence = c("Mother's Uptake","Father's Uptake","Friend's Uptake"
                                 ,"Mother's Uptake","Father's Uptake","Friend's Uptake"),
                   Mean = c(mean(mva),mean(fva),mean(bva),mean(mua),mean(fua),mean(bua)),
                   CI = c(CI(mva), CI(fva), CI(bva), CI(mua), CI(fua), CI(bua))
  
)

# add two variables needed to plot the confidence intervals
dat2$hi <- dat2$Mean + dat2$CI
dat2$low <- dat2$Mean - dat2$CI
# http://127.0.0.1:24825/graphics/90a7419a-f894-4fea-adab-6b787fb3ca13.png

# 4. plot it.

# Define APA-friendly color palette
apa_palette_beh <- c("#E69F00", "#56B4E9")

atth2 <-    ggplot(dat2, aes(x = Influence, y = Mean, fill= Vaccine_status)) +     
      geom_bar(position="dodge",stat = "identity") +
      geom_errorbar(position = position_dodge(width=.8), width=0.2, aes(ymin=low, ymax=hi)) +
      ylab("Students' Mean Attitude towards COVID-19 Vaccination") +
      xlab("The COVID-19 Vaccine Uptake of Each Agent") +
  labs(fill = "Agents' Vaccination Uptake") +
  scale_fill_manual(values = apa_palette_beh) +
  apa_theme +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(legend.position = c(0.4, 0.92),
        legend.key.height = unit(0.5, "cm"),    # Adjust legend box height
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = rel(0.6)),  # Adjust legend title size
        legend.text = element_text(size = rel(0.6)), # this is to change legend position 
        legend.box.just = "top")# set y to same values 
atth2

ggsave("bar_plot_student_attitudes_agents_uptake.png", plot = atth2,width = 6, height = 5)

############################################################################
###         h2b GREATEST PREDICTOR OF ATTITUDES?
############################################################################

## 1. Run with the variables as factors
attfull <- glm(Student_attitude_reversed ~ 
                 Mother_Vaccine_Status + 
                  Father_Vaccine_Status + 
                  Friend_Vaccine_Status + 
                  Mother_attitude_reversed + 
                  Father_attitude_reversed + 
                  Friend_attitude_reversed , 
                data = vax, family = poisson(link = "log"))
summary(attfull)
tab_model(attfull)
# dispersion =  0.5, <1. not overdispersed

attfullg <- glm(Student_attitude_reversed ~ 
                 Mother_Vaccine_Status + 
                 Father_Vaccine_Status + 
                 Friend_Vaccine_Status + 
                 Mother_attitude_reversed + 
                 Father_attitude_reversed*Gender + 
                 Friend_attitude_reversed , 
               data = vax, family = poisson(link = "log"))
summary(attfullg)
tab_model(attfullg)
# dispersion =  0.5, <1. not overdispersed

attfullcat <- glm(Student_attitude_reversed ~ Mother_Vaccine_Status_Categorical + 
                          Father_Vaccine_Status_Categorical + 
                          Friend_Vaccine_Status_Categorical + 
                          Mother_attitude_reversed +
                          Father_attitude_reversed + 
                          Friend_attitude_reversed , 
               data = vax, family = poisson(link = "log"))
summary(attfullcat)
tab_model(attfullcat)

## 2. check for further co-variance
attfullr <- glm(Student_attitude_reversed ~ Mother_Vaccine_Status + 
                 Father_Vaccine_Status + 
                 Friend_Vaccine_Status + 
                 Mother_attitude_reversed*RelatWithMother + 
                 Father_attitude_reversed*RelatWithFather + 
                 Friend_attitude_reversed , 
               data = vax, family = quasipoisson(link = "log"))
summary(attfullr)
tab_model(attfullr)



############################################################################
###         H3: BEH ~ ATT
############################################################################

### 1. MODEL 

h3 <- glm(Self_Vaccine_Status ~  
           Mother_Attitude_Covid_Vaccine + 
           Father_Attitude_Covid_Vaccine + 
           Friend_Attitude_Covid_Vaccine, 
         data = vax, family=binomial(link="logit"))
summary(h3) # father attitude signficant predictor 
tab_model(h3)

h3s <- glm(Self_Vaccine_Status ~  # including self attitudes
            Mother_Attitude_Covid_Vaccine + 
            Father_Attitude_Covid_Vaccine + 
            Friend_Attitude_Covid_Vaccine +
            Self_Attitude_Covid_Vaccine,
          data = vax, family=binomial(link="logit"))
summary(h3s) # no sig predictors when the individual is included
tab_model(h3s)


#### 1.b. running with relaitonship of parents
h3b <- glm(Self_Vaccine_Status ~  
            Mother_Attitude_Covid_Vaccine*RelatWithMother + 
            Father_Attitude_Covid_Vaccine*RelatWithFather + 
            Friend_Attitude_Covid_Vaccine, 
          data = vax, family=binomial(link="logit"))
summary(h3b) # father attitude signficant predictor 

summ(h3b)
tab_model(h3b)

tbl_regression(h3b)

#### tables 

d <- vax %>% select(Self_Vaccine_Status_Categorical,
                    Mother_Attitude_Covid_Vaccine,
                    Father_Attitude_Covid_Vaccine,
                      Friend_Attitude_Covid_Vaccine,
                      Self_Attitude_Covid_Vaccine)

library(dplyr)

aggregate(Friend_Attitude_Covid_Vaccine ~ 
            Self_Vaccine_Status_Categorical,
          data = d,
          FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)))

means_and_sd <- function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
summary_table <- tapply(d$Friend_Attitude_Covid_Vaccine, d$Self_Vaccine_Status_Categorical, means_and_sd)

print(summary_table)

means_and_ci <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  ci <- t.test(x, na.action = na.omit)$conf.int
  c(Mean = mean_val, Lower_CI = ci[1], Upper_CI = ci[2])
}

summary_table <- tapply(d$Friend_Attitude_Covid_Vaccine, d$Self_Vaccine_Status_Categorical, means_and_ci)

print(summary_table)
### 2. Graphical representation 


CI <- function(x) {( qnorm(0.975) * sd(x/sqrt(length(x))))}  # Finction to calculate the 95% confidence interval (function)used in the plot below)

##### including self vaccine attitude

names(vax)

toplot <- vax[,c(6:9,11)] # including self attitude  
toplot$Self_Vaccine_Status <- as.factor(toplot$Self_Vaccine_Status) # makes self beh. factor

toplot$Self_Vaccine_Status <- fct_collapse(toplot$Self_Vaccine_Status, # collapses factors and labels them to put in graph
                                           "Student Vaccinated" = "1", 
                                           "Student Unvaccinated" = "0")
str(vax)

lplot <- toplot %>% 
  gather(Who,Attitude,
         Mother_Attitude_Covid_Vaccine, 
         Father_Attitude_Covid_Vaccine, 
         Friend_Attitude_Covid_Vaccine,
         Self_Attitude_Covid_Vaccine)


# gather - makes a pair of many columns into one - it puts all attitudes into one column
# it then divides by the person - so it gathers each column, and puts it in column who
# it then puts this by vaccine status  

lplot$Who <- fct_collapse(lplot$Who, # selecting the who 
                          "Mother" = "Mother_Attitude_Covid_Vaccine", 
                          "Father" = "Father_Attitude_Covid_Vaccine",
                          "Friend" = "Friend_Attitude_Covid_Vaccine",
                          "Student" = "Self_Attitude_Covid_Vaccine")

toplot2 <- aggregate(lplot,Attitude ~ Who + Self_Vaccine_Status, FUN="mean")
CIs <- aggregate(lplot,Attitude ~ Who + Self_Vaccine_Status, FUN="CI")

toplot2$CI <- CIs$Attitude
toplot2$hi <- toplot2$Attitude + toplot2$CI
toplot2$lo <- toplot2$Attitude - toplot2$CI

g2b <- ggplot(toplot2,aes(y=Attitude,x=Self_Vaccine_Status,  fill=Who)) +
  geom_bar(position="dodge",stat = "identity", width=.8) +
  geom_errorbar(position = position_dodge(width=.8), width=0.2, aes(ymin=lo, ymax=hi)) +
  xlab("Student's own COVID-19 Vaccine Uptake") +
  ylab("Each Agents' Mean Attitude towards COVID-19 Vaccination") +
  apa_theme +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(legend.position = c(0.4, 0.87), # this is to change legend position 
        legend.box.just = "top",
        legend.key.height = unit(0.5, "cm"),    # Adjust legend box height
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = rel(0.6)),  # Adjust legend title size
        legend.text = element_text(size = rel(0.6))) +
  scale_fill_manual(breaks = c("Mother", "Father", "Friend", "Student"),
                    values = c("#E41A1C", "#4DAF4A", "#377EB8", "#984EA3")) +  # Adjust legend fill colors
  labs(fill = "Each Agent's Vaccine Attitude") 

g2b



ggsave("Fig3_StudentUptake_OtherAttitudes.png", dpi=300,height = 5, width = 6)



############################################################################
###         H4: BEH ~ BEH
############################################################################

# Log regression behaviour

h4 <- glm(Self_Vaccine_Status ~ 
            Mother_Vaccine_Status + 
            Father_Vaccine_Status + 
            Friend_Vaccine_Status, 
          data = vax, family = "binomial")
summary(h4)
tab_model(h4)

h4g <- glm(Self_Vaccine_Status ~ 
             Mother_Vaccine_Status*Gender + 
             Father_Vaccine_Status + 
             Friend_Vaccine_Status, 
           data = vax, family = "binomial")
summary(h4g)
tab_model(h4g, transform = NULL)

h4cat <- glm(Self_Vaccine_Status_Categorical ~ 
            Mother_Vaccine_Status_Categorical + 
            Father_Vaccine_Status_Categorical + 
            Friend_Vaccine_Status_Categorical, 
          data = vax, family = "binomial")
summary(h4cat)


# Log regression - relationship #

h4b <- glm(Self_Vaccine_Status ~ 
             Mother_Vaccine_Status*RelatWithMother + 
             Father_Vaccine_Status*RelatWithFather + 
             Friend_Vaccine_Status, 
           data = vax, family = "binomial")
summary(h4b)
tab_model(h4b, transform = NULL)

h4bcat <- glm(Self_Vaccine_Status_Categorical ~ 
            Mother_Vaccine_Status_Categorical*RelatWithMother + 
            Father_Vaccine_Status_Categorical*RelatWithFather + 
            Friend_Vaccine_Status_Categorical, 
          data = vax, family = "binomial")
summary(h4bcat)
tab_model(h4bcat, transform = NULL)

### 2. Graph 

# Trying to make a tabl

############################################################################
###         H4: BEH ~ BEH
############################################################################


status <- vax %>% # first get behaviour variables together 
  select(Self_Vaccine_Status_Categorical,
         Mother_Vaccine_Status_Categorical:
           Friend_Vaccine_Status_Categorical) 

status_c <- vax %>% # first get behaviour variables together 
  select(Self_Vaccine_Status_Categorical,
         Mother_Vaccine_Status_Categorical:
           Friend_Vaccine_Status_Categorical) %>%
  filter(complete.cases(.)) 

status_c %>% # basic summary with all statistcs including difference
  tbl_summary(by = Self_Vaccine_Status_Categorical) %>%
  add_difference() %>% # although, this seems to specify only the mean difference - we want factor difference
  add_p %>%
  add_q() %>% 
  add_overall() %>% 
  add_n() %>% 
  add_ci() %>% 
  add_stat_label()


tf <- tempfile("example", fileext = ".docx")

status_c %>% # basic summary with all statistcs including difference
  tbl_summary(by = Self_Vaccine_Status_Categorical) %>%
  add_overall() %>% 
  add_n() %>% 
  add_stat_label() %>%
  as_gt() %>%
  gt::gtsave(filename = "uptake_table.docx")



############################################################################
###         GREATEST PREDICTOR OF BEHAVIOUR ?
############################################################################


## but isn't this incorrect? why are you doing poisson when your variable is dichotmous?
behfull <- glm(Self_Vaccine_Status ~ 
                    Mother_Vaccine_Status + 
                    Father_Vaccine_Status + 
                    Friend_Vaccine_Status + 
                    Mother_Attitude_Covid_Vaccine + 
                    Father_Attitude_Covid_Vaccine + 
                    Friend_Attitude_Covid_Vaccine +
                    Self_Attitude_Covid_Vaccine, 
                  data = vax, family = "binomial")
summary(behfull)
tab_model(behfull)

tbl_regression(behfull, exponentiate = FALSE)

## categorical 

behfullcat <- glm(Self_Vaccine_Status ~ Mother_Vaccine_Status_Categorical + 
                    Father_Vaccine_Status_Categorical + 
                    Friend_Vaccine_Status_Categorical + 
                    Mother_Attitude_Covid_Vaccine + 
                    Father_Attitude_Covid_Vaccine + 
                    Friend_Attitude_Covid_Vaccine +
                    Self_Attitude_Covid_Vaccine, 
                  data = vax, family = "binomial")
summary(behfull2cat)
tab_model(behfull2cat, transform = NULL)



## but isn't this incorrect? why are you doing poisson when your variable is dichotmous?
behfullg <- glm(Self_Vaccine_Status ~ 
                 Mother_Vaccine_Status + 
                 Father_Vaccine_Status + 
                 Friend_Vaccine_Status + 
                 Mother_Attitude_Covid_Vaccine + 
                 Father_Attitude_Covid_Vaccine + 
                 Friend_Attitude_Covid_Vaccine +
                 Self_Attitude_Covid_Vaccine +
                  Gender, 
               data = vax, family = "binomial")
summary(behfullg)
tab_model(behfullg, transform = NULL)




############################################################################
###         REGRESSION TABLES
############################################################################
####

# SJPLOT TABLES

# using sjplot, put all models together into one table
tab_model(glma, H2q, attfull,
          dv.labels = c("Model 1: Others' Attitudes", # relabel model heads
                        "Model 2: Others' Uptake",
                        "Model 3: All Others' Norms"),
          pred.labels = c("Intercept", # relabel variable nanmes
                          "Mother's Vaccine Attitude", 
                          "Father's Vaccine Attitude",
                          "Friend's Vaccine Attitude", 
                          "Mother's Vaccine Uptake",
                          "Father's Vaccine Uptake", 
                          "Friend's Vaccine Uptake"),
          file = "C:/Users/oscar/OneDrive/Uni/Carnegie Trust/Data-analysis/Rstudios-Projects/attituderegression.doc")
# save file to location + insert name of file
# go here for more information on editing table: https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html


tab_model(h3s, h4, behfull,
          dv.labels = c("Model 4: Others' and Student's Attitudes",
                        "Model 5: Others' Uptake",
                        "Model 6: All Others' Norms and Student's Attiude's"),
          pred.labels = c("Intercept", 
                          "Mother's Vaccine Attitude", 
                          "Father's Vaccine Attitude",
                          "Friend's Vaccine Attitude",
                          "Student's Vaccine Attitude",
                          "Mother's Vaccine Uptake",
                          "Father's Vaccine Uptake", 
                          "Friend's Vaccine Uptake"),
          file = "C:/Users/oscar/OneDrive/Uni/Carnegie Trust/Data-analysis/Rstudios-Projects/uptakeregression2.doc")

tab_model(attrel, H2b, attfullr,
          dv.labels = c("Model 7: Others' Attitudes",
                        "Model 8: Others' Uptake",
                        "Model 9: All Others' Norms"),
          pred.labels = c("Intercept", 
                          "Mother's Vaccine Attitude",
                          "Mother's Relationship Quality",
                          "Father's Vaccine Attitude",
                          "Father's Relationship Quality",
                          "Friend's Vaccine Attitude",
                          "Mother's Attitude x Relationship Quality",
                          "Father's Attitude x Relationship Quality",
                          "Mother's Vaccine Uptake",
                          "Father's Vaccine Uptake", 
                          "Friend's Vaccine Uptake",
                          "Mother's Uptake x Relationship Quality",
                          "Father's Uptake x Relationship Quality"),
          file = "C:/Users/oscar/OneDrive/Uni/Carnegie Trust/Data-analysis/Rstudios-Projects/attrelatregression.doc")

tab_model(h3b, h4b,
          dv.labels = c("Model 10: Others' Attitudes",
                        "Model 11: Others' Uptake"),
          pred.labels = c("Intercept", 
                          "Mother's Vaccine Attitude",
                          "Mother's Relationship Quality",
                          "Father's Vaccine Attitude",
                          "Father's Relationship Quality",
                          "Friend's Vaccine Attitude",
                          "Mother's Attitude x Relationship Quality",
                          "Father's Attitude x Relationship Quality",
                          "Mother's Vaccine Uptake",
                          "Father's Vaccine Uptake", 
                          "Friend's Vaccine Uptake",
                          "Mother's Uptake x Relationship Quality",
                          "Father's Uptake x Relationship Quality"),
          file = "C:/Users/oscar/OneDrive/Uni/Carnegie Trust/Data-analysis/Rstudios-Projects/uptakerelregression.doc")


##### ATTITUDES OF THE STUDENT

h1t <- tbl_regression(glma, exponentiate =TRUE) %>%
  bold_p()  # att~ att 

summary(glma)
  
glance(glma)
tab_model(glma)

h2t <- tbl_regression(H2q, exponentiate =TRUE) %>%
  bold_p()# att ~ beh
compA <- tbl_regression(attfull, exponentiate =TRUE) %>%
  bold_p()# all att

h3t <- tbl_regression(h3s, exponentiate =TRUE) %>%
  bold_p()# beh ~ att, # h3 for others, h3s for including self
h4t <- tbl_regression(h4, exponentiate =TRUE) %>%
  bold_p()# beh ~ beh
compB <- tbl_regression(behfull, exponentiate =TRUE) %>%
  bold_p()# all beh
  # use behfull for just others, behfull2  self-attitudes, behfull2cat for categorical
  # **USE DIFFERENT SUMMARY FUNCTION AND INPUT DIRECTLY ON WORD FOR CI**

h1r <- tbl_regression(attrel, exponentiate =TRUE) %>%
  bold_p()
h2r <- tbl_regression(H2b, exponentiate =TRUE) %>%
  bold_p()
compAr <- tbl_regression(attfullr, exponentiate =TRUE) %>%
  bold_p()
h3r <- tbl_regression(h3b, exponentiate =TRUE) %>%
  bold_p()
h4r <- tbl_regression(h4b, exponentiate =TRUE) %>%
  bold_p()




###### Looking at student attitudes ######

fullA <- # all predictors of attitudes
  tbl_merge(
    tbls = list(h1t, h2t, compA),
    tab_spanner = c("Other's Vaccine Attitudes", 
                    "Other's Vaccine Uptake",
                    "All Others' Norms"))
fullA

###### Looking at student BEHAVIOUR ######

fullB <- # all predictors of attitudes
  tbl_merge(
    tbls = list(h3t, h4t, compB),
    tab_spanner = c("Other's Vaccine Attitudes", 
                    "Other's Vaccine Uptake",
                    "All Others' Norms"))
fullB

###### Looking at student atttiudes RELATIONSHIP ######

fullAr <- # all predictors of attitudes
  tbl_merge(
    tbls = list(h1r, h2r, compAr),
    tab_spanner = c("Other's Vaccine Attitudes", 
                    "Other's Vaccine Uptake",
                    "All Others' Norms"))
fullAr

fullBr <- # all predictors of attitudes
  tbl_merge(
    tbls = list(h3r, h4r),
    tab_spanner = c("Other's Vaccine Attitudes", 
                    "Other's Vaccine Uptake"))
fullBr

###

apa.reg.table(glma, H2q)

############################################################################
###         ANALYSIS: PARENTAL TYPES ADDITIVE MODEL of Behaviour
############################################################################
# get complete cases with darta from all agents
dat <- na.omit( vax[,c(11:14)] )

summary(lm(Self_Vaccine_Status~Add,data=vax))$coefficients[8]

summary(lm(Self_Vaccine_Status~Father_Vaccine_Status,data=dat))$coefficients[8]
summary(lm(Self_Vaccine_Status~Mother_Vaccine_Status,data=dat))$coefficients[8]
summary(lm(Self_Vaccine_Status~Friend_Vaccine_Status,data=dat))$coefficients[8]

summary(lm(Self_Vaccine_Status~Friend_Vaccine_Status,data=dat))$coefficients[8]


############################################################################
###                   ANALYSIS: HERITABILITY
############################################################################
# Function that gets two vectors, converts them to numeric, then does a Monte Carlo of N = T, and returns a list of verid_correl followed by Z-score of scrambled_correls
T <- 10000
MC_corr <- function(x,y){
  verid <- cor.test(as.numeric(x),as.numeric(y))
  store <- unname(verid$estimate)
  for (i in (1:T)){
    MCcor <- cor.test(as.numeric(x),sample(as.numeric(y)))
    store <- append(store, unname(MCcor$estimate))
    if(i == T){
      return(store)
    }
  }
}


# get data
data <- vax[,c(6:14)]

# get heritability for all pairs
Herit <- data.frame(Name1=c(), Name2=c(), verid_r =  c(), Z=c())
for (relation1 in c(1:length(data))){
  for (relation2 in c(1:length(data))){
    if (relation1 > relation2){
      name1 <- names(data[relation1])
      name2 <- names(data[relation2])
      Currdat <-  na.omit(data[,c(relation1, relation2)])
      l <- MC_corr(Currdat[,1],Currdat[,2])
      verid <- l[1]
      l <- l[-1]
      sd <- sd(l)
      z <- (verid - mean(l))/sd(l) 
      line <- data.frame(Name1 = name1, Name2 = name2, verid_r = verid, SD = sd, Z = z)
      Herit <- rbind(Herit,line)
    }
  }
}

Herit

#############################################
##  "Family tree" plot

family <- data.frame(who = c("student","friend","mother","father"),
                     b = c(0, -5, -1.85, 1.85 ),
                     a = c(1,  1,  4.00, 4.00 ))
                     

# plot Attitudes
Att <- Herit[c(1,2,4,3,5,6),]
Beh <- Herit[c(21,27,34,28,35,36),]

# Select which data we want to plot (Att or Beh) 
subq <- Att

# Remove nonsignificant values of Z
threshold = 3.1   # use 1.96 for p < 0.05 or 2.58 for p<0.005 (1-tailed) or 3.1 for 0.001(1-tailed)
subq <- mutate(subq, lineweight = ifelse(Z >= threshold & Z < Inf , Z/3,0 ))
  
  # Draw family tree with lines
g <- ggplot(family,aes(y=a,x=b)) +
  geom_point() +  xlim(-6.5,4) +  ylim(0,5) +
  geom_curve(aes(x = b[2], y = a[2], 
                 xend = b[3], yend = a[3]),curvature = .1,linewidth=subq[5,5], color="#aaaaaa") +  # friend-mother
  geom_curve(aes(x = b[2], y = a[2], 
                 xend = b[4], yend = a[4]),curvature = .1,linewidth=subq[6,5], color="#aaaaaa") +  # friend-father
  geom_curve(aes(x = b[1], y = a[1], 
                 xend = b[3], yend = a[3]),curvature = -.1,linewidth=subq[1,5], color="#d1774f") + # mother-student
  geom_curve(aes(x = b[1], y = a[1], 
                 xend = b[4], yend = a[4]),curvature = .1,linewidth=subq[2,5], color="#d1774f") + # father-student
  geom_curve(aes(x = b[1], y = a[1], 
                 xend = b[2], yend = a[2]),curvature = .1,linewidth=subq[3,5], color="#0d88a1") + # friend-student
  geom_curve(aes(x = b[4], y = a[4], 
                 xend = b[3], yend = a[3]),curvature = .1,linewidth=subq[4,5], color="#0d88a1") +  # father-mother
  theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank()) 
g
  
  
ggsave("Heritability_Vaccine_Att.jpg",dpi=300,height = 6, width = 8)

## Table of Heritability between attitudes and behaviours
Herit[,c(3,4)] <- round(Herit[,c(3,4)],2)

Tab <- data.frame()
Tab[1,1] <-Herit$Z[12] # mother-student
Tab[1,2] <-Herit$Z[30] # mother-friend
Tab[1,3] <-Herit$Z[23] # mother-father
Tab[1,4] <-Herit$Z[17] # mother-mother
Tab[2,1] <-Herit$Z[13] # father-student
Tab[2,2] <-Herit$Z[31] # father-friend
Tab[2,3] <-Herit$Z[24] # father-father
Tab[2,4] <-Herit$Z[18] # father-mother
Tab[3,1] <-Herit$Z[14] # friend-student
Tab[3,2] <-Herit$Z[32] # friend-friend
Tab[3,3] <-Herit$Z[25] # friend-father
Tab[3,4] <-Herit$Z[19] # friend-mother
Tab[4,1] <-Herit$Z[15] # student-student
Tab[4,2] <-Herit$Z[29] # student-friend
Tab[4,3] <-Herit$Z[22] # student-father
Tab[4,4] <-Herit$Z[16] # student-mother

rownames(Tab) <- c("Mother", "Father", "Friend", "Student")
colnames(Tab) <- c("Student", "Friend", "Father", "Mother")

# create nice table
kbl(Tab) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Heritability_Attid_Behav.pdf")        # <--- change name of file to number of relevant question

