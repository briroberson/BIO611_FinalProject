# ------------------------------------------------------------------------------
# Disparities in Perimyotis subflavus body mass between cave and culvert 
# hibernacula in Georgia, USA
# ------------------------------------------------------------------------------

# Set up directory -----
setwd("C:/Users/santi/OneDrive/Documentos/Proyectos/Culvert GA/Body Condition Data")

# Load necessary packages -----
x <-
  c(
    "tidyverse", "magrittr", "ggplot2", "ggplot2", "dplyr", "glmmTMB", "ggpubr", "ggeasy", "effects", "heplots",
    "MASS", "AICcmodavg", "dplyr", "AICcmodavg", "DHARMa", "Matrix",
    "lattice", "rstatix", "ggeffects", "sjPlot", "sjlabelled", "RColorBrewer", "DHARMa", "corrplot", "Hmisc", "installr",
    "remotes")


lapply(x, function(y) {
  # check if installed, if not install
  if (!y %in% installed.packages()[, "Package"])
    install.packages(y)
  
  # load package
  try(require(y, character.only = T), silent = T)
})

# TOTAL BAT MASS ANALYSIS ------------------------------------------------------
# ------------------------------------------------------------------------------

# Read in bat data for all PESU captures from all sites        
batsA_withNAs <- read.csv("BatData_PESU_All.csv") 
batsA <- batsA_withNAs %>% drop_na(mass) #Remove bats with no mass. Already removed bats with mass but no sex.
head(batsA)

# SUMMARIZE AND VISUALIZE DATA -----
# Summary Code for All PESU
table(batsA$type)
table(batsA$sex)
table(batsA$Pd_area)
table(batsA$timing)
table(batsA$year)
table(batsA$site_num)


# LOOK AT DATA RELATED TO BAT TRUE MASS (batsA) -----

# See mass average for all PESU by site type and survey timing
aggregate(x = batsA$mass, by = list(batsA$type, batsA$timing), FUN = mean, na.rm=T, na.action='na.pass')
# Mass higher in caves than culverts early and higher in culverts than caves late

# EARLY season weights all-------------------------------------------------------
# ------------------------------------------------------------------------------
subEarly = subset(batsA, timing %in% c("Early"))
subEarly_Cave = subset(subEarly, type %in% c("Cave"))   #Subset data to early season weights and by site type
subEarly_Culvert = subset(subEarly, type %in% c("Culvert"))

t.test(subEarly_Cave$mass, subEarly_Culvert$mass)
# wilcox.test(subEarly_Cave$mass, subEarly_Culvert$mass)
# Our t.test is significant so early season mass is sig different for cave v culvert PESU

mean(subEarly_Cave$mass);sd(subEarly_Cave$mass)
mean(subEarly_Culvert$mass);sd(subEarly_Culvert$mass)

# LATE Season Weights all ------------------------------------------------------
# ------------------------------------------------------------------------------
subLate = subset(batsA, timing %in% c("Late"))
subLate_Cave = subset(subLate, type %in% c("Cave"))   #Subset data to late season weights and by site type
subLate_Culvert = subset(subLate, type %in% c("Culvert"))

t.test(subLate_Cave$mass, subLate_Culvert$mass)
# wilcox.test(subLate_Cave$mass, subLate_Culvert$mass) 
# Our t.test is significant so late season mass is sig different for cave v culvert PESU

mean(subLate_Cave$mass);sd(subLate_Cave$mass)
mean(subLate_Culvert$mass);sd(subLate_Culvert$mass)

# Mmass average for all PESU by site type, sex, and survey timing --------------
# ------------------------------------------------------------------------------
aggregate(x = batsA$mass, by = list(batsA$type, batsA$timing, batsA$sex), 
          FUN = mean, na.rm=T, na.action='na.pass')
# Females always weigh more than males, regardless of timing or site type

# EARLY season weights by sex --------------------------------------------------
# ------------------------------------------------------------------------------
subEarly_Cave.F =subset(subEarly_Cave, sex %in% c("F"))
subEarly_Cave.M =subset(subEarly_Cave, sex %in% c("M"))
subEarly_Culvert.F =subset(subEarly_Culvert, sex %in% c("F"))
subEarly_Culvert.M =subset(subEarly_Culvert, sex %in% c("M"))
shapiro.test(subEarly_Cave$mass)
shapiro.test(subEarly_Culvert$mass)
shapiro.test(subEarly_Cave.F$mass)
shapiro.test(subEarly_Cave.M$mass)
shapiro.test(subEarly_Culvert.F$mass)
shapiro.test(subEarly_Culvert.M$mass)

t.test(subEarly_Cave.F$mass, subEarly_Culvert.F$mass) 
# wilcox.test(subEarly_Cave.F$mass, subEarly_Culvert.F$mass) 
# Our t.test is significant so Early season mass is significantly different for FEMALE cave v culvert

t.test(subEarly_Cave.M$mass, subEarly_Culvert.M$mass) 
# wilcox.test(subEarly_Cave.M$mass, subEarly_Culvert.M$mass)
# Our t.test is significant so Early season mass is significantly different for MALE cave v culvert 

# LATE Season Weights by sex ----------------------------------------------------
# ------------------------------------------------------------------------------
subLate_Cave.F =subset(subLate_Cave, sex %in% c("F"))
subLate_Cave.M =subset(subLate_Cave, sex %in% c("M"))
subLate_Culvert.F =subset(subLate_Culvert, sex %in% c("F"))
subLate_Culvert.M =subset(subLate_Culvert, sex %in% c("M"))

t.test(subLate_Cave.F$mass, subLate_Culvert.F$mass)
t.test(subLate_Cave.M$mass, subLate_Culvert.M$mass)
# wilcox.test(subLate_Cave.F$mass, subLate_Culvert.F$mass) 
# wilcox.test(subLate_Cave.M$mass, subLate_Culvert.M$mass)
# Our t.test is significant so late season mass is significantly different for FEMALE cave v culvert
# Our t.test is significant so late season mass is significantly different for MALE cave v culvert 

table(subLate$sex)

mean(subLate_Cave.F$mass);sd(subLate_Cave.F$mass)
mean(subLate_Culvert.F$mass);sd(subLate_Culvert.F$mass)

mean(subLate_Cave.M$mass);sd(subLate_Cave.M$mass)
mean(subLate_Culvert.M$mass);sd(subLate_Culvert.M$mass)

# Plot late body mass differences ----------------------------------------------
# by sex and site --------------------------------------------------------------

mean_sd <- function(x) {
  data.frame(y = mean(x), 
             ymin = mean(x) - sd(x), 
             ymax = mean(x) + sd(x))
}

# --------------------------------------
a <- ggplot(subLate_plot, aes(x = type, y = mass, fill=sex)) + 
  #geom_boxplot(outlier.color= "dark gray", outlier.fill="dark gray", outlier.size=1) +
  geom_violin(#alpha = 0.8, 
    #adjust = 4.2,
    #color = NA, 
    position =position_dodge(0.75),
    scale = "width",
    width = 0.60) +
  geom_jitter(alpha = 0.1) +
  #stat_summary(fun=mean, color = "black", position = position_dodge(0.9)) +
  stat_summary(geom = "linerange",
               fun.data = mean_sd,
               position = position_dodge(0.75),
               color = "#636363",
               size = 1)+
  stat_summary(fun = median, color = "#636363", position = position_dodge(0.75),
               geom = "crossbar", size = 2.5,width = 0.3,
               show.legend = FALSE,aes(fill=factor(sex)))+
  stat_summary(fun = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 19, size = 4,
               show.legend = FALSE,aes(fill=factor(sex)))+
  stat_summary(fun = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 19, size = 3,
               show.legend = FALSE,aes(fill=factor(sex)))+
  scale_fill_manual(values= c("#bdbdbd", "#ffeda0"),
                    labels = c("Female", "Male")) +
  scale_color_manual(values= c("#bdbdbd", "#ffeda0")) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, color = "#4D4D4D"),
        axis.title = element_text(size = 16, color = "#4D4D4D"),
        axis.title.x = element_blank(), 
        #panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "#4D4D4D"),
        panel.grid.minor = element_blank()) +
  #facet_wrap(. ~ sex, labeller = as_labeller(gender_names)) +
  ylab("Late season body mass (g)") +
  ylim(3.75, 8.25)

# Run models on total mass -----------------------------------------------------
# ------------------------------------------------------------------------------

# Look just at culvert data and late season bat mass 

# Subset data to late season culverts only for mass analysis
subLate = subset(batsA, timing %in% c("Late"))
subLate_Culvert = subset(subLate, type %in% c("Culvert"))

hist(subLate_Culvert.F$mass)
hist(subLate_Culvert.M$mass)

# Run models of late season mass for culvert PESU

# null
fm1 <- glmmTMB(mass ~ 1  + (1|site_num) , family ="gaussian", data=subLate_Culvert)
# single models
fm2 <- glmmTMB(mass ~ scale(year) + (1|site_num), family="gaussian", data=subLate_Culvert)
fm3 <- glmmTMB(mass ~ sex  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm4 <- glmmTMB(mass ~ Pd_area  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm5 <- glmmTMB(mass ~ scale(utmN)  + (1|site_num), family="gaussian", data=subLate_Culvert)
#fm6 <- glmmTMB(mass ~ roost  + (1|site_num), family="gaussian", data=subLate_Culvert)
# WNS expansion combinations
fm7 <- glmmTMB(mass ~ scale(year)*scale(utmN)  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm8 <- glmmTMB(mass ~ scale(year) + Pd_area  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm9 <- glmmTMB(mass ~ scale(year) + scale(utmN)  + (1|site_num), family="gaussian", data=subLate_Culvert)
# sex and WNS combinations
fm10 <- glmmTMB(mass ~ sex + Pd_area  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm11 <- glmmTMB(mass ~ sex + scale(utmN)  + (1|site_num), family="gaussian", data=subLate_Culvert)
fm12 <- glmmTMB(mass ~ scale(year) + sex + Pd_area  + (1|site_num) , family="gaussian", data=subLate_Culvert)
fm13 <- glmmTMB(mass ~ scale(year) + sex + scale(utmN)  + (1|site_num), family="gaussian", data=subLate_Culvert)
fmglobal1 <- glmmTMB(mass ~ year + sex + Pd_area + (1|site_num), family="gaussian", data=subLate_Culvert)

models_all <- list(fm1=fm1,fm3=fm3,fm4=fm4,fm5=fm5,fm9=fm9,fm10=fm10,
                   fm2=fm2,fm7=fm7,fm8=fm8,fm11=fm11,
                   fm12=fm12,fm13=fm13)

aictab(models_all)

# -----------------------------------------------------------------------------
summary(fm3);plot(allEffects(fm3)) #Sex significant

summary(fm3)

summary(fm12);plot(allEffects(fm11)) #Sex significant, positive effect recent years and negative Pd + area
summary(fm13);plot(allEffects(fm12)) #Sex significant, positive effect recent years, negative utmN inrc.
summary(fm11);plot(allEffects(fm10)) #Sex significant, negative utmN incr.
summary(fm10);plot(allEffects(fm9)) #Sex significant, negative Pd + area

#Look at random effects of site number for top model and plot them
ranef(fm3)

#Prep ranef output for plotting
str(r<-as.data.frame(ranef(fm3)))
r<-subset(r, select=-c(component, grpvar, term)); r
names(r)[names(r) == 'grp'] <- 'site'
names(r)[names(r) == 'condval'] <- 'intercept'
plot(intercept~site, data=r, xlab= "Site number", 
     ylab= "Random Effect Estimate")  + abline(h=0)

#Test goodness of fit for top model 
hist(residuals(fm3))
testDispersion(fm3) 

simulationOutput <- simulateResiduals(fittedModel = fm3, plot = F)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# PROPORTION BAT MASS LOSS ANALYSIS --------------------------------------------
# ------------------------------------------------------------------------------

# Summarize & Prep Data ----

# Read in bat data for PESU that were recaptured 2x in one winter and we calculated mass loss by proportion for.
batsR_withNA <- read.csv("recap_loss_prop.csv")
batsR <- batsR_withNA %>% drop_na(prop.loss) #Remove bats with no proportional mass loss.
batsR <- batsR %>% drop_na(year) #Remove bats with no year
head(batsR)

# Check data frame
summary(batsR)

# Summary Code for Recap PESU proportion
table(batsR$type)
table(batsR$sex)
table(batsR$Pd_area)
table(batsR$year)
table(batsR$site_num)

# Look at data dispersal
hist(batsR$prop.loss)
mean(batsR$prop.loss); var(batsR$prop.loss)

# Reformat data for analysis
str(batsR)
batsR$site <- as.factor(batsR$site)
batsR$type <- as.factor(batsR$type)
batsR$utmN <- as.integer(batsR$utmN)
batsR$sex <- as.factor(batsR$sex)
batsR$WNS_status <- as.factor(batsR$Pd_area)
batsR$year <- as.integer(batsR$year)
str(batsR)

# Proportion of mass loss means by factor 
m_sex = tapply(batsR$prop.loss, batsR$sex, mean, na.rm = T)
m_sex
m_WNS_status = tapply(batsR$prop.loss, batsR$Pd_area, mean, na.rm = T)
m_WNS_status
m_type = tapply(batsR$prop.loss, batsR$type, mean, na.rm = T)
m_type
aggregate(x = batsR$prop.loss, by = list(batsR$sex, batsR$type), FUN = mean) #Proportion of mass loss averages by sex and site type

#Subset data by site type ------------------------------------------------------
# ------------------------------------------------------------------------------

subCaveR = subset(batsR, type %in% c("Cave"))
subCulvertR = subset(batsR, type %in% c("Culvert"))

t.test(subCaveR$prop.loss, subCulvertR$prop.loss) 
# Our t.test is significant so proportion of mass loss is sig different for cave v culvert PESU!

mean(subCaveR$prop.loss);sd(subCaveR$prop.loss)
mean(subCulvertR$prop.loss);sd(subCulvertR$prop.loss)

# T-test to see if mass loss is significantly different cave vs culverts BY SEX
subCaveR.F =subset(subCaveR, sex %in% c("F"))
subCaveR.M =subset(subCaveR, sex %in% c("M"))
subCulvertR.F =subset(subCulvertR, sex %in% c("F"))
subCulvertR.M =subset(subCulvertR, sex %in% c("M"))

t.test(subCaveR.F$prop.loss, subCulvertR.F$prop.loss) 
# Our t.test is NOT significant so proportion of mass loss is NOT significantly different for FEMALE cave v culvert
  
mean(subCulvertR$prop.loss);sd(subCulvertR$prop.loss)

t.test(subCaveR.M$prop.loss, subCulvertR.M$prop.loss) 
# Our t.test is significant so proportion of mass loss is significantly different for MALE cave v culvert 

mean(subCulvertR.F$prop.loss);sd(subCulvertR.F$prop.loss)
mean(subCulvertR.M$prop.loss);sd(subCulvertR.M$prop.loss)

mean(subCaveR.F$prop.loss);sd(subCaveR.F$prop.loss)
mean(subCaveR.M$prop.loss);sd(subCaveR.M$prop.loss)

# Visualize overall loss by roost type
boxplot(subCulvertR$prop.loss, subCaveR$prop.loss, names = c("Culvert", "Cave"), xlab = "Roost type", ylab = "loss (g)")
hist(subCulvertR$prop.loss, xlab = "loss (g)", ylab = "Frequency")
hist(subCaveR$prop.loss, xlab = "loss (g)", ylab = "Frequency")

# Boxplot: Breakdown figures by sex and roost  (basically same as p1 but w/CIs)
subCulvertR.F = subset(subCulvertR,sex %in% c("F"))
subCulvertR.M = subset(subCulvertR, sex %in% c("M"))
subCaveR.F = subset(subCaveR,sex %in% c("F"))
subCaveR.M = subset(subCaveR,sex %in% c("M")) 
boxplot(subCulvertR.F$prop.loss, subCaveR.F$prop.loss, subCulvertR.M$prop.loss, subCaveR.M$prop.loss, names = c("culvert F", "cave F", "culvert M", "cave M"), xlab = "Roost Location", ylab = "loss (g)")

# Did not redo correlation tests from original biometric analysis since those variables didn't change, considering these still correlated:
# type/Pd_status, Pd_status/utmN, & type/utmN

# Plot results -----------------------------------------------------------------
# ------------------------------------------------------------------------------
gender_names <- c('F'= "Female", 'M'="Male")
ggplot(batsR, aes(x = type, y = prop.loss, fill=type)) + 
  geom_boxplot(outlier.color= "dark gray", outlier.fill="dark gray", outlier.size=1) +
  stat_summary(fun=mean, color = "black", position = position_dodge(0.7)) +
  scale_fill_manual(values= c("#bdbdbd", "#ffeda0")) +
  scale_color_manual(values= c("#bdbdbd", "#ffeda0")) +
  scale_y_continuous(labels = function(x) paste0((x*100), "%")) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, color = "#4D4D4D"),
        axis.title = element_text(size = 16, color = "#4D4D4D"),
        axis.title.x = element_blank(), 
        #panel.grid = element_blank(),
        legend.position = "none") +
  #facet_wrap(. ~ sex, labeller = as_labeller(gender_names)) +
  ylab("Percent mass lost (%)") 


# Calculating means and standard deviations
mean(subCulvertR.F$prop.loss); sd(subCulvertR.F$prop.loss)
mean(subCulvertR.M$prop.loss); sd(subCulvertR.M$prop.loss)
mean(subCaveR.F$prop.loss); sd(subCaveR.F$prop.loss)
mean(subCaveR.M$prop.loss); sd(subCaveR.M$prop.loss)

# Function to calculate mean and standard deviation
mean_sd <- function(x) {
  data.frame(y = mean(x), 
             ymin = mean(x) - sd(x), 
             ymax = mean(x) + sd(x))
}

# Subset data for late season
subLateR <- batsR %>% filter(timing == 'Late')

b <- ggplot(subLateR, aes(x = type, y = prop.loss, fill=sex)) + 
  #geom_boxplot(outlier.color= "dark gray", outlier.fill="dark gray", outlier.size=1) +
  geom_violin(#alpha = 0.8, 
    #adjust = 4.2,
    #color = NA, 
    position =position_dodge(0.75),
    scale = "width",
    width = 0.60) +
  geom_jitter(alpha = 0.1) +
  #stat_summary(fun=mean, color = "black", position = position_dodge(0.9)) +
  stat_summary(geom = "linerange",
               fun.data = mean_sd,
               position = position_dodge(0.75),
               color = "#636363",
               size = 1)+
  stat_summary(fun = median, color = "#636363", position = position_dodge(0.75),
               geom = "crossbar", size = 2.5,width = 0.3,
               show.legend = FALSE,aes(fill=factor(sex)))+
  stat_summary(fun = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 19, size = 4,
               show.legend = FALSE,aes(fill=factor(sex)))+
  stat_summary(fun = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 19, size = 3,
               show.legend = FALSE,aes(fill=factor(sex)))+
  scale_fill_manual(values= c("#bdbdbd", "#ffeda0"),
                    labels = c("Female", "Male")) +
  scale_color_manual(values= c("#bdbdbd", "#ffeda0")) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, color = "#4D4D4D"),
        axis.title = element_text(size = 16, color = "#4D4D4D"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "#4D4D4D"),
        panel.grid.minor = element_blank()) +
  #facet_wrap(. ~ sex, labeller = as_labeller(gender_names)) +
  ylab("Proportion of body mass lost (%)")

figure1 <- ggarrange(
  a, b, labels = c("A", "B"),
  common.legend = TRUE, legend = "right"
)

ggexport(figure1, filename = "figure1.tiff", width = 1000, height = 662, dpi = 300)

# Analysis w/glmmTMB function --------------------------------------------------
# ------------------------------------------------------------------------------

# Test beta regression with glms so we can include site number as a random effect for culverts only

mod1 <- glmmTMB(prop.loss ~ 1 + (1|site_num), family ="beta_family", data=subCulvertR)
mod2 <- glmmTMB(prop.loss ~ scale(length_m) + (1|site_num) , family ="beta_family", data=subCulvertR)
mod3 <- glmmTMB(prop.loss ~ sex + (1|site_num) , family ="beta_family", data=subCulvertR)
mod4 <- glmmTMB(prop.loss ~ scale(year) + (1|site_num) , family="beta_family", data=subCulvertR)
mod5 <- glmmTMB(prop.loss ~ Pd_area + (1|site_num) , family="beta_family", data=subCulvertR)
mod6 <- glmmTMB(prop.loss ~ scale(width_m) + (1|site_num) , family="beta_family", data=subCulvertR)
mod7 <- glmmTMB(prop.loss ~ scale(height_m) + (1|site_num) , family="beta_family", data=subCulvertR)
mod8 <- glmmTMB(prop.loss ~ scale(utmN) +  (1|site_num) , family="beta_family", data=subCulvertR) 
models <- list(mod1=mod1, mod2=mod2, mod3=mod3, mod4=mod4, mod5=mod5, mod6=mod6, mod7=mod7, mod8=mod8)
aictab(models)

# sex and culvert structure

mod9 <- glmmTMB(prop.loss ~ sex + scale(length_m) + (1|site_num) , family="beta_family", data=subCulvertR)
mod10 <- glmmTMB(prop.loss ~ sex + scale(length_m) + scale(width_m) + (1|site_num) , family="beta_family", data=subCulvertR)
#mod11 <- glmmTMB(prop.loss ~ sex + scale(length_m) + scale(height_m) + (1|site_num) , family="beta_family", data=subCulvertR)
summary(mod10)
summary(mod9)
# WNS expansion combinations
#mod12 <- glmmTMB(prop.loss ~ scale(year)*scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod13 <- glmmTMB(prop.loss ~ scale(year) + Pd_area  + (1|site_num), family="beta_family", data=subCulvertR)
#mod14 <- glmmTMB(prop.loss ~ scale(year) + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)

# sex and WNS combinations

mod15 <- glmmTMB(prop.loss ~ sex + Pd_area  + (1|site_num), family="beta_family", data=subCulvertR)
#mod16 <- glmmTMB(prop.loss ~ sex + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod17 <- glmmTMB(prop.loss ~ scale(year) + sex + Pd_area  + (1|site_num) , family="beta_family", data=subCulvertR)
#mod18 <- glmmTMB(prop.loss ~ scale(year) + sex + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
#mod18 <- glmmTMB(prop.loss ~ scale(year) + sex + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)

# Culvert structure and WNS combinations

mod19 <- glmmTMB(prop.loss ~ scale(length_m) + Pd_area  + (1|site_num), family="beta_family", data=subCulvertR)
#mod20 <- glmmTMB(prop.loss ~ scale(length_m) + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod21 <- glmmTMB(prop.loss ~ scale(year) + scale(length_m) + Pd_area  + (1|site_num) , family="beta_family", data=subCulvertR)
#mod22 <- glmmTMB(prop.loss ~ scale(year) + scale(length_m) + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod23 <- glmmTMB(prop.loss ~ scale(length_m) + Pd_area  + scale(height_m) + (1|site_num), family="beta_family", data=subCulvertR)
#mod24 <- glmmTMB(prop.loss ~ scale(length_m) + scale(utmN)  + scale(height_m) +(1|site_num), family="beta_family", data=subCulvertR)
mod25 <- glmmTMB(prop.loss ~ scale(year) + scale(length_m) + Pd_area  + scale(height_m) + (1|site_num) , family="beta_family", data=subCulvertR)
#mod26 <- glmmTMB(prop.loss ~ scale(year) + scale(length_m) + scale(utmN)  + scale(height_m) + (1|site_num), family="beta_family", data=subCulvertR)

# Culvert structure, sex,  WNS combinations

mod27 <- glmmTMB(prop.loss ~ sex + scale(length_m) + Pd_area  + (1|site_num), family="beta_family", data=subCulvertR)
#mod28 <- glmmTMB(prop.loss ~ sex + scale(length_m) + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod29 <- glmmTMB(prop.loss ~ sex + scale(year) + scale(length_m) + Pd_area  + (1|site_num) , family="beta_family", data=subCulvertR)
mod30 <- glmmTMB(prop.loss ~ sex + scale(year) + scale(length_m) + scale(utmN)  + (1|site_num), family="beta_family", data=subCulvertR)
mod31 <- glmmTMB(prop.loss ~ sex + scale(length_m) + Pd_area  + scale(width_m) + (1|site_num), family="beta_family", data=subCulvertR)
#mod32 <- glmmTMB(prop.loss ~ sex + scale(length_m) + scale(utmN)  + scale(height_m) +(1|site_num), family="beta_family", data=subCulvertR)
mod33 <- glmmTMB(prop.loss ~ sex + scale(year) + scale(length_m) + Pd_area  + scale(width_m) + (1|site_num) , family="beta_family", data=subCulvertR)
mod34 <- glmmTMB(prop.loss ~ sex + scale(year) + scale(length_m) + scale(utmN)  + scale(width_m) + (1|site_num), family="beta_family", data=subCulvertR)
mod35 <- glmmTMB(prop.loss ~ sex*scale(year) + scale(length_m) + (1|site_num), family="beta_family", data=subCulvertR)

mod.global <- glmmTMB(prop.loss ~ scale(year) + scale(length_m) + Pd_area  + scale(width_m) + 
                        sex + (1|site_num) , family="beta_family", data=subCulvertR)

models <- list(mod1=mod1, mod2=mod2, mod3=mod3, mod4=mod4, mod5=mod5, mod6=mod6, mod7=mod7, mod8=mod8,
               mod9=mod9, mod10=mod10, mod13=mod13, mod15=mod15,
               mod17=mod17, mod19=mod19, mod21=mod21, 
               mod23=mod23, mod25=mod25, mod27=mod27, mod29=mod29, mod30=mod30, 
               mod31=mod31, mod33=mod33, mod34=mod34, mod35=mod35, mod.global=mod.global)
aictab(models)
#Best models are fmR2 as the top then fmR4, fmR10, fmR15, and fmR18 within 2 delta AICc


summary(mod9) #Sex and length are significant to prop mass loss
summary(mod3)
summary(mod35)
summary(mod17)
summary(mod15)
summary(mod27)
summary(mod10)
summary(mod)
plot_model(fmRR2)