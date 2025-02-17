# Installation of missing libraries

libraries <- c("labelled","SmartEDA","epitools","ggplot2","pandoc","compareGroups","epiR","car")
check.libraries <- is.element(libraries, installed.packages()[, 1])==FALSE
libraries.to.install <- libraries[check.libraries]
if (length(libraries.to.install!=0)) 
{
    install.packages(libraries.to.install)
}

# Installation of "wo" class

rm(list = ls())
source("wo_class.R")

#############
# DATA LOAD #
#############

# source("data_wrangling.R",encoding="UTF-8",echo=TRUE) 

setwd("../Data")
load("WTA.RData")
setwd("../Results")

# Variable_Table.R

library(labelled)
sink("Variable_Table.txt")
generate_dictionary(wta)
sink()


#############################
# EXPLORATORY DATA ANALYSIS #
#############################

# Library charge

library(SmartEDA)
library(epitools)
library(ggplot2)
library(pandoc)

pandoc_install()
pandoc_activate()

# Exploratory Data Analysis

aux <- c("tourney_category", "tourney_level", "year", "surface", "round_level", "games", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age",
         "sum_age", "mean_age", "winner_rank", "loser_rank", "dif_rank", "match_outcome", "WalkOver") 
wta_eda <- wta[,aux]
rm(aux)

ExpReport(wta_eda, Template = NULL, Target = NULL, label = NULL, theme = "Default", op_file = "1 - Exploratory Data Analysis.html", op_dir = getwd(), sc = NULL, sn = NULL, Rc = NULL)

sink("1.1 - Descriptive WO y Default.txt")
# Descriptive Walkovers
result <- wo_1000(year_i = 1975, year_f = 2024)
print(result)

table(wta[wta$WalkOver=="WalkOver",]$Type_WO)
round(prop.table(table(wta[wta$WalkOver=="WalkOver",]$Type_WO))*100,1)

table(wta[wta$Type_WO=="Injury",]$Region_Injury)
round(prop.table(table(wta[wta$Type_WO=="Injury",]$Region_Injury))*100,1)

# Descriptive Default

result <- default_1000(year_i = 1975, year_f = 2024)
print(result)

table(wta$Default_Type)
prop.table(table(wta$Default_Type))
table(wta$Cause)
round(prop.table(table(wta$Cause)) * 100, 1)
table(wta[wta$Default == "Default",]$Default_Type)
round(prop.table(table(wta[wta$Default == "Default",]$Default_Type)) * 100, 1)
sink()

# Walkover by year

# Table

sink("1.1 - Incidence of Walkovers in WTA tennis matches by year (1975-2024).txt")
wo_1000(year_i=1975,year_f=2024)$dd[,1:4]
sink()

sink("1.1 - Incidence of Default in WTA tennis matches by year (1975-2024).txt")
default_1000(year_i=1975,year_f=2024)$dd[,1:4]
sink()

# Plot

png(file="1.2 - Incidence of Walkovers in WTA tennis matches by year (1975-2024).png", width=800, height=600)
plot.wo(year_i=1975,year_f=2024)
dev.off()

png(file="1.2 - Incidence of Default in WTA tennis matches by year (1975-2024).png", width=800, height=600)
plot.def(year_i=1975,year_f=2024)
dev.off()
###########################
# VARIABLE TRANSFORMATION #
###########################

# Transformation of variables for use in epidemiological mesures.


# Creation of the variable "Age difference between the winning and losing tennis players >= 0.2"

wta$dif_age_0.2 <- cut(wta$dif_age, breaks = c(-100,0.2,Inf),
                     labels = c("No", "Yes"),
                     right = FALSE)
attr(wta$dif_age_0.2,"label") <- "Age difference between the winning and losing tennis players >= 0.2"


# Creation of the variable "Difference in ranking positions between the winning and the losing tennis player >= -45"

wta$dif_rank_45 <- cut(wta$dif_rank, breaks = c(-2000,-45,Inf),
                       labels = c("No", "Yes"),
                       right = FALSE)
attr(wta$dif_rank_45,"label") <- "Difference in ranking positions between the winning and the losing tennis player >= -45"


#######################
# BIVARIATE ANALYSIS  #
#######################

# Library charge

library(compareGroups)
library(ggplot2)



# compareGroups (Continuous variables are not normal, which is why method=2 is used)

compare_wta <- compareGroups(WalkOver ~ tourney_category + year + surface + round_level + winner_hand + loser_hand + winner_age + loser_age + dif_age + mean_age + winner_rank + loser_rank + dif_rank + winner_age_21 + loser_age_21 + mean_age_21 + winner_rank_400 + loser_rank_400 + dif_rank_70 + dif_age_0, 
                             chisq.test.perm = TRUE, 
                             data = wta, 
                             method = 2, 
                             byrow = TRUE)

descriptive_wta <- createTable(compare_wta, show.all = TRUE, show.n = TRUE, show.p.overall = TRUE, 
                               digits = 2, q.type = c(2, 4))
descriptive_wta
compare_wta

table_wta <- createTable(compare_wta)
export2word(table_wta, file='2 - Bivariate Descriptive Analysis WalkOver.docx')

compare_wta <- compareGroups(Default ~ tourney_category + year + surface + round_level + winner_hand + loser_hand + winner_age + loser_age + dif_age + mean_age + winner_rank + loser_rank + dif_rank + winner_age_21 + loser_age_21 + mean_age_21 + winner_rank_400 + loser_rank_400 + dif_rank_70 + dif_age_0, 
                             chisq.test.perm = TRUE, 
                             data = wta, 
                             method = 2, 
                             byrow = TRUE)

descriptive_wta <- createTable(compare_wta, show.all = TRUE, show.n = TRUE, show.p.overall = TRUE, 
                               digits = 2, q.type = c(2, 4))





table_wta_default <- createTable(compare_wta)
export2word(table_wta_default, file='2 - Bivariate Descriptive Analysis Default.docx')

############################
# EPIDEMIOLOGICAL MEASURES #
############################

library(epiR) # This library includes the "epi.2by2" function.
library(epitools)

sink("3 - Epidemiological Measures.txt")

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("ALL MATCHES:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_1000(title_wo="All matches:")

# Incidende rate ratio by epi.2by2

print.epi <- function(a,A,ref,var)
{
    cat("Variable: ",var," -  Factor: ",A)
    cat(sep = "\n")
    cat("Walkovers per 1000 matches: ",a$tab[1,3])
    cat(sep = "\n")
    cat(sep = "\n")
    cat("Variable: ",var," - ",A," vs ",ref)
    cat(sep = "\n")
    cat("Incidence rate ratio:       ",paste0(round(a$massoc.summary$est[1],2)," (", round(a$massoc.summary$lower[1] ,2)," to ", round(a$massoc.summary$upper[1],2),")"))
    cat(sep = "\n")
    cat("Attributable risk:          ",paste0(round(a$massoc.summary$est[2],2)," (", round(a$massoc.summary$lower[2] ,2)," to ", round(a$massoc.summary$upper[2],2),")"))
    cat(sep = "\n")
    cat(sep = "\n")
}


# VARIABLE Tourney Category 

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("TOURNEY CATEGORY:")
cat(sep = "\n")
cat("-----------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$tourney_category, wta$WalkOver)
wo_table
cat(sep = "\n")


wo_1000(tourney_category="ITF Women's World Tennis Tour",title_wo="Tourney Category - ITF Women's World Tennis Tour:")
wo_1000(tourney_category="WTA 125 Tournaments",title_wo="Tourney Category - WTA 125 Tournaments")
wo_1000(tourney_category="WTA Tour",title_wo="Tourney Category - WTA Tour")

# Reference: ITF Women's World Tennis Tour (ITF Women's World Tennis Tour is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["ITF Women's World Tennis Tour", "WalkOver"]
Matches_Ref <- wo_table["ITF Women's World Tennis Tour", "WalkOver"] + wo_table["ITF Women's World Tennis Tour", "No WalkOver"]

# Tourney Category: WTA 125 Tournaments vs ITF Women's World Tennis Tour

WalkOver_A <- wo_table["WTA 125 Tournaments", "WalkOver"]
Matches_A <- wo_table["WTA 125 Tournaments", "WalkOver"] + wo_table["WTA 125 Tournaments", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.WTA_125 <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Tourney Category: WTA Tour vs ITF Women's World Tennis Tour

WalkOver_A <- wo_table["WTA Tour", "WalkOver"]
Matches_A <- wo_table["WTA Tour", "WalkOver"] + wo_table["WTA Tour", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.WTA_Tour <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.WTA_125,"WTA 125 Tournaments","ITF Women's World Tennis Tour","Tourney Category")
print.epi(epi.WTA_Tour,"WTA Tour","ITF Women's World Tennis Tour","Tourney Category")

rm(epi.WTA_125, epi.WTA_Tour)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)
unique(wta$surface) 
sum(is.na(wta$surface))


# VARIABLE Surface

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("SURFACE:")
cat(sep = "\n")
cat("--------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$surface, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(surface="Carpet",title_wo="Surface - Carpet:")
wo_1000(surface="Clay",title_wo="Surface - Clay:")
wo_1000(surface="Grass",title_wo="Surface - Grass:")
wo_1000(surface="Hard",title_wo="Surface - Hard:")

# Reference: Hard (Hard is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Hard", "WalkOver"]
Matches_Ref <- wo_table["Hard", "WalkOver"] + wo_table["Hard", "No WalkOver"]

# Surface: Carpet vs Hard

WalkOver_A <- wo_table["Carpet", "WalkOver"]
Matches_A <- wo_table["Carpet", "WalkOver"] + wo_table["Carpet", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Carpet <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Clay vs Hard

WalkOver_A <- wo_table["Clay", "WalkOver"]
Matches_A <- wo_table["Clay", "WalkOver"] + wo_table["Clay", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Clay <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Grass vs Hard

WalkOver_A <- wo_table["Grass", "WalkOver"]
Matches_A <- wo_table["Grass", "WalkOver"] + wo_table["Grass", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Grass <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Carpet,"Carpet","Hard","Surface")
print.epi(epi.Clay,"Clay","Hard","Surface")
print.epi(epi.Grass,"Grass","Hard","Surface")

rm(epi.Carpet, epi.Clay, epi.Grass)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)

# VARIABLE Round Level

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("ROUND LEVEL:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$round_level, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(round_level="Final Round",title_wo="Round Level - Final Round:")
wo_1000(round_level="Preliminary Round",title_wo="Round Level - Preliminary Round")
wo_1000(round_level="Qualifying Round",title_wo="Round Level - Qualifying Round")

# Reference: Preliminary Round (Preliminary Round is chosen as the reference surface as it is the most common)
WalkOver_Ref <- wo_table["Preliminary Round", "WalkOver"]
Matches_Ref <- wo_table["Preliminary Round", "WalkOver"] + wo_table["Preliminary Round", "No WalkOver"]

# Round Level: Final Round vs Preliminary Round

WalkOver_A <- wo_table["Final Round", "WalkOver"]
Matches_A <- wo_table["Final Round", "WalkOver"] + wo_table["Final Round", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Final <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Round Level: Qualifying Round vs Preliminary Round

WalkOver_A <- wo_table["Qualifying Round", "WalkOver"]
Matches_A <- wo_table["Qualifying Round", "WalkOver"] + wo_table["Qualifying Round", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Qualifying <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Final,"Final Round","Preliminary Round","Round Level")
print.epi(epi.Qualifying,"Qualifying Round","Preliminary Round","Round Level")

rm(epi.Final, epi.Qualifying)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)


# VARIABLE Difference Age

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("AGE DIFFERENCE BETWEEN THE WINNING AND LOSING TENNIS PLAYERS:")
cat(sep = "\n")
cat("-------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$dif_age_0.2, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(dif_age_f=0.2,title_wo="Age Difference < 0.2 y.o. :")
wo_1000(dif_age_i=0.2,title_wo="Age Difference >= 0.2 y.o. :")

# Reference: Age Difference < 0.2 y.o.
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]
 
# Age Difference: < 0.2 y.o. vs >= 0.2 y.o.
 
WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
 
print.epi(epi.Yes,">= 0.2 y.o.","< 0.2 y.o.","Age Difference")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)

# VARIABLE Difference Rank

# Walkovers per 1000 matches:

cat(sep = "\n")
cat("DIFFERENCE IN RANKING POSITIONS BETWEEN THE WINNING AND THE LOSING TENNIS PLAYER:")
cat(sep = "\n")
cat("---------------------------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

wo_table <- table(wta$dif_rank_70, wta$WalkOver)
wo_table
cat(sep = "\n")

wo_1000(dif_rank_f=-45,title_wo="Difference in ranking positions between the winning and the losing tennis player < -45")
wo_1000(dif_rank_i=-45,title_wo="Difference in ranking positions between the winning and the losing tennis player >= -45")

# Reference: Difference in ranking positions between the winning and the losing tennis player < -45
WalkOver_Ref <- wo_table["No", "WalkOver"]
Matches_Ref <- wo_table["No", "WalkOver"] + wo_table["No", "No WalkOver"]

# Difference in ranking positions between the winning and the losing tennis player < -45 vs >= -45

WalkOver_A <- wo_table["Yes", "WalkOver"]
Matches_A <- wo_table["Yes", "WalkOver"] + wo_table["Yes", "No WalkOver"]
wo_epiR <- c(WalkOver_A, Matches_A, WalkOver_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=wo_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= -45","< -45","Difference in ranking positions between the winning and the losing tennis player")

rm(epi.Yes)
rm(wo_table, WalkOver_Ref, Matches_Ref, WalkOver_A, Matches_A, wo_epiR)

sink()

sink("3 - Epidemiological Measures Default.txt")

# Defaults per 1000 matches:

cat(sep = "\n")
cat("ALL MATCHES:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

default_1000(title_default="All matches:")

# Incidence rate ratio by epi.2by2

print.epi <- function(a,A,ref,var)
{
  cat("Variable: ",var," -  Factor: ",A)
  cat(sep = "\n")
  cat("Defaults per 1000 matches: ",a$tab[1,3])
  cat(sep = "\n")
  cat(sep = "\n")
  cat("Variable: ",var," - ",A," vs ",ref)
  cat(sep = "\n")
  cat("Incidence rate ratio:       ",paste0(round(a$massoc.summary$est[1],2)," (", round(a$massoc.summary$lower[1] ,2)," to ", round(a$massoc.summary$upper[1],2),")"))
  cat(sep = "\n")
  cat("Attributable risk:          ",paste0(round(a$massoc.summary$est[2],2)," (", round(a$massoc.summary$lower[2] ,2)," to ", round(a$massoc.summary$upper[2],2),")"))
  cat(sep = "\n")
  cat(sep = "\n")
}

# VARIABLE Tourney Category 

cat(sep = "\n")
cat("TOURNEY CATEGORY:")
cat(sep = "\n")
cat("-----------------")
cat(sep = "\n")
cat(sep = "\n")

default_table <- table(wta$tourney_category, wta$Default)
default_table
cat(sep = "\n")

default_1000(tourney_category="ITF Women's World Tennis Tour",title_default="Tourney Category - ITF Women's World Tennis Tour:")
default_1000(tourney_category="WTA 125 Tournaments",title_default="Tourney Category - WTA 125 Tournaments")
default_1000(tourney_category="WTA Tour",title_default="Tourney Category - WTA Tour")

# Reference: ITF Women's World Tennis Tour
Default_Ref <- default_table["ITF Women's World Tennis Tour", "Default"]
Matches_Ref <- default_table["ITF Women's World Tennis Tour", "Default"] + default_table["ITF Women's World Tennis Tour", "No Default"]

# Tourney Category: WTA 125 Tournaments vs ITF Women's World Tennis Tour
Default_A <- default_table["WTA 125 Tournaments", "Default"]
Matches_A <- default_table["WTA 125 Tournaments", "Default"] + default_table["WTA 125 Tournaments", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.WTA_125 <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Tourney Category: WTA Tour vs ITF Women's World Tennis Tour
Default_A <- default_table["WTA Tour", "Default"]
Matches_A <- default_table["WTA Tour", "Default"] + default_table["WTA Tour", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.WTA_Tour <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.WTA_125,"WTA 125 Tournaments","ITF Women's World Tennis Tour","Tourney Category")
print.epi(epi.WTA_Tour,"WTA Tour","ITF Women's World Tennis Tour","Tourney Category")

rm(epi.WTA_125, epi.WTA_Tour)
rm(default_table, Default_Ref, Matches_Ref, Default_A, Matches_A, default_epiR)

# VARIABLE Surface

cat(sep = "\n")
cat("SURFACE:")
cat(sep = "\n")
cat("--------")
cat(sep = "\n")
cat(sep = "\n")

default_table <- table(wta$surface, wta$Default)
default_table
cat(sep = "\n")

default_1000(surface="Carpet",title_default="Surface - Carpet:")
default_1000(surface="Clay",title_default="Surface - Clay:")
default_1000(surface="Grass",title_default="Surface - Grass:")
default_1000(surface="Hard",title_default="Surface - Hard:")

# Reference: Hard
Default_Ref <- default_table["Hard", "Default"]
Matches_Ref <- default_table["Hard", "Default"] + default_table["Hard", "No Default"]

# Surface: Carpet vs Hard
Default_A <- default_table["Carpet", "Default"]
Matches_A <- default_table["Carpet", "Default"] + default_table["Carpet", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Carpet <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Clay vs Hard
Default_A <- default_table["Clay", "Default"]
Matches_A <- default_table["Clay", "Default"] + default_table["Clay", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Clay <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface: Grass vs Hard
Default_A <- default_table["Grass", "Default"]
Matches_A <- default_table["Grass", "Default"] + default_table["Grass", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Grass <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Carpet,"Carpet","Hard","Surface")
print.epi(epi.Clay,"Clay","Hard","Surface")
print.epi(epi.Grass,"Grass","Hard","Surface")

rm(epi.Carpet, epi.Clay, epi.Grass)
rm(default_table, Default_Ref, Matches_Ref, Default_A, Matches_A, default_epiR)

# VARIABLE Round Level

cat(sep = "\n")
cat("ROUND LEVEL:")
cat(sep = "\n")
cat("------------")
cat(sep = "\n")
cat(sep = "\n")

default_table <- table(wta$round_level, wta$Default)
default_table
cat(sep = "\n")

default_1000(round_level="Final Round",title_default="Round Level - Final Round:")
default_1000(round_level="Preliminary Round",title_default="Round Level - Preliminary Round")
default_1000(round_level="Qualifying Round",title_default="Round Level - Qualifying Round")

# Reference: Preliminary Round
Default_Ref <- default_table["Preliminary Round", "Default"]
Matches_Ref <- default_table["Preliminary Round", "Default"] + default_table["Preliminary Round", "No Default"]

# Round Level: Final Round vs Preliminary Round
Default_A <- default_table["Final Round", "Default"]
Matches_A <- default_table["Final Round", "Default"] + default_table["Final Round", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Final <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Round Level: Qualifying Round vs Preliminary Round
Default_A <- default_table["Qualifying Round", "Default"]
Matches_A <- default_table["Qualifying Round", "Default"] + default_table["Qualifying Round", "No Default"]
default_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Qualifying <- epi.2by2(dat=default_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Final,"Final Round","Preliminary Round","Round Level")
print.epi(epi.Qualifying,"Qualifying Round","Preliminary Round","Round Level")

rm(epi.Final, epi.Qualifying)
rm(default_table, Default_Ref, Matches_Ref, Default_A, Matches_A, default_epiR)

# VARIABLE Difference Age

# Default per 1000 matches:

cat(sep = "\n")
cat("AGE DIFFERENCE BETWEEN THE WINNING AND LOSING TENNIS PLAYERS:")
cat(sep = "\n")
cat("-------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

default_table <- table(wta$dif_age_0.2, wta$Default)
default_table
cat(sep = "\n")

default_1000(dif_age_f=0.2,title_default = "Age Difference < 0.2 y.o. :")
default_1000(dif_age_i=0.2,title_default ="Age Difference >= 0.2 y.o. :")

# Reference: Age Difference < 0.2 y.o.
Default_Ref <- default_table["No", "Default"]
Matches_Ref <- default_table["No", "Default"] + default_table["No", "No Default"]

# Age Difference: < 0.2 y.o. vs >= 0.2 y.o.

Default_A <- default_table["Yes", "Default"]
Matches_A <- default_table["Yes", "Default"] + default_table["Yes", "No Default"]
def_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=def_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= 0.2 y.o.","< 0.2 y.o.","Age Difference")

rm(epi.Yes)
rm(default_table, Default_Ref, Matches_Ref, Default_A, Matches_A, def_epiR)

# VARIABLE Difference Rank

# Default per 1000 matches:

cat(sep = "\n")
cat("DIFFERENCE IN RANKING POSITIONS BETWEEN THE WINNING AND THE LOSING TENNIS PLAYER:")
cat(sep = "\n")
cat("---------------------------------------------------------------------------------")
cat(sep = "\n")
cat(sep = "\n")

default_table <- table(wta$dif_rank_70, wta$Default)
default_table
cat(sep = "\n")

default_1000(dif_rank_f=-45,title_default ="Difference in ranking positions between the winning and the losing tennis player < -45")
default_1000(dif_rank_i=-45,title_default ="Difference in ranking positions between the winning and the losing tennis player >= -45")

# Reference: Difference in ranking positions between the winning and the losing tennis player < -45
Default_Ref <- default_table["No", "Default"]
Matches_Ref <- default_table["No", "Default"] + default_table["No", "No Default"]

# Difference in ranking positions between the winning and the losing tennis player < -45 vs >= -45

Default_A <- default_table["Yes", "Default"]
Matches_A <- default_table["Yes", "Default"] + default_table["Yes", "No Default"]
def_epiR <- c(Default_A, Matches_A, Default_Ref, Matches_Ref)
epi.Yes <- epi.2by2(dat=def_epiR, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

print.epi(epi.Yes,">= -45","< -45","Difference in ranking positions between the winning and the losing tennis player")

rm(epi.Yes)
rm(default_table, Default_Ref, Matches_Ref, Default_A, Matches_A, def_epiR)

sink()
           
