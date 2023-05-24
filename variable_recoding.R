# packages

library(tidyverse)
library(here)
library(ggpubr) # for some graphic applications that extend ggplot2
library(janitor)
library(broom) # used to make tables
library(knitr) # used to make table
library(car) # has leveneTest 
library(foreign) # to read in NHANES data
library(rstanarm) # for the model fitting
library(jtools) # Load jtools, for forest plots
library(sandwich) # needed for robust standard errors in forest plot
library(huxtable) # needed to be able to export table of forest plot values
library(broom.mixed) # used in making tables
library(visdat)
library(ggplot2)

library(haven) # for reading SAS XPT file from NHANES website
library(survey) # for using survey weights
library(dplyr) # for data wrangling
library(forcats)

library("remotes")
library("svrepmisc")

## LOAD THE DATASET 

# Years 2001-2016
## demographics file
## phthalates, urine file

# Downloaded from CDC National Health and Examination Survey, [NHANES Questionnaires, Datasets, and Related Documentation](https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)

# 2001-2002
demographics01 <- read.xport(here("data","DEMO_B_2001_2002.XPT"))
phthalates01 <- read.xport(here("data","PHPYPA_B_2001_2002.XPT"))
nhanes01 <- merge(phthalates01,demographics01,all.x=T)

# 2003-2004
demographics03 <- read.xport(here("data","DEMO_C_2003_2004.XPT"))
phthalates03 <- read.xport(here("data","L24PH_C_2003_2004.XPT"))
nhanes03 <- merge(phthalates03,demographics03,all.x=T)

# 2005-2006
demographics05 <- read.xport(here("data","DEMO_D_2005_2006.XPT"))
phthalates05 <- read.xport(here("data","PHTHTE_D_2005_2006.XPT"))
nhanes05 <- merge(phthalates05,demographics05,all.x=T)

# 2007-2008
demographics07 <- read.xport(here("data","DEMO_E_2007_2008.XPT"))
phthalates07 <- read.xport(here("data","PHTHTE_E_2007_2008.XPT"))
nhanes07 <- merge(phthalates07,demographics07,all.x=T)

# 2009-2010
demographics09 <- read.xport(here("data","DEMO_F_2009_2010.XPT"))
phthalates09 <- read.xport(here("data","PHTHTE_F_2009_2010.XPT"))
nhanes09 <- merge(phthalates09,demographics09,all.x=T)

# 2011-2012
demographics11 <- read.xport(here("data","DEMO_G_2011_2012.XPT"))
phthalates11 <- read.xport(here("data","PHTHTE_G_2011_2012.XPT"))
nhanes11 <- merge(phthalates11,demographics11,all.x=T)

# 2013-2014
demographics13 <- read.xport(here("data","DEMO_H_2013_2014.XPT"))
phthalates13 <- read.xport(here("data","PHTHTE_H_2013_2014.XPT"))
nhanes13 <- merge(phthalates13,demographics13,all.x=T)

# 2015-2016
demographics15 <- read.xport(here("data","DEMO_I_2015_2016.XPT"))
phthalates15 <- read.xport(here("data","PHTHTE_I_2015_2016.XPT"))
nhanes15 <- merge(phthalates15,demographics15,all.x=T)


## Add a column with the YEAR to each data file:
nhanes01$year <- 2001
nhanes03$year <- 2003
nhanes05$year <- 2005
nhanes07$year <- 2007
nhanes09$year <- 2009
nhanes11$year <- 2011
nhanes13$year <- 2013
nhanes15$year <- 2015


## Choose the variables to select from each data file

## Key step to ensure proper weighting =
# persWeight = person-level weight ("WTINT2YR")
# psu = primary sampling unit ("SDMVPSU")
# strata = strata-level sampling unit ("SDMVSTRA")

var<-c('year','WTINT2YR','SDMVPSU','SDMVSTRA','DMDEDUC2','DMDHREDU','RIDAGEYR','RIAGENDR','RIDRETH1','INDFMPIR','DMDCITZN','URXMEP')
nhanes01s<-subset(nhanes01,select=var)
nhanes03s<-subset(nhanes03,select=var)
nhanes05s<-subset(nhanes05,select=var)
nhanes07s<-subset(nhanes07,select=var)
nhanes09s<-subset(nhanes09,select=var)
nhanes11s<-subset(nhanes11,select=var)
nhanes13s<-subset(nhanes13,select=var)
nhanes15s<-subset(nhanes15,select=var)

## Create one dataset for NHANES demographics and phthalates, year 2001-2016
fullNHANES <- rbind(nhanes01s,nhanes03s,nhanes05s,nhanes07s,nhanes09s,nhanes11s,nhanes13s,nhanes15s)

## Copy and rename variables so they are more intuitive. 

# fpl = INDFMPIR (ratio of family income to poverty,ranges from 0 to 5 (i.e., 0% - 500%))
# age = RIDAGEYR (age in years at screening)
# gender = RIAGENDR
# persWeight = WTINT2YR (person-level weight)
# psu = SDMVPSU (primary sampling unit)
# strata = SDMVSTRA (strata-level sampling unit)
# refED = DMDHREDU (the household reference person's education level)
# adultED = DMDEDUC2 (the highest grade/level of education completed by participants 20 years and older)
# ethnicity = RIDRETH1 (race/Hispanic origin)
# citizenship = DMDCITZN (citizenship status)
# monoEthyl = URXMEP (mono-ethyl phthalate, ng/mL)
# year = year the observation took place (*this was added by me when I merged the years of data)

fullNHANES$fpl <- fullNHANES$INDFMPIR
fullNHANES$age <- fullNHANES$RIDAGEYR
fullNHANES$gender <- fullNHANES$RIAGENDR
fullNHANES$persWeight <- fullNHANES$WTINT2YR
fullNHANES$psu <- fullNHANES$SDMVPSU
fullNHANES$strata <- fullNHANES$SDMVSTRA
fullNHANES$refED <- fullNHANES$DMDHREDU
fullNHANES$adultED <- fullNHANES$DMDEDUC2
fullNHANES$ethnicity <- fullNHANES$RIDRETH1
fullNHANES$citizenship <- fullNHANES$DMDCITZN
fullNHANES$monoEthyl <- fullNHANES$URXMEP

## RE-CATEGORIZE the variables:

## Household reference person's education level

fullNHANES$refED = factor(ifelse(fullNHANES$DMDHREDU %in% 1:4,"partial college and below",
                                 ifelse(fullNHANES$DMDHREDU==5,"college and beyond",NA)),
                          levels=c("partial college and below","college and beyond"))

summary(fullNHANES$refED)

## Highest grade/level of education completed by participants 20 years and older

fullNHANES$adultED = factor(ifelse(fullNHANES$DMDEDUC2 >=1 & fullNHANES$DMDEDUC2 <=2, "less than HS/GED",
                                   ifelse(fullNHANES$DMDEDUC2 ==3, "high school grad/GED", 
                                          ifelse(fullNHANES$DMDEDUC2 ==4, "some college or AA",
                                                 ifelse(fullNHANES$DMDEDUC2 ==5, "college grad or above", NA)))),
                            levels=c("less than HS/GED", "high school grad/GED", "some college or AA", "college grad or above"))
summary(fullNHANES$adultED)

## Age in years at screening
# Based on the summary statistics of the variable "RIDAGEYR": the youngest participant is 3 years old, the oldest participant is 85 years old, and the median age is 31 years old. 

fullNHANES$age = factor(ifelse(fullNHANES$RIDAGEYR >=65,"older adult",
                               ifelse(fullNHANES$RIDAGEYR >25 & fullNHANES$RIDAGEYR<=64,"middle-aged",
                                      ifelse(fullNHANES$RIDAGEYR >=19 & fullNHANES$RIDAGEYR<=25,"young adult",
                                             ifelse(fullNHANES$RIDAGEYR <=18,"child",NA)))),
                        levels=c("child","young adult", "middle-aged", "older adult"))
summary(fullNHANES$age)

is.factor(fullNHANES$age)

fullNHANES$age <- relevel(fullNHANES$age, ref = "child")


## Gender

fullNHANES$gender = factor(ifelse(fullNHANES$RIAGENDR ==1, "male",
                                  ifelse(fullNHANES$RIAGENDR ==2, "female", NA)),
                           levels=c("male", "female"))
summary(fullNHANES$gender)


## Race/ethnicity

fullNHANES$ethnicity = factor(ifelse(fullNHANES$RIDRETH1 ==1, "Mexican American",
                                     ifelse(fullNHANES$RIDRETH1 ==2, "Other Hispanic",
                                            ifelse(fullNHANES$RIDRETH1 ==3, "Non-Hispanic White", 
                                                   ifelse(fullNHANES$RIDRETH1 ==4, "Non-Hispanic Black",
                                                          ifelse(fullNHANES$RIDRETH1 ==5, "Other or Multi", NA))))),
                              levels=c("Non-Hispanic White", "Non-Hispanic Black", "Mexican American", "Other Hispanic", "Other or Multi"))
summary(fullNHANES$ethnicity)
is.factor(fullNHANES$ethnicity)

fullNHANES$ethnicity <- relevel(fullNHANES$ethnicity, ref = "Non-Hispanic White")

## Citizenship Status
fullNHANES$citizenship = factor(ifelse(fullNHANES$DMDCITZN ==1, "birth or naturalization",
                                       ifelse(fullNHANES$DMDCITZN ==2, "not U,S, citizen", NA)),
                                levels=c("birth or naturalization", "not U,S, citizen"))
summary(fullNHANES$citizenship)

## Ratio of Family Income to Poverty Level
fullNHANES$fpl = factor(ifelse(fullNHANES$INDFMPIR < 1,"at poverty threshold",
                               ifelse(fullNHANES$INDFMPIR >=1 & fullNHANES$INDFMPIR <2, "family income 2x poverty threshold",
                                      ifelse(fullNHANES$INDFMPIR >=2 & fullNHANES$INDFMPIR <3, "family income 3x poverty threshold",
                                             ifelse(fullNHANES$INDFMPIR >=3 & fullNHANES$INDFMPIR <4, "family income 4x poverty threshold",
                                                    ifelse(fullNHANES$INDFMPIR >=4 & fullNHANES$INDFMPIR <5, "family income 5x poverty threshold",
                                                           ifelse(fullNHANES$INDFMPIR ==5, "family income more than 5x poverty threshold", NA)))))),
                        levels=c("at poverty threshold", "family income 2x poverty threshold", "family income 3x poverty threshold", "family income 4x poverty threshold", "family income 5x poverty threshold", "family income more than 5x poverty threshold"))

summary(fullNHANES$fpl)


## Year of survey

fullNHANES$year = factor(ifelse(fullNHANES$year ==2001, "2001",
                                ifelse(fullNHANES$year ==2003, "2003",
                                       ifelse(fullNHANES$year ==2005, "2005",
                                              ifelse(fullNHANES$year ==2007, "2007",
                                                     ifelse(fullNHANES$year ==2009, "2009",
                                                            ifelse(fullNHANES$year ==2011, "2011",
                                                                   ifelse(fullNHANES$year ==2013, "2013",
                                                                          ifelse(fullNHANES$year ==2015, "2015", NA)))))))),
                         levels=c("2001","2003", "2005", "2007", "2009","2011", "2013", "2015"))
summary(fullNHANES$year)

is.factor(fullNHANES$year)

fullNHANES$year <- relevel(fullNHANES$year, ref = "2001")


## Save a dataset with renamed, recategorized variables, and only the variables I am using in my analysis

# [don't need to write a new csv]
# write.csv(fullNHANES, "NHANES.csv")
