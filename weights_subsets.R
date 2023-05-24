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

## Load the data
NHANES <- read_csv(here("cleaned_data","NHANES.csv"))

NHANES$ethnicity <- as.factor(NHANES$ethnicity)
is.factor(NHANES$ethnicity)

NHANES$year <- as.factor(NHANES$year)
is.factor(NHANES$year)

NHANES$ethnicity <- relevel(NHANES$ethnicity, ref = "Non-Hispanic White")
NHANES$year <- relevel(NHANES$year, ref = "2001")



# Survey Design
nhc <- svydesign(id=~SDMVPSU, weights=~WTINT2YR,strata=~SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=NHANES)
nhc

summary(nhc)


### Subset for CHILDREN
subset_child <- subset(nhc, RIDAGEYR < 19)
only_child <- NHANES %>% filter(RIDAGEYR<19)

### Subset for ADULTS
subset_adult <- subset(nhc, RIDAGEYR >= 19)
only_adults <- NHANES %>% filter(RIDAGEYR>=19)



