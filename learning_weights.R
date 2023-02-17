# for reading SAS XPT file from NHANES website
## haven::read_xpt

install.packages("haven")
library(haven)

# for using survey weights
# survey::svydesign, svymean, svyglm

install.packages("survey")
library(survey)

# for data wrangling
# dplyr::select, mutate, select, recode

library(dplyr)

### LOAD THE DATASET ###
# import NHANES demographic data
nhanesDemo <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT"))

# DATA WRANGLING #
# copy and rename variables so they are more intuitive
# fpl = percent of the federal poverty level (ranges from 0 to 5 (i.e., 0% -500%))
# age = age in years
# gender = male or female
# persWeight = person-level weight
# psu = primary sampling unit
# strata = strata-level sampling unit

nhanesDemo$fpl <- nhanesDemo$INDFMPIR
nhanesDemo$age <- nhanesDemo$RIDAGEYR
nhanesDemo$gender <- nhanesDemo$RIAGENDR
nhanesDemo$persWeight <- nhanesDemo$WTINT2YR
nhanesDemo$psu <- nhanesDemo$SDMVPSU
nhanesDemo$strata <- nhanesDemo$SDMVSTRA

# Since there are 47 variables, we will select only the variables we will use in this analysis

nhanesAnalysis <- nhanesDemo %>% 
  select(fpl, age, gender, persWeight, psu, strata)

# Recode gender
nhanesAnalysis <- nhanesAnalysis %>% 
  mutate(gender = dplyr::recode(gender, '1' = 0L,
                                '2' = 1L))

## 1 >> 0, = male
## 2 >> 1, = female

# Convert "gender" to a factor variable. We need to do this so it isn't treated as a continuous variable in our analyses

nhanesAnalysis$gender <- as.factor(nhanesAnalysis$gender)

# SURVEY WEIGHTS # 

# Here we use "svydesign" to assign the weights. We will use this new design variable "nhanesDesign" when running our analyses

nhanesDesign <- svydesign(id = ~psu,
                          strata = ~strata,
                          weights = ~persWeight,
                          nest = TRUE,
                          data = nhanesAnalysis)

# Here we use "subset" to tell "nhanesDesign" that we want to only look at a specific sub-population 
## (i.e., those age between 18-79 y.o.)
# This is important to do. If you don't do this and just restrict it in a different way, 
## your estimates won't have correct SEs

ageDesign <- subset(nhanesDesign, age > 17 & 
                                  age <80)

# STATISTICS # 

# We will use "svymean" to calculate the population mean for age. The na.rm argument "TRUE" excludes 
## missing values from the calculation.
# We see that the mean age is 45.648 and the standard error is 0.5131

svymean(~age, ageDesign, na.rm = TRUE)

# Since gender is a factor variable, "svymean" will treat it as such and give us the proportion of women
# We see that men are 48.601% and women are 51.399% of the population in this age of 18-79 y.o.

svymean(~gender, ageDesign, na.rm = TRUE)

# Now we will run a general linear model (glm) with a gaussian link function. 
# We tell svyglm that nhanesAnalysis is the dataset to use and to apply the "svydesign" object "ageDesign"
# I won't dive into the results here, but you can see that age is positively correlated with FPL and that 
# women are predicted to have lower FPL than men

output <- svyglm(fpl ~ age + gender,
                 family = gaussian(),
                 data = nhanesAnalysis,
                 design = ageDesign)

summary(output)


