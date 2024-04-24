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


## ADULTS
 
# what's missing?
vis_miss(only_adults, sort_miss = TRUE)

## regression
model_adult <- svyglm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED+year, design=subset_adult, na.action = na.omit)

summ(model_adult)

summ(model_adult, robust = "HC1") #robust standard errors 

summ(model_adult, confint = TRUE, digits = 3) #In many cases, you’ll learn more by looking at confidence intervals than p-values. You can request them from summ. default is 95% CIs

summ(model_adult, confint = TRUE, pvals = FALSE) #DROP the p values all together

# THE FOREST GRAPH
plot_summs(model_adult)
plot_summs(model_adult, inner_ci_level = .95)
plot_summs(model_adult, robust = TRUE)

# plot coefficient uncertainty as normal distributions
plot_summs(model_adult, plot.distributions = TRUE, inner_ci_level = .95)

# table output for Word and RMarkdown documents
## error is in the parenthesis
export_summs(model_adult, scale = TRUE)

# confidence intervals instead of standard errors
export_summs(model_adult, scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]")

# renaming the forest plot labels
forest_adult <- plot_summs(
  point.size = 3,
  fontsize=8,
  colors = "skyblue3",
  model_adult, coefs = c("Age: Older Adult
                                 Middle-Aged (ref)" = "ageolder adult",
                         
                         "Age: Young Adult
                                 Middle-Aged (ref)" = "ageyoung adult",
                         
                         "Gender: Male
                                 Gender: Female (ref)" = "gendermale",
                         
                         "Ethnicity: Mexican American
                                 Non-Hispanic White (ref)" = "ethnicityMexican American",
                         
                         "Ethnicity: Non-Hispanic Black
                                 Non-Hispanic White (ref)" = "ethnicityNon-Hispanic Black",
                         
                         "Ethnicity: Other Hispanic
                                 Non-Hispanic White (ref)" = "ethnicityOther Hispanic",
                         
                         "Ethnicity: Other or Multi
                                 Non-Hispanic White (ref)" = "ethnicityOther or Multi",
                         
                         "Family Income to Poverty Ratio: 2x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 2x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 3x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 3x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 4x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 4x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 5x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 5x poverty threshold",
                         
                         "Family Income to Poverty Ratio: more than 5x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income more than 5x poverty threshold",
                         
                         "Citizenship Status: Not U.S. Citizen
                                 U.S. Citizen by birth or naturalization (ref)" = "citizenshipnot U,S, citizen",
                         
                         "Participant Education: High School Grad/GED
                                  College Grad or Above (ref)" = "adultEDhigh school grad/GED",
                         
                         "Participant Education: Less than High School/GED
                                  College Grad or Above (ref)" = "adultEDless than HS/GED",
                         
                         "Participant Education: Some College or AA
                                  College Grad or Above (ref)" = "adultEDsome college or AA",
                         
                         "Year of Survey: 2003
                              Year 1999 (ref)" = "year2003",
                         
                         "Year of Survey: 2005
                              Year 1999 (ref)" = "year2005",
                         
                         "Year of Survey: 2007
                              Year 1999 (ref)" = "year2007",
                         
                         "Year of Survey: 2009
                              Year 1999 (ref)" = "year2009",
                         
                         "Year of Survey: 2011
                              Year 1999 (ref)" = "year2011",
                         
                         "Year of Survey: 2013
                              Year 1999 (ref)" = "year2013",
                         
                         "Year of Survey: 2015
                              Year 1999 (ref)" = "year2015",
                         
                         "Year of Survey: 2017
                              Year 1999 (ref)" = "year2017"),
  
  scale = TRUE, robust = TRUE)

forest_adult










## CHILDREN
# what's missing
vis_miss(only_child, sort_miss = TRUE)
# check this but get rid of the adult variables:
only_child_x <- select(only_child, fpl, monoEthyl, refED, citizenship, year, age, gender, psu, persWeight, strata, ethnicity)
vis_miss(only_child_x, sort_miss = TRUE)

# regression
model_child <- svyglm(log(monoEthyl)~refED+RIDAGEYR+gender+ethnicity+fpl+citizenship+year, design=subset_child, na.action = na.omit)
## using continuous RIDAGEYR rather than "age" categories, since all are children

summ(model_child)

summ(model_child, robust = "HC1") #robust standard errors 


summ(model_child, confint = TRUE, pvals = TRUE) #DROP the p values all together

# THE GRAPH
plot_summs(model_child)
plot_summs(model_child, inner_ci_level = .95)
plot_summs(model_child, robust = TRUE)

# plot coefficient uncertainty as normal distributions
plot_summs(model_child, plot.distributions = TRUE, inner_ci_level = .95)

# table output for Word and RMarkdown documents
## error is in the parenthesis
export_summs(model_child, scale = TRUE)

# confidence intervals instead of standard errors
export_summs(model_child, scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]")

### Renaming the forest plot labels
forest_child <- plot_summs(
  point.size = 3,
  fontsize=8,
  colors = "skyblue3",
  model_child, coefs = c("Household Education Partial College and Below
                                 College and Beyond (ref)" = "refEDpartial college and below", 
                         
                         "Age in years (continuous)
                                 (mean-centered and scaled by 1 SD)" = "RIDAGEYR",
                         
                         "Gender: Male
                                 Gender: Female (ref)" = "gendermale",
                         
                         "Ethnicity: Mexican American
                                 Non-Hispanic White (ref)" = "ethnicityMexican American",
                         
                         "Ethnicity: Non-Hispanic Black
                                 Non-Hispanic White (ref)" = "ethnicityNon-Hispanic Black",
                         
                         "Ethnicity: Other Hispanic
                                 Non-Hispanic White (ref)" = "ethnicityOther Hispanic",
                         
                         "Ethnicity: Other or Multi
                                 Non-Hispanic White (ref)" = "ethnicityOther or Multi",
                         
                         "Family Income to Poverty Ratio: 2x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 2x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 3x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 3x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 4x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 4x poverty threshold",
                         
                         "Family Income to Poverty Ratio: 5x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income 5x poverty threshold",
                         
                         "Family Income to Poverty Ratio: more than 5x Poverty threshold
                                 At poverty threshold (ref)" = "fplfamily income more than 5x poverty threshold",
                         
                         "Citizenship Status: Not U.S. Citizen
                                 U.S. Citizen by birth or naturalization (ref)" = "citizenshipnot U,S, citizen",
                         
                         "Year of Survey: 2003
                              Year 1999 (ref)" = "year2003",
                         
                         "Year of Survey: 2005
                              Year 1999 (ref)" = "year2005",
                         
                         "Year of Survey: 2007
                              Year 1999 (ref)" = "year2007",
                         
                         "Year of Survey: 2009
                              Year 1999 (ref)" = "year2009",
                         
                         "Year of Survey: 2011
                              Year 1999 (ref)" = "year2011",
                         
                         "Year of Survey: 2013
                              Year 1999 (ref)" = "year2013",
                         
                         "Year of Survey: 2015
                              Year 1999 (ref)" = "year2015",
                         
                         "Year of Survey: 2017
                              Year 1999 (ref)" = "year2017"),
  
  scale = TRUE, robust = TRUE)

forest_child









## comparing AICs
# ADULTS
ols_adult <- (svyglm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED+year, design=subset_adult, na.action = na.omit))
ols_adult
# this gives an AIC of 49,140

# without year
ols_adult <- (svyglm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED, design=subset_adult, na.action = na.omit))
ols_adult
# this gives an AIC of 50,530

# with refED, not adultED
ols_adult <- (svyglm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+refED+year, design=subset_adult, na.action = na.omit))
ols_adult
# this gives an AIC of 49,610










#CHILD
ols_child <- svyglm(log(monoEthyl)~refED+RIDAGEYR+gender+ethnicity+fpl+citizenship+year, design=subset_child, na.action = na.omit)
ols_child 
# this gives an AIC of 23,580

## without year
ols_child <- svyglm(log(monoEthyl)~refED+RIDAGEYR+gender+ethnicity+fpl+citizenship, design=subset_child, na.action = na.omit)
ols_child 
# this gives an AIC of 24,430
