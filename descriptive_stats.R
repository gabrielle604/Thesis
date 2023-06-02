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

#install.packages("skimr")
library(skimr)

#install.packages("gtsummary")
library(gtsummary)

#install.packages("srvyr")
library(srvyr)


## distribution of MONO-ETHYL PHTHALATE
## raw
hist(NHANES$monoEthyl, col = 'skyblue3', border = "white", xlab = 'Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Mono-ethyl Phthalate')

## raw, just children
hist(only_child$monoEthyl, col = 'skyblue3', border = "white", breaks = 50, xlab = 'Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Mono-ethyl Phthalate')

## raw, just adults
hist(only_adults$monoEthyl, col = 'skyblue3', border = "white", breaks = 50, xlab = 'Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Mono-ethyl Phthalate')

## dotchart
dotchart(only_child$monoEthyl, col = 'skyblue3', col.line = "transparent", xlab = 'Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Mono-ethyl Phthalate')

ggplot(only_child,aes(monoEthyl))+geom_dotplot(binwidth=100)+theme_minimal()


## logged
hist(log(NHANES$URXMEP), col = 'skyblue3', border = "white", xlab = '(Logged) Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Logged Mono-ethyl Phthalate')

## logged, just children
hist(log(only_child$URXMEP), col = 'skyblue3', border = "white", xlab = '(Logged) Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Logged Mono-ethyl Phthalate')

## logged, just adults
hist(log(only_adults$URXMEP), col = 'skyblue3', border = "white", xlab = '(Logged) Mono-ethyl Phthalate (ng/mL)', ylab = 'Frequency', main = 'Distribution of Logged Mono-ethyl Phthalate')


## density plot
dens1<-svysmooth(~log(monoEthyl), design=nhc)
plot(dens1)

## density plot: just children
dens2<-svysmooth(~log(monoEthyl), design=subset_child)
plot(dens2)

## density plot: just adults
dens3<-svysmooth(~log(monoEthyl), design=subset_adult)
plot(dens3)

## Scatterplot with the sampling weights corresponding to the bubble size.
svyplot(~log(monoEthyl)+RIDAGEYR, nhc, style="bubble")

## Quantiles
# all, raw
svyquantile(~URXMEP, design = nhc, na = TRUE, c(.1,.5,.9),ci=TRUE)
#all, logged monoethyl
svyquantile(~(log(monoEthyl)), design = nhc, na = TRUE, c(.1,.5,.9),ci=TRUE)

#children, raw
svyquantile(~URXMEP, design = subset_child, na = TRUE, c(.1,.5,.9),ci=TRUE)
#children, logged monoethyl
svyquantile(~(log(monoEthyl)), design = subset_child, na = TRUE, c(.1,.5,.9),ci=TRUE)

#adults, raw
svyquantile(~URXMEP, design = subset_adult, na = TRUE, c(.1,.5,.9),ci=TRUE)
#adult, logged monoethyl
svyquantile(~(log(monoEthyl)), design = subset_adult, na = TRUE, c(.1,.5,.9),ci=TRUE)









## Bivariate analysis for CHILDREN:

## AGE
noNAs = only_child %>% filter(!is.na(RIDAGEYR)) %>% filter(!is.na(monoEthyl))

box_age <- ggplot(data = noNAs, design=subset_child,
                  aes(x=log(monoEthyl), y=(RIDAGEYR))) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_point() +
  geom_smooth() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Age of Participant") +
  ggtitle("Age of participant and Logged Phthalate Level")

box_age

## EDUCATION
noNAs = only_child %>% filter(!is.na(refED)) %>% filter(!is.na(refED))

box_refED <- ggplot(data = noNAs, design=subset_child,
                    aes(x=log(monoEthyl), y=refED, fill=refED)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Household Reference Person's Education Level") +
  ggtitle("Household Reference Person's Education Level and Logged Phthalate Level")


box_refED

box_refED + scale_fill_discrete(breaks=c("partial college and below","college and beyond"))


## GENDER

noNAs = only_child %>% filter(!is.na(gender)) %>% filter(!is.na(gender))

box_gender <- ggplot(data = noNAs, design=subset_child,
                     aes(x=log(monoEthyl), y=gender, fill=gender)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Gender") +
  ggtitle("Participant Gender and Logged Phthalate Level")

box_gender

box_gender + scale_fill_discrete(breaks=c("male","female"))

## ETHNICITY
noNAs = only_child %>% filter(!is.na(ethnicity)) %>% filter(!is.na(ethnicity))

box_eth <- ggplot(data = noNAs, design=subset_child,
                  aes(x=log(monoEthyl), y=ethnicity, fill=ethnicity)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Race/Ethnicity") +
  ggtitle("Participant Race/Ethnicity and Logged Phthalate Level")


box_eth

box_eth + scale_fill_discrete(breaks=c("Other or Multi","Other Hispanic", "Non-Hispanic Black", "Mexican American", "Non-Hispanic White"))

## FPL
noNAs = only_child %>% filter(!is.na(fpl)) %>% filter(!is.na(fpl))

box_fpl <- ggplot(data = noNAs, design=subset_child,
                  aes(x=log(monoEthyl), y=fpl, fill=fpl)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Family Income to Poverty Ratio") +
  ggtitle("Participant Family Income to Poverty Ratio and Logged Phthalate Level")

box_fpl + scale_fill_discrete(breaks=c("family income more than 5x poverty threshold",
                                       "family income 5x poverty threshold",
                                       "family income 4x poverty threshold",
                                       "family income 3x poverty threshold",
                                       "family income 2x poverty threshold",
                                       "at poverty threshold"))

## CITIZENSHIP
noNAs = only_child %>% filter(!is.na(citizenship)) %>% filter(!is.na(citizenship))

box_cit <- ggplot(data = noNAs, design=subset_child,
                  aes(x=log(monoEthyl), y=citizenship, fill=citizenship)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Citizenship") +
  ggtitle("Participant Citizenship and Logged Phthalate Level")

box_cit + scale_fill_discrete(breaks=c("not U,S, citizen","birth or naturalization"))

## YEAR
noNAs = only_child %>% filter(!is.na(year)) %>% filter(!is.na(year))

box_year <- ggplot(data = noNAs, design=subset_child,
                   aes(x=log(monoEthyl), y=year, fill=year)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Year of Participant Survey") +
  ggtitle("Year of Participant Survey and Logged Phthalate Level")

box_year + scale_fill_discrete(breaks=c("2015","2013","2011","2009","2007","2005","2003","2001"))

svyboxplot(~log(monoEthyl)~factor(year), subset_child, all.outliers=TRUE)

# ////////////
summary(as.factor(NHANES$gender))

svymean(~log(URXMEP), design = subset_child, na = TRUE)
svysd(~log(URXMEP), design = subset_child, na = TRUE)

svymean(~RIDAGEYR, design = subset_child, na = TRUE)
svysd(~RIDAGEYR, design = subset_child, na = TRUE)

svymean(~log(URXMEP), design = subset_adult, na = TRUE)
svysd(~log(URXMEP), design = subset_adult, na = TRUE)

svymean(~RIDAGEYR, design = subset_adult, na = TRUE)
svysd(~RIDAGEYR, design = subset_adult, na = TRUE)

summary(subset_child)
str(subset_child)


## TABLE
# descriptive statistics with categorical variables (replace ~fpl with each variable individually)

table2 <- svytable(~fpl, design = subset_child)

prop.table(table2)

skim(NHANES)

#### this does not account for the weights!!! 
only_child$ageCont <- only_child$RIDAGEYR
only_child$LOGmonoEthyl <- log(only_child$URXMEP)


only_child %>% tabyl(age, gender)

only_child %>% 
  select(RIDAGEYR,URXMEP) %>%  # keep only columns of interest
  tbl_summary(                                     # create summary table
    type = all_continuous() ~ "continuous2",       # indicate that you want to print multiple statistics 
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",                             # line 1: mean and SD
      "{median} ({p25}, {p75})",                   # line 2: median and IQR
      "{min}, {max}")                              # line 3: min and max
  )

only_child %>% 
  select(age, ageCont, gender, ethnicity, fpl, citizenship, refED, LOGmonoEthyl) %>%  # keep only columns of interest
  tbl_summary(                                     # create summary table
    type = all_continuous() ~ "continuous2",       # indicate that you want to print multiple statistics 
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",                             # line 1: mean and SD
      "{median} ({p25}, {p75})",                   # line 2: median and IQR
      "{min}, {max}")                              # line 3: min and max
  )


## Bivariate analysis for ADULTS:

## EDUCATION
noNAs = only_adults %>% filter(!is.na(adultED)) %>% filter(!is.na(adultED))

box_edu <- ggplot(data = noNAs, design=subset_adult,
                  aes(x=log(monoEthyl), y=adultED, fill=adultED)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Education Level") +
  ggtitle("Participant Education Level and Logged Phthalate Level")

box_edu

box_edu + scale_fill_discrete(breaks=c("some college or AA","less than HS/GED",
                                       "high school grad/GED","college grad or above"))

## AGE
noNAs = only_adults %>% filter(!is.na(age)) %>% filter(!is.na(monoEthyl))

age_ordered <- ordered(noNAs$age, levels = c("child", "young adult", "middle-aged", "older adult"))

box_age <- ggplot(data = noNAs, design=subset_adult,
                  aes(x=log(monoEthyl), y=age_ordered, fill=age_ordered)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Age of Participant") +
  ggtitle("Age of Participant and Logged Phthalate Level")

box_age 

box_age + scale_fill_discrete(breaks=c("older adult","middle-aged","young adult"))

## GENDER

noNAs = only_adults %>% filter(!is.na(gender)) %>% filter(!is.na(gender))

box_gender <- ggplot(data = noNAs, design=subset_adult,
                     aes(x=log(monoEthyl), y=gender, fill=gender)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Gender") +
  ggtitle("Participant Gender and Logged Phthalate Level")


box_gender

box_gender + scale_fill_discrete(breaks=c("male","female"))

## ETHNICITY
noNAs = only_adults %>% filter(!is.na(ethnicity)) %>% filter(!is.na(ethnicity))

box_eth <- ggplot(data = noNAs, design=subset_adult,
                  aes(x=log(monoEthyl), y=ethnicity, fill=ethnicity)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Race/Ethnicity") +
  ggtitle("Participant Race/Ethnicity and Logged Phthalate Level")


box_eth

box_eth + scale_fill_discrete(breaks=c("Other or Multi","Other Hispanic",
                                       "Non-Hispanic Black","Mexican American",
                                       "Non-Hispanic White"))

## FPL
noNAs = only_adults %>% filter(!is.na(fpl)) %>% filter(!is.na(fpl))

box_fpl <- ggplot(data = noNAs, design=subset_adult,
                  aes(x=log(monoEthyl), y=fpl, fill=fpl)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Family Income to Poverty Ratio") +
  ggtitle("Participant Family Income to Poverty Ratio and Logged Phthalate Level")

box_fpl

box_fpl + scale_fill_discrete(breaks=c("family income more than 5x poverty threshold",
                                       "family income 5x poverty threshold",
                                       "family income 4x poverty threshold",
                                       "family income 3x poverty threshold",
                                       "family income 2x poverty threshold",
                                       "at poverty threshold"))

## CITIZENSHIP
noNAs = only_adults %>% filter(!is.na(citizenship)) %>% filter(!is.na(citizenship))

box_cit <- ggplot(data = noNAs, design=subset_adult,
                  aes(x=log(monoEthyl), y=citizenship, fill=citizenship)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Participant Citizenship") +
  ggtitle("Participant Citizenship and Logged Phthalate Level")

box_cit

box_cit + scale_fill_discrete(breaks=c("not U,S, citizen", "birth or naturalization"))

## YEAR
noNAs = only_adults %>% filter(!is.na(year)) %>% filter(!is.na(year))

box_year <- ggplot(data = noNAs, design=subset_adult,
                   aes(x=log(monoEthyl), y=year, fill=year)) +
  scale_fill_brewer(palette="PuBuGn") +
  geom_boxplot() +
  theme(text = element_text(size=12)) +
  xlab("(logged) Mono-Ethyl Phthalate Level (ng/mL)") +
  ylab("Year of Participant Survey") +
  ggtitle("Year of Participant Survey and Logged Phthalate Level")

box_year

box_year + scale_fill_discrete(breaks=c("2015","2013","2011","2009","2007","2005","2003","2001"))


svyboxplot(~log(monoEthyl)~factor(year), subset_adult, all.outliers=TRUE)

# /////////////
summary(as.factor(only_child$gender))

svysd(~RIDAGEYR, design = subset_adult, na = TRUE)

svymean(~log(URXMEP), design = subset_adult, na = TRUE)
svysd(~log(URXMEP), design = subset_adult, na = TRUE)

svymean(~RIDAGEYR, design = subset_adult, na = TRUE)

svymean(~INDFMPIR, nhc, na = TRUE)

# descriptive statistics with categorical variables (replace ~fpl with each variable individually)
# adults
table2 <- svytable(~adultED, design = subset_adult)

prop.table(table2)

## TABLE
skim(only_adults)

#### this does not account for the weights!!! 

only_adults$ageCont <- only_adults$RIDAGEYR
only_adults$LOGmonoEthyl <- log(only_adults$URXMEP)

only_adults %>% tabyl(age, gender)

only_adults %>% 
  select(RIDAGEYR,URXMEP) %>%  # keep only columns of interest
  tbl_summary(                                     # create summary table
    type = all_continuous() ~ "continuous2",       # indicate that you want to print multiple statistics 
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",                             # line 1: mean and SD
      "{median} ({p25}, {p75})",                   # line 2: median and IQR
      "{min}, {max}")                              # line 3: min and max
  )

only_adults %>% 
  select(age, ageCont, gender, ethnicity, fpl, citizenship, refED, adultED, LOGmonoEthyl) %>%  # keep only columns of interest
  tbl_summary(                                     # create summary table
    type = all_continuous() ~ "continuous2",       # indicate that you want to print multiple statistics 
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",                             # line 1: mean and SD
      "{median} ({p25}, {p75})",                   # line 2: median and IQR
      "{min}, {max}")                              # line 3: min and max
  )


only_adults %>%
  as_survey(weights = c(WTINT2YR)) %>%
  group_by(gender) %>%
  summarize(n = survey_total())
# I have no idea how to interpret these numbers that get put out,, I think they are weighted though!






## BIVARIATE ANALYSES for the table (not figures)
# change "year" to each variable (ie: refED, RIDAGEYR,gender, ethnicity, fpl, citizenship)

## CHILDREN 
model_child_bivariate <- svyglm(log(monoEthyl)~year, design=subset_child, na.action = na.omit)

summ(model_child_bivariate, confint = TRUE, pvals = TRUE)

## ADULTS
model_adult_bivariate <- svyglm(log(monoEthyl)~year, design=subset_adult, na.action = na.omit)

summ(model_adult_bivariate, confint = TRUE, pvals = TRUE)










