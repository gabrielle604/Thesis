## Chi Squared Test
# very small p value ->> reject the idea that they are equal
svychisq(~ethnicity+gender, nhc, statistic="adjWald")


## Independent-samples t-test:
svyttest(log(monoEthyl)~gender, nhc)

## ANOVA
# This is an example of a Kruskal Wallis test, which is the non-parametric analog of a one-way ANOVA.

# Null Hypothesis (H0): All ranks are equal (no difference in medians) 
# Alternative Hypothesis (HA): At least two ranks differ (at least one difference in group medians)

# Tests whether samples are originated from the same distribution.
# The ANOVA (and t-test) is explicitly a test of equality of means of values. The Kruskal-Wallis (and Mann-Whitney) can be seen technically as a comparison of the mean ranks

# NOTE: the "Chisq" is actually the H Statistic
kwtest <- svyranktest(monoEthyl~adultED, design = nhc, na = TRUE, test=("KruskalWallis"))
kwtest

## Collinearity of reference person education and adult participant education

model <- lm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED+refED+year, design=subset_adult, data = only_adults)
ols_coll_diag(model)
# how do I interpret that ^^^ ???

## another approach
### standard errors tend to inflate (get bigger) when there are collinear variables
summary(lm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED+year, design=subset_adult, data = only_adults))

summary(lm(log(monoEthyl)~age+gender+ethnicity+fpl+citizenship+adultED+refED+year, design=subset_adult, data = only_adults))
# coefficients for adultED go down when refED added into the model

## Only looking at correlation between adult participant education and household head education

# If you want to have a genuine correlation plot for factors or mixed-type, you can also use model.matrix to one-hot encode all non-numeric variables. This is quite different than calculating Cramér's V as it will consider your factor as separate variables, as many regression models do.

# You can then use your favorite correlation-plot library. I personally like ggcorrplot for its ggplot2 compatibility.

# Source: https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
only_adults_edu <- only_adults %>% select(adultED,refED)

model.matrix(~0+., data=only_adults_edu) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


only_adults_edu <- NHANES %>% select(DMDHREDU,DMDEDUC2)

model.matrix(~0+., data=only_adults_edu) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

only_children_edu <- NHANES %>% select(DMDHREDU,INDFMPIR)

model.matrix(~0+., data=only_children_edu) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
