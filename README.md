# sourceOrientedApproach
R package for the analysis performed in A Source-Oriented Approach to Coal Combustion Emissions (need to add more info here)

# Installing sourceOrientedApproach
In RStudio, install the sourceOrientedApproach R package from github.  
### Install and load devtools
```
install.packages(c('devtools','data.table'))
library(devtools)
library(data.table)
```
### Install and load sourceOrientedApproach
```
devtools::install_github("kfcaby/sourceOrientedApproach")
library(sourceOrientedApproach)
```
# sourceOrientedApproach example

Estimate the effect of high coal emissions exposure on ischemic heart disease (IHD) hospitalizations in the MEDICARE population.

## Create a data set for analysis

### outcome - medicare_sim2005 
Outcome: medicare_sim2005 contains a data table of simulated MEDICARE health outcomes in 2005.  For example, the IHD column contains the simulated number of IHD hospitalizations in the MEDICARE population in 2005 at each U.S. ZIP code.  The person_years column is the number of observed person years contributing to the hospitalizations.  You can load the data set using ```data('medicare_sim2005')```. 

### exposure - inmap2005
Exposure: inmap2005 contains a data table of the estimated PM2.5 at each ZIP code attributible to the emissions from 783 coal-fired power generating units operating in the U.S. in 2005.  We obtained these estimates using the Intervention Model for Air Pollution. You can load the data set using ```data('inmap2005')```.

### covariates
Covariates: covariates contains a data table of potential confounders of the IHD/exposure relationship. They include various demographic and socioeconomic characteristics of each ZIP code from Census 2000, county smoking rates (add citation), and climatological variables. You can load the data set using ```data('covariates')```.

NOTE: NEED TO ADD WEATHER TO THESE

## Analyzing a data set

### Perform propensity score matching using MatchIt to adjust for potential confounders
```
# Include these regions
regions <- c("IndustrialMidwest", "Northeast", "Southeast")

# Covariates to adjust for using propensity score matching
covariate.vars <- c("logPop","PctUrban","PctBlack","PctHisp","PctHighSchool", 
"MedianHHInc", "PctPoor","PctFemale","PctOccupied","PctMovedIn5", "smokerate2000",
"mean_age", "Female_rate", "White_rate", "avtmpf", "avrelh")

dataset <- getMatchedDataset(exposure = inmap2005, covariates, covariate.vars, regions, exact.vars = "region")
```
### Plot U.S. maps of the exposure
```
# Continuous Exposure
plotExposureUSmap(dataset$raw, exposure.binary = FALSE)
```
![Alt text](images/continuous.png)
```
# Binary Exposure
plotExposureUSmap(dataset$raw, exposure.binary = TRUE)
```
![Alt text](images/binary.png)
```
# Matched data
plotExposureUSmap(dataset$matched, exposure.binary = TRUE)
```
UPDATE THIS ONCE I HAVE WEATHER DATA
![Alt text](images/binary_matched.png)
### Other useful information
```
# Histogram of the propensity scores by high exposed and controls
plotPropensityScoreHistogram(dataset$raw)

# Standardized mean difference plot (before and after matching)
createSMDplot(dataset$match.model)

# Summary of matching results
dataset$match.model$nn
```
![Alt text](images/propensityScores.png)
![Alt text](images/SMD.png)
### Estimate incidence rate ratio (IRR) for IHD comparing high exposed locations to the controls
```
outcome.model <- fitOutcomeModel(dataset$matched, medicare_sim2005, covariate.vars)
summary(outcome.model)

# Coefficients and confidence intervals on natural scale
cbind(exp(coef(outcome.model)), exp(confint(outcome.model)))
```
# References
Austin, Peter C. "An introduction to propensity score methods for reducing the effects of confounding in observational studies." Multivariate behavioral research 46.3 (2011): 399-424.

Dwyer-Lindgren, Laura, et al. "Cigarette smoking prevalence in US counties: 1996-2012." Population health metrics 12.1 (2014): 5.

Ho, Daniel E., et al. "MatchIt: nonparametric preprocessing for parametric causal inference." Journal of Statistical Software 42.8 (2011): 1-28.

Tessum, Christopher W., Jason D. Hill, and Julian D. Marshall. "InMAP: A model for air pollution interventions." PloS one 12.4 (2017): e0176131.
