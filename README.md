# sourceOrientedApproach
R package for the analysis performed in A Source-Oriented Approach to Coal Combustion Emissions

# Installing sourceOrientedApproach
In RStudio, install the sourceOrientedApproach R package from github.  
### Install and load ```devtools```
```
install.packages(c('devtools','data.table'))
library(devtools)
library(data.table)
```
### Install and load ```sourceOrientedApproach```
```
devtools::install_github("kfcaby/sourceOrientedApproach")
library(sourceOrientedApproach)
```
# sourceOrientedApproach example

Estimating the effect of high coal emissions exposure on IHD hospitalizations in the MEDICARE population.

## ihd2005 
Outcome: ihd2005 contains a data table of simulated MEDICARE health outcomes in 2005.  The IHD column contains the simulated number of ischemic heart disease hospitalizations in the MEDICARE population in 2005 at each U.S. ZIP code.  The person_years column is the number of observed person years contributing to the hospitalizations.  You can load the data set using ```data('ihd2005')```. (need to get these from Cory)

## inmap2005
Exposure: inmap2005 contains a data table of the estimated PM2.5 at each ZIP code attributible to the emissions from 783 coal-fired power generating units operating in the U.S. in 2005.  We obtained these estimates using the Intervention Model for Air Pollution (add citation - also a reference to rinmap). You can load the data set using ```data('inmap')```.

## covariates
Covariates: covariates contains a data table of potential confounders of the IHD/exposure relationship. They include various demographic and socioeconomic characteristics of each ZIP code from census data, smoking rates, and climitalogical variables. You can load the data set using ```data('covariates')```.

## Analyzing a data set

There are three main ways to analyze a data set for which we have set of observed confounders, location information (such as longitude, latitude), a binary treatment, and propensity score estimates of the treatment on the observed covariates.

These ways include

- DAPSm with fixed weight
- DAPSm with fast search of the optimal weight
- DAPSm with extensive search of the optimal weight (recommended)

For each one of the three ways to choose the paramter $w$, we have a greedy algorithm,
and an optimal algorithm available, which can be specified using the argument ```matching_algorithm``` equal to 'greedy' or 'optimal' accordingly. When the matching algorithm is set equal to optimal, the additional argument ```remove.unmatchables``` can be set equal to TRUE to allow the drop of treated units that do not have an appropriate match.

### DAPSm with fixed weight, and greedy algorithm

If the investigator has a prior belief of the relative importance of propensity score similarity and distance in the matching, DAPSm could be fit with fixed w expressing these beliefs.

For example, in the toyData2 data set, one could fit DAPSm with w = 0.7 by performing

```
data('toyData2')
toyData2$prop.scores <- glm(Z ~ X1 + X2 + X3 + X4, family = binomial,
                            data = toyData2)$fitted.values

daps <- DAPSest(toyData2, out.col = 2, trt.col = 1, caliper = 0.3,
                weight = 0.7, coords.columns = c(4, 5),
                pairsRet = TRUE, cov.cols = 6:9, cutoff = 0.1,
                coord_dist = TRUE, caliper_type = 'DAPS',
                matching_algorithm = 'greedy')
```

### DAPSm with fixed weight, and optimal algorithm

Optimal algorithm can often fail to return matches.

```
daps <- DAPSest(toyData2, out.col = 2, trt.col = 1, caliper = 0.5,
                weight = 0.7, coords.columns = c(4, 5),
                pairsRet = TRUE, cov.cols = 6:9, cutoff = 0.15,
                coord_dist = TRUE, caliper_type = 'DAPS',
                matching_algorithm = 'optimal',
                remove.unmatchables = TRUE)
```


### DAPSm with fast search of the optimal weight (greedy algorithm)

The optimal weight is defined as the minimum w for which the absolute standardized difference of means (ASDM) of all covariates is less than a cutoff.

This includes a fast search of the optimal weight. The algorithm assumes a decreasing trend in ASDM for increasing w. It starts at 0.5 and checks balance of the covariates. If balance is acheived w is decreased to 0.25. If balance is not acheived, w is increased to 0.75. The algorithm continues this way, and w is moved by 1/2<sub>n + 1</sub> at each step n.

The fast algorithm should only be used for large data sets for which an extensive search of the optimal w is not feasible.

Fit this algorithm by performing

```
daps <- DAPSest(toyData2, out.col = 2, trt.col = 1, caliper = 0.3,
                weight = 'optimal', coords.columns = c(4, 5),
                pairsRet = TRUE, cov.cols = 6:9, cutoff = 0.15,
                w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                matching_algorithm = 'greedy')
```

### DAPSm with extensive search for the optimal weight - *Recommended*

Instead, we can fit the algorithm for varying values of w and assess balance of covariates at every step. This can be performed by

```
bal <- CalcDAPSWeightBalance(toyData2, weights = seq(0, 1, length.out = 40),
                             cov.cols = 6:9, trt.col = 1,
                             coords.columns = c(4, 5), caliper = 0.3,
                             matching_algorithm = 'greedy')
```

Balance of the covariates can be assessed by checking the ASDM as a function of w using

```
PlotWeightBalance(bal$balance, weights = seq(0, 1, length.out = 40), cutoff = 0.15)
```


<br>

![Alt text](images/DAPSm_plot1.png)

The function that can be used to choose the optimal weight and fit the model and return estimates is
```
DAPS <- DAPSchoiceModel(toyData2, trt.col = 1, balance = bal$balance,
                        cutoff = 0.15, pairs = bal$pairs,
                        weights = seq(0, 1, length.out = 40))
```
The weight chosen is equal to approximately 0.231.


## Plot of matched pairs.
We can plot the matched pairs for different weights. The function ```CalcDAPSWeightBalance()``` already returned information of the matched pairs for 40 different values of w. We will plot the set of matched pairs for the optimal w chosen above, as well as a larger w.

```
MatchedDataMap(x = bal$full_pairs[[10]], trt_coords = c(3, 4),
               con_coords = c(7, 8))
```

![Alt text](images/plot2.png)
![Alt text](images/plot3.png)

Comparing the plots of matched pairs for the two choices of w we see that for larger w the matched pairs are further away from each other. This is expected since matching weigth given to distance is decreased for increasing w.



### DAPSm with extensive search for the optimal weight with optimal matching

```
bal <- CalcDAPSWeightBalance(toyData2, weights = seq(0, 1, length.out = 40),
                             cov.cols = 6:9, trt.col = 1,
                             coords.columns = c(4, 5), caliper = 1,
                             matching_algorithm = 'optimal',
                             remove.unmatchables = TRUE)
                             
DAPS <- DAPSchoiceModel(toyData2, trt.col = 1, balance = bal$balance,
                        cutoff = 0.2, pairs = bal$pairs,
                        weights = seq(0, 1, length.out = 40))

```
