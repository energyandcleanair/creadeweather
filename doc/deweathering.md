# "Deweathering" air quality measurements

## Introduction

Air pollution is influenced by both natural factors, such as weather, and human activities, particularly emissions. Understanding the distinct contributions of these factors is crucial for developing effective air quality management strategies and measuring progress. This document outlines CREA's method to disentangle the effects of weather from emissions on air pollution levels using a machine learning model.

## What is deweathering?
Deweathering refers to the process of correcting for the influence of weather on air pollution data to better understand the underlying trends in air quality. This method separates the effects of natural weather patterns from other factors affecting air pollution levels, such as changes in emissions. By doing so, deweathering provides clearer insights into the structural and long-term changes in air quality, independent of short-term weather fluctuations.

## Methodology: Deweathering Air Pollution Data

Deweathering is achieved by training a machine learning morel (in our case a Gradient Boosting Machine) to predict air pollution levels based on weather-related variables and a trend term.

### 1. Data Inputs

**Weather Variables:**  
The model incorporates a comprehensive set of weather variables sourced from the ERA5 reanalysis dataset, including:

- Air temperature - minimum and maximum
- Atmospheric pressure
- Wind direction
- Wind speed
- Precipitation
- Dewpoint temperature
- Planetary boundary layer - minimum and maximum

**Data Aggregation:**  
Both air pollution measurements and weather variables are aggregated at daily frequencies.

**Lagged Variables:**  
To capture delayed effects of weather, the model includes lagged versions of these variables for 1, 2, and 3 days prior to the day of interest.

### 2. Machine Learning Model: Gradient Boosting Machine (GBM)

The GBM is trained using historical data on air pollution levels and weather variables. It "predicts" pollution levels based solely on weather conditions (unless a trend variable is used). The model includes several regularization techniques, such as shrinkage (learning rate), subsampling, and cross-validation (cv_folds=3) to prevent overfitting and ensure generalization.


## Applications

### 1. Trend Analysis

Trend analysis of deweathered data allows for the identification of long-term changes in air pollution that are driven by emissions rather than weather variability. It is accomplished by introducing a general trend term (i.e. date) as a predictor and extracting the partial depence of the trained model on this variable. The figure below shows the example of such trend for Delhi, India.

![trendts](figures/deweathering_ts_trend.png)


### 2. Monthly Year-over-Year (Y-o-Y) Analysis

By comparing the value of this trend term across different periods, it is possible to estimate the part of observed changes in concentration that is due to weather and the part that is attributable to other factors (assumed to be driven by change in emissions).

The respective influence of weather and emissions in the year-on-year changes of air quality are shown in the chart below.

![yoyts](figures/deweathering_bar_yoy.png)

From this chart, one can conclude that:
- Delhi's observed increase of PM2.5 levels (+16%) were due to weather conditions. After correcting for these, we would expect an actual decrease of 10%.
- Mumbai's observed decrease of PM2.5 levels (-62%) was almost entirely due to change in emissions.

## Appendix
### Model parameters
We conducted a grid analysis of GBM hyper-parameters (e.g. interaction depth, days of lag, cv_folds, learning rate) on five Indian cities, and picked these values that minimise RMSE and maximise R2 (there was no ambiguity):
- interaction depth: 7
- learning rate: 0.01
- maximum number of trees: 20,000
- cv_folds: 2


## References
Grange, S. K., & Carslaw, D. C. (2019). Using meteorological normalisation to detect interventions in air quality time series. Science of the Total Environment, 653, 578–588. https://doi.org/10.1016/j.scitotenv.2018.10.344