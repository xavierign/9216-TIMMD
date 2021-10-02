# Quizz 1 Instructions

The main goal is to play around with the data and work on the pre-processing to improve a classification

Imagine that you have a dataset with robberies. This is dummy data and not real.

* **Date:** date of the robbery
* **Ammount:** ammount transported that was robbed
* **LONG:** Longitude of the robbery
* **LAT** Latitude of the robbery
* **f1** Imaginary category of the good that was transported and stolen.
* **f2** Imaginary category of the good that was transported and stolen.
* **Target** if the good was stolen (1) or not (0)

Three files are given: 
* **train_df.csv:** training data with Target
* **test_df.csv:** testing data to predict if the good will be stolen or not.
* **out.csv:** example prediction to be submmited

A demo script is provided as a baseline
* **demo_script.R**

The prediction should be submmited to 
https://xavierign.shinyapps.io/9216-Q1-CostCalculator/

the cost will be computed on the errors:

false positive: 1 

false negative: 2

Better models will be discussed during the course.


