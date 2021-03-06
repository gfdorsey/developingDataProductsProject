Shiny Implementation of a Diabetes Risk Score
========================================================
author: George F. Dorsey, Jr.
date: 26 July 2015

Background
========================================================

- The incidence of type 2 diabetes (formerly adult-onset diabetes, or non-insulin dependent diabetes) is increasing worldwide.
- Serious health issues can result from unmanaged diabetes, including death, cardiovascular disease, or amputation due to neuropathy.
- Lifestyle changes in at-risk individuals have been shown to be effective in preventing the disease.
- Therefore a method of predicting diabetes risk based on certain risk factors could be useful for improving health and reducing health care costs.
- A group of researchers in Finland publishd a paper in 2003 outlining development of one possible diabetes risk prediction model.  
[J. Lindström, J. Tuomilehto.  "The Diabetes Risk Score: A Practical Tool to Predict Type 2 Diabetes Risk"  *Diabetes Care*, 26:725-731, 2003.](http://www.ncbi.nlm.nih.gov/pubmed/12610029)  



The Model
========================================================

The authors present two logistic regression models for predicting the risk of developing type 2 diabetes based on a number of risk factors.  For the sake of this exercise, some factors were broadened as shown:
- Age: Dummy variables are set to 0 for age < 45, and over 64 is binned into the 55-64 group
- Body mass index (BMI)
- Waist size, adjusted for gender
- Use of blood pressure medication
- History of high blood glucose
- *Level of physical activity*
- *Diet, specifically consumption of fruits and vegetables*  


Obligatory Code
========================================================

Some values are calculated behind the scenes based on user input.  One of these is body mass index (BMI), which takes a height in cm and weight in kg and calculates the BMI value.


```r
calcBMI <- function(weight, height) {
    ## weight in kg and height in cm
    height <- height / 100 # convert to m
    round(weight / height / height, 0)
}
calcBMI(80, 180)
```

```
[1] 25
```

Results and Conclusions
========================================================

- A Shiny app was created to implement the full logistic regression model presented in the paper by Lindström and Tuomilehto
- To calculate intermediate values, reactive expressions were used
- The app is a demonstration of what is possible with Shiny, but only scratches the surface--some possible improvements are:
    - A nicer layout based on CSS styles
    - Reacting specifically to change in units to preserve the value--for example if the height input is 72 and the units are inches, changing to cm should change the numeric input to 183 to preserve the existing value
