library(shiny)

## raw input
#  1: [age] age (years)
#  2: [sex] gender (M/F)
#  3: [wt] weight
#  4: [wtUnits] weight units (kg or lb)
#  5: [ht] height
#  6: [htUnits] height units (cm or in)
#  7: [waist] waist
#  8: [waistUnits] waist units (cm or in)
#  9: [bpMeds] blood pressure medication (Y/N)
# 10: [hiGlucose] blood glucose (normal/high) - convert to numeric value?
# 11: [activity] physical activity (>= or < 4 h/week)
# 12: [diet] daily consumption of fruits, vegetables, or berries


shinyUI(
    fluidPage(
        titlePanel(
            title = 'Diabetes Risk Score'
        ),
        fluidRow(
            column(10, ooffset = 1,
                   h4('Background'),
                   p('This application implements a diabetes risk prediction based on a paper by',
                     a(href='http://www.ncbi.nlm.nih.gov/pubmed/12610029', 'Lindström and Tuomilehto'),
                     '.  The paper identifies several risk factors used to implement a logistic regression model for predicting risk of developing diabetes.  The logit calculated from the predictors can be used to estimate the probability.  A score in the range of 0-20 is also presented in the paper with a cutoff of 9 representing more than 75% of the new cases requiring medical treatment over a 10-year follow-up period.'),
                   h4('Directions'),
                   p('Use the inputs on the left to enter the required risk factors.  The right column will dynamically update with the estimated probability of developing diabetes along with a human-readable outcome based on the score.  Also given for those interested is a list of the factors and the raw predictor vector used in the logistic regression model.')
            )
        ),
        fluidRow(
            sidebarPanel(
                h3('Input patient risk factors:'),
                numericInput('age', 'Age in years:', 45, min = 0, step = 1),
                radioButtons('sex', 'Gender:', c('M','F'), inline = TRUE),
                numericInput('wt', 'Weight:', 185, min = 1, step = 1),
                selectInput('wtUnits', 'Weight Units:', c('lb', 'kg'), selected = 'lb'),
                numericInput('ht', 'Height:', 69, min = 0, step = 1),
                selectInput('htUnits', 'Height Units:', c('in', 'cm'), selected = 'in'),
                numericInput('waist', 'Waist:', 32, min = 0, step = 1),
                selectInput('waistUnits', 'Waist Units:', c('in', 'cm'), selected = 'in'),
                checkboxInput('bpMeds', 'Uses blood pressure medication', value = FALSE),
                checkboxInput('hiGlucose', 'High glucose (Fasting blood plasma level ≥ 7.0 mmol/L or ≥ 126 mg/dL, or for older whole blood measure, ≥ 6.1 mmol/L or ≥ 110 mg/dL)', value = FALSE),
                checkboxInput('activity', 'Physical activity < 4 h/week', value = FALSE),
                checkboxInput('diet', 'Daily consumption of fruits, vegetables, or berries', value = TRUE)
            ),
            mainPanel(
                h3('Diabetes risk based on given inputs:'),
                h4('Probability of developing diabetes based on the article\'s logistic regression model:'),
                verbatimTextOutput('probability'),
                h4('Result:'),
                uiOutput('result'),
                hr(),
                h4('List of inputs and calculated values:'),
                verbatimTextOutput('age'),
                verbatimTextOutput('gender'),
                verbatimTextOutput('weight'),
                verbatimTextOutput('height'),
                verbatimTextOutput('bmi'),
                verbatimTextOutput('waist'),
                verbatimTextOutput('bpMeds'),
                verbatimTextOutput('hiGlucose'),
                verbatimTextOutput('physicalActivity'),
                verbatimTextOutput('diet'),
                hr(),
                h4('Other information'),
                p('Raw predictor vector'),
                verbatimTextOutput('predictors'),
                p('Score'),
                verbatimTextOutput('score')
            )
        )
    )
)
