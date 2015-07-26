library(shiny)

## conversion factors
poundsPerKilogram <- 2.20462
cmPerIn <- 2.54
mgdLPermM <- 18

## full model betas
#  1: Intercept
#  2: Age 45-54
#  3: Age 55-64 -- note no category for higher age, so choose to treat as >= 55
#  4: BMI in kg/m^2 25-30
#  5: BMI in kg/m^2 >30
#  6: Waist circumference in cm men(94 to <102), women(80 to <88)
#  7: Waist circumference in cm men(>= 102), women(>=88)
#  8: Have you ever used medication for high blood pressure?
#  9: History of high blood glucose
# 10: Physical activity < 4 h/week
# 11: Daily consumption of vegetables, fruits, or berries
betas <-  c(-5.658, 0.650, 0.940, 0.015, 0.938, 1.021, 1.424, 0.714, 2.263, 0.268, 0.165)

## score 'coefficients'
scores <- c(0, 2, 3, 1, 3, 3, 4, 2, 5, 2, 1)

## human readable outcome
lowScore  <- c(-Inf, 4, 9, 13)
hiScore   <- c(3, 8, 12, Inf)
risk      <- c('low', 'low', 'moderate', 'high')
incidence <- c('0.3%', '2.4%', '10.5%', '32.7%')
message   <- c('The patient should maintain his or her healthy lifestyle!',
               'The patient has some room for improvement, but low risk.',
               'The patient\'s score is above the cutoff.  Action should be taken to reduce the risk factors, and the patient should consider discussing these results with his or her physician.',
               'The patient\'s score is well above the cutoff.  Consultation with a physician would be the best course of action.')
outcome   <- data.frame(lowScore, hiScore, risk, incidence, message, stringsAsFactors = FALSE)

result <- function(s) {
    for (i in 1:4) {
        if (s >= outcome$lowScore[i] & s <= outcome$hiScore[i]) {
            message <- NULL
            message[1] <- paste0('The calculated score is <strong>', s, '</strong> on a scale of 0 (best) to 20 (worst).')
            message[2] <- paste('This represents a', outcome$risk[i], 'risk of developing diabetes.')
            message[3] <- paste('In a 10-year follow-up,', outcome$incidence[i], 'of people with that score developed diabetes.')
            message[4] <- outcome$message[i]
            return(paste0('<p>', message, '</p>', collapse = '\n'))
        }
    }
    return('<p>Error: Invalid score!<p>')
}


## helper functions
bmi <- 0
calcBMI <- function(weight, height) {
    ## weight in kg and height in cm
    height <- height / 100 # convert to m
    round(weight / height / height, 0)
}

weight <- 0
calcWeight <- function(weight, units) {
    ## returns weight in kg
    if (units == 'lb') {
        weight <- weight / poundsPerKilogram
    }
    round(weight, 0)
}

height <- 1
waist <- 0
calcLength <- function(len, units) {
    ## returns length (e.g. height or waist) in cm
    if (units == 'in') {
        len <- len * cmPerIn
    }
    round(len, 0)
}

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

getPredictors <- function(input) {
    xs <- rep(NA, length(betas))
    xs[1] <- 1 # intercept term
    if (input$age >= 45 & input$age < 55) {
        xs[2] <- 1
        xs[3] <- 0
    } else if (input$age >= 55) {
        xs[2] <- 0
        xs[3] <- 1
    } else {
        xs[2] <- 0
        xs[3] <- 0
    }
    wt <- input$wt
    if (input$wtUnits == 'lb') {
        wt <- wt / poundsPerKilogram
    }
    ht <- input$ht
    if (input$htUnits == 'in') {
        ht <- ht * cmPerIn
    }
    bmi <- calcBMI(wt, ht)
    if (bmi > 30) {
        xs[4] <- 0
        xs[5] <- 1
    } else if (bmi > 25) {
        xs[4] <- 1
        xs[5] <- 0
    } else {
        xs[4] <- 0
        xs[5] <- 1
    }
    waist <- input$waist
    if (input$waistUnits == 'in') {
        waist <- waist * cmPerIn
    }
    if (input$sex == 'M' & waist >= 102) {
        xs[6] <- 0
        xs[7] <- 1
    } else if (input$sex == 'M' & waist >= 94) {
        xs[6] <- 1
        xs[7] <- 0
    } else if (input$sex == 'F' & waist >= 88) {
        xs[6] <- 0
        xs[7] <- 1
    } else if (input$sex == 'F' & waist >= 80) {
        xs[6] <- 1
        xs[7] <- 0
    } else {
        xs[6] <- 0
        xs[7] <- 0
    }
    xs[8] <- as.numeric(input$bpMeds)
    xs[9] <- as.numeric(input$hiGlucose)
    xs[10] <- as.numeric(input$activity)
    xs[11] <- as.numeric(!input$diet)
    xs
}

diabetesProbability <- function(predictors) {
    logit <- sum(betas * predictors)
    if (is.na(logit)) {
        return(NA)
    }
    p <- exp(logit) / (1 + exp(logit))
    paste0(round(p*100,2), '%')
}

diabetesScore <- function(predictors) {
    sum(scores * predictors)
}


## main app function for server side

shinyServer(
    function(input, output, clientData, session) {
        output$age <- reactive({paste('Age:', input$age, 'years')})
        output$gender <- reactive({paste('Gender:', ifelse(input$sex == 'M', 'Male', 'Female'))})
        weight <- reactive({calcWeight(input$wt, input$wtUnits)})
        output$weight <- reactive({paste('Weight:', weight(), 'kg')})
        height <- reactive({calcLength(input$ht, input$htUnits)})
        output$height <- reactive({paste('Height:', height(), 'cm')})
        bmi <- reactive({calcBMI(weight(), height())})
        output$bmi <- reactive({paste('BMI:', bmi())})
        waist <- reactive({calcLength(input$waist, input$waistUnits)})
        output$waist <- reactive({paste('Waist:', waist(), 'cm')})
        output$bpMeds <- reactive({paste('Uses blood pressure medication:', ifelse(input$bpMeds, 'Yes', 'No'))})
        output$hiGlucose <- reactive({paste('Elevated glucose levels:', ifelse(input$hiGlucose, 'Yes', 'No'))})
        output$physicalActivity <- reactive({paste('Physical activity:', ifelse(input$activity, 'Less than four hours per week', 'At least 4 hours per week'))})
        output$diet <- reactive({paste('Diet:', ifelse(input$diet, 'Daily consumption of fruits, vegetables, or berries', 'Lack of fruits, vegetables, or berries'))})
        predictors <- reactive({getPredictors(input)})
        probability <- reactive({diabetesProbability(predictors())})
        score <- reactive({diabetesScore(predictors())})
        output$predictors <- renderPrint(predictors())
        output$probability <- renderText(probability())
        output$score <- renderText(score())
        #        output$result <- renderText({result(score())})
        output$result <- renderUI(HTML(result(score())))
    }
)
