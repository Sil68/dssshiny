##########
## $Id$
## ui.R
## ====
##     Shiny user interface controls.
##
##     Underlying scheme:
##         Build a linear regression model with parameters set via the
##         user interface.
##
##             y = b0 + b1 * x1 + b2 * x2 + e
##
##     Application input parameter:
##         :: b0 ... set by slider;
##         :: b1 ... set by slider;
##         :: x1 ... N(mu, sd) (mu as numeric input, sd set by slider);
##         :: b2 ... set by slider;
##         :: x2 ... function of x1, set via drop down box;
##         :: e .... N(0, sd) (sd set by slider);
##         :: n .... number of observations (numeric inputs).
##
## Copyright (C) Martin HEIN (m#)/March 2016
##
##     $Log$
##

##########
library(data.table)
library(shiny)
source("global.R")

##########
shinyUI(
    # setup a navigation bar layout
    navbarPage("Linear Regression Model", 
        # introduction
        tabPanel("introduction",
            fluidPage(
                includeMarkdown("introduction.Rmd")
            ) # fluidPage
        ), # tabPane
       
        # overview/summary of the data & model
        tabPanel("overview",
                 fluidPage(
                     fluidRow(
                         h4("Predictor/response relationship"),
                         wellPanel(
                             h2("y = b0 + b1 * x1 + b2 * x2 + e")
                         )
                     ), # fluidRow
                     fluidRow(
                         h4("Simulation parameter provided"),
                         dataTableOutput("inpara")
                     ), # fluidRow
                     fluidRow(
                         wellPanel(
                             h4("Model coefficients"),
                             verbatimTextOutput("modcoef")
                         )
                     ) # fluidRow
                 ) # fluidPage
        ), # tabPane
        
        # input parameter
        tabPanel("parameter",
            fluidPage(
                fluidRow(
                    column(9, h3("Model parameter")),
                    column(2, actionButton("start","Start"))
                ), 
                fluidRow(
                    wellPanel(
                        sliderInput("b0", "beta_0:", min=-100, max=100, 
                                    value=defB0, step=0.1),
                        sliderInput("b1", "beta_1:", min=-100, max=100, 
                                    value=defB1, step=0.1),
                        numericInput("mu_x1", "mu_x1:", 
                                     value=defMu_x1, step=0.1),
                        sliderInput("sd_x1", "sd_x1:", min=0, max=5, 
                                    value=defSd_x1, step=0.1),
                        sliderInput("b2", "beta_2:", min=-100, max=100, 
                                    value=defB2, step=0.1),
                        selectInput("x2", "x2", 
                                    choices=valX2, selected=defX2),
                        sliderInput("sd_e", "sd_e:", min=0, max=1, 
                                    value=defSd_e, step=0.1)
                    ),
                    wellPanel(
                        numericInput("nobs", "observations:", value=defNobs)
                    ),
                    wellPanel(
                        dateInput("seed", "seed:", 
                                  value=defSeed, format="yyyy-mm-dd")
                    )
                ) # fluidRow
            ) # fluidPage
        ), # tabPane
        
        # simulated data
        tabPanel("data",
            fluidPage(
                fluidRow(
                    h3("Generated/simulated observations")
                ), 
                fluidRow(
                    dataTableOutput("simdat")
                )
            ) # fluidPage
        ), # tabPane
        
        # model summary
        tabPanel("summary",
            fluidPage(
                fluidRow(
                    h3("Data & model summary")
                ), 
                fluidRow(
                    h4("Data summary"),
                    verbatimTextOutput("moddt")
                ),
                fluidRow(
                    h4("Model summary"),
                    verbatimTextOutput("modsum")
                )
            ) # fluidPage
        ), # tabPane
        
        # plotted data
        tabPanel("plot",
                 fluidPage(
                     fluidRow(
                         h3("Data & model plots")
                     ), 
                     fluidRow(
                         h4("Relationship between the predictor variable(s) and the response variable"),
                         plotOutput("datplot")
                     ),
                     fluidRow(
                         h4("Diagnostic plots"),
                         plotOutput("modplot")
                     )
                 ) # fluidPage
        ), # tabPane
        
        # plotted data
        tabPanel("help",
                 includeMarkdown("help.Rmd")
        ), # tabPane
        
        # plotted data
        tabPanel("rationale",
                 includeMarkdown("Shiny Application and Reproducible Pitch.Rmd")
        ) # tabPane
    ) # navbarPage
) # shinyUI

##
## end of file
##