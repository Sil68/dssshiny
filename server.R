##########
## $Id$
## server.R
## ========
##     Shiny server.
##
## Copyright (C) Martin HEIN (m#)/March 2016
##
##     $Log$
##

##########
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(shiny)
source("global.R")

##########
shinyServer(
    function(input, output, session) {
        observeEvent(input$start, {
            isolate({
                ## return input parameters as table
                b0 <- input$b0
                b1 <- input$b1
                mu_x1 <- input$mu_x1
                sd_x1 <- input$sd_x1
                b2 <- input$b2
                x2 <- input$x2
                sd_e <- input$sd_e
                nobs <- input$nobs
                seed <- input$seed
                dtPara <- data.table(
                    parameter=c("beta_0", "beta_1", "x1", "mu_x1", "sd_x1", 
                                "beta_2", "x2", "sd_e", "observations", 
                                "seed"),
                    value=c(b0, b1, "x1 ~ N(mu_x1, sd_x1)", mu_x1, sd_x1, b2, x2, sd_e, nobs, seed)
                )
                
                ## generate simulation data
                # generate random data for x1
                x1 <- rnorm(input$nobs, mean=input$mu_x1, sd=input$sd_x1)
                
                # generate random data for x2
                if (input$x2 == valX2[1]) {
                    x2 <- log(abs(x1)+1)
                } else if (input$x2 == valX2[2]) {
                    x2 <- x1^2
                } else if (input$x2 == valX2[3]) {
                    x2 <- 1/x1
                } else {
                    x2 <- rnorm(input$nobs, mean=input$mu_x1, sd=input$sd_x1)
                }
                
                # generate random data for standard error/random noise
                e <- rnorm(input$nobs, mean=0, sd=input$sd_e)
                
                # calculate response variable
                y <- input$b0 + input$b1 * x1 + input$b2 * x2 + e
                
                # pour all of these into a table structure
                dt <- data.table(b0=input$b0, b1=input$b1, x1=x1,
                                 b2=input$b2, x2=x2, e=e, y=y)
                
                ## build a linear regression model
                fit <- lm(y ~ x1 + x2, data=dt)
                ey <- resid(lm(y ~ x2, data=dt))
                ex <- resid(lm(x1 ~ x2, data=dt))
                
                ## create data relationship plot(s)
                g1 <- ggplot(dt) +
                      aes(x=x1, y=y) +
                      geom_point(colour=abs(x2*10), alpha=0.3, size=3) +
                      geom_point(colour=abs(x2*10), alpha=0.7, size=1) +
                      scale_x_continuous(name="predictor x1") +
                      scale_y_continuous(name="response y") +
                      ggtitle("Unadjusted (predictor x2 in colour)")
                g2 <- ggplot(dt) +
                      aes(x=ex, y=ey) +
                      geom_point(colour="darkgreen", alpha=0.3, size=3) +
                      geom_point(colour="orange", alpha=0.7, size=1) +
                      scale_x_continuous(name="adjusted predictor x1") +
                      scale_y_continuous(name="adjusted response y") +
                      ggtitle("Adjusted (for predictor x2)")

                ## create model diagnostic plot(s)
            }) # isolate
            
            # return parameter, generated data and data & model summary
            output$inpara <- renderDataTable(dtPara)
            output$simdat <- renderDataTable(dt)
            output$moddt <- renderPrint({ summary(dt) })
            output$modsum <- renderPrint({ summary(fit) })
            output$modcoef <- renderPrint({ summary(fit)$coefficients })
            output$datplot <- renderPlot({ grid.arrange(g1, g2, nrow=1, ncol=2) })
            output$modplot <- renderPlot({ par(mfrow = c(2, 2)) ; plot(fit) })
        }) # observeEvent
    } # function
) # shinyServer

##
## end of file
##