
# server.R

library(shiny)
library(ggplot2)
library(scales)

lambda = 0.94

shinyServer(function(input, output) {
        datInput <- reactive({
                
                # final date month -1, day and year
                d = as.integer(format(input$dateRng[2],"%m")) - 1 
                e = as.integer(format(input$dateRng[2],"%d")) 
                f = as.integer(format(input$dateRng[2],"%Y"))
                
                # initial date month -1, day and year
                a = as.integer(format(input$dateRng[1],"%m")) - 1
                b = as.integer(format(input$dateRng[1],"%d"))
                c = as.integer(format(input$dateRng[1],"%Y"))
                
                # loading data from finance.yahoo.com
                data1 = read.csv(paste(
                        "http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&d=",
                        d, "&e=", e, "&f=", f, "&g=d&a=", a, "&b=", b, "&c=", c,
                        "&ignore=.csv", sep = ""))
                
                data1$Date = as.Date(data1$Date)
                
                # Calculating logarithmic returns 
                
                data1$logret = log(data1$Close/data1$Close[2:nrow(data1)])
                data1 = data1[1:(nrow(data1)-1),]
                
                ndat = nrow(data1)
                
                # Calculating price volatility using Exponentially Weighted
                # Moving Average (exponential smoothing)
                
                data1$ewma = as.numeric(rep(NA, nrow(data1)))
                data1$ewma[ndat] = data1$logret
                for(i in (ndat-1):1){
                        data1$ewma[i] = sqrt(
                                lambda*(data1$ewma[i+1]^2)+
                                        (1-lambda)*(data1$logret[i]^2))
                }
                
                # Calculating price volatility using Moving Average of 1 month data
                # (assuming one month of data contains 20 business days)
                
                data1$ma21 = as.numeric(rep(NA, nrow(data1)))
                if(ndat>=21) {
                        for(i in 1:(ndat-20)){
                                data1$ma21[i] = sqrt(sum((data1$logret[i:(i+20)])^2)/21) 
                        }
                }
                
                # Calculating price volatility using Moving Average of 2 month data
                # (assuming two months of data contains 40 business days)
                
                data1$ma63 = as.numeric(rep(NA, nrow(data1)))
                if(ndat>=63) {
                        for(i in 1:(ndat-62)){
                                data1$ma63[i] = sqrt(sum((data1$logret[i:(i+62)])^2)/63) 
                        }
                        
                }
                
                # Calculating price volatility using Moving Average of 3 month data
                # (assuming three months of data contains 60 business days)
                
                data1$ma126 = as.numeric(rep(NA, nrow(data1)))
                if(ndat>=126){
                        for(i in 1:(ndat-125)){
                                data1$ma126[i] = sqrt(sum((data1$logret[i:(i+125)])^2)/126) 
                        }
                        
                }
                data1
        })         
        
        output$pricePlot <- renderPlot({
                x = datInput()
                ggplot(x, aes(Date, Close)) + geom_line(colour = "blue") + 
                        scale_x_date(labels = date_format("%b-%Y")) +
                        labs(
                                x = "",
                                y = "Close Price",
                                title = "Daily Closing Prices") +
                        theme_bw()
        })
        
        
        output$distPlot <- renderPlot({
                x = datInput()
                ggplot(data = x, aes(logret)) + 
                        geom_histogram(aes(y = ..density..),
                                       fill = "lightblue",
                                       col = "white") +
                        geom_density(col="blue") +
                        labs(title = "Histogram of Logarithmic Returns") + 
                        theme_bw()
        })
        
        
        output$volPlot <- renderPlot({
                
                x = datInput()
                g1 = ggplot(x, aes(x =Date)) + 
                        geom_line(aes(y = logret, color = "Log Returns")) + 
                        scale_x_date(labels = date_format("%b-%Y")) +
                        labs(
                                x = "",
                                y = "",
                                title = "Price Volatility")
                
                if("1" %in% input$volcalc) {
                        g1 = g1 + geom_line(aes(y = ewma, color = "EWMA"), size = 1)
                }
                
                if("2" %in% input$volcalc) {
                        g1 = g1 + geom_line(aes(y = ma21, color = "MA 1M"), size = 1)
                }
                
                if("3" %in% input$volcalc) {
                        g1 = g1 + geom_line(aes(y = ma63, color = "MA 3M"), size = 1)
                }
                
                if("4" %in% input$volcalc) {
                        g1 = g1 + geom_line(aes(y = ma126, color = "MA 6M"), size = 1)
                }
                
                g1 + scale_color_manual(
                        name = "",
                        values = c(
                                "Log Returns" = "darkgray",
                                "EWMA" = "red",
                                "MA 1M" = "blue",
                                "MA 3M" = "green",
                                "MA 6M" = "orange")) +
                        theme_bw()
        })
})
