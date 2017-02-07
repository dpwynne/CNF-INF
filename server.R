## Shiny app to demonstrate cumulative normal proportion problems and inverse normal proportion problems
## Server side

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  ## need the custom slider for the CNF version
    output$slider_cnf_bounds <- renderUI({
      min_range <- input$cnf_mean - 4*input$cnf_sd
      max_range <- input$cnf_mean + 4*input$cnf_sd
      if (is.null(input$slider_cnf_range)){
        if (input$cnf_type == 2) bounds <- c(min_range, input$cnf_mean)
        if (input$cnf_type == 1) bounds <- c(input$cnf_mean, max_range)
        if (input$cnf_type == 3) bounds <- c(min_range, max_range)
      }else{
        if (input$cnf_type == 2) bounds <- c(min_range, input$slider_cnf_range[2])
        if (input$cnf_type == 1) bounds <- c(input$slider_cnf_range[1], max_range)
        if (input$cnf_type == 3) bounds <- input$slider_cnf_range
      }

      sliderInput("slider_cnf_range", label = "Range of Values", min = min_range, max = max_range, value = bounds, step = 0.1*input$cnf_sd)
  })

  ## Step 1: Normal Distribution Plot
  output$cnf_plot <- renderPlot({
    x <- seq(input$cnf_mean - 4.01*input$cnf_sd, input$cnf_mean + 4.01*input$cnf_sd, by = input$cnf_sd/100) 
    y <- dnorm(x, input$cnf_mean, input$cnf_sd)
    min_range <- input$cnf_mean - 4*input$cnf_sd
    max_range <- input$cnf_mean + 4*input$cnf_sd
    newdata <- data.frame(x,y)
    ## sort out the labels in the graph based on the type of problem
    if (input$cnf_type == 1){
      above <- input$slider_cnf_range[1]
      below <- max_range
      x.labels <- above
    }
    if (input$cnf_type == 2){
      above <- min_range
      below <- input$slider_cnf_range[2]
      x.labels <- below
    }
    if (input$cnf_type == 3){    
      above <- input$slider_cnf_range[1]
      below <- input$slider_cnf_range[2]
      x.labels <- c(above, below)
    }
    y.bounds <- -max(y)/40

    ## four steps to creating the graph
    n1 <- ggplot(data=newdata, aes(x=x,y=y))+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    n2 <- n1 + geom_area(data=newdata[(x>=above & x<=below),], color="black", linetype="dashed", fill="lightgray")
    n3 <- n2 + geom_area(data=newdata, color="black", fill=NA) + labs(x="Values", y="Density")
    if (length(x.labels) > 0){
      n4 <- n3 + annotate("text", x=x.labels, y = y.bounds, label=as.character(x.labels))
    }else{
      n4 <- n3
    }
    print(n4)
    
  })

  ## Step 2: z-score - low
  output$cnf_zscore_low <- renderText({
    z1 <- (input$slider_cnf_range[1] - input$cnf_mean)/input$cnf_sd

    ex1 <- paste0("The z-score corresponding to x = ", input$slider_cnf_range[1], " is ", round(z1, 2))

    if (input$cnf_type == 1 | input$cnf_type == 3) return(ex1)
    if (input$cnf_type == 2) return("")
  
  })
  
  output$cnf_zscore_high <- renderText({
    z2 <- (input$slider_cnf_range[2] - input$cnf_mean)/input$cnf_sd
    
    ex2 <- paste0("The z-score corresponding to x = ", input$slider_cnf_range[2], " is ", round(z2, 2))
    
    if (input$cnf_type == 2 | input$cnf_type == 3) return(ex2)
  })
  
  ## Step 3: cumulative probabilities - low
  output$cnf_cprob_low <- renderText({
    z1 <- (input$slider_cnf_range[1] - input$cnf_mean)/input$cnf_sd
    p1 <- pnorm(z1)
    
    ex1 <- paste("The cumulative proportion corresponding to z =", round(z1, 2), "is", round(p1, 4))
    
    if (input$cnf_type == 1 | input$cnf_type == 3) return(ex1)
    if (input$cnf_type == 2) return("")
  })
    
  ## Step 3: cumulative probabilities - high
  output$cnf_cprob_high <- renderText({
    z2 <- (input$slider_cnf_range[2] - input$cnf_mean)/input$cnf_sd
    p2 <- pnorm(z2)
    
    ex2 <- paste("The cumulative proportion corresponding to z =", round(z2, 2), "is", round(p2, 4))
    
    if (input$cnf_type == 1) return("")
    if (input$cnf_type == 2 | input$cnf_type == 3) return(ex2)
  
  })
  
  ## Step 4: answer
  output$cnf_answer <- renderText({
    z1 <- (input$slider_cnf_range[1] - input$cnf_mean)/input$cnf_sd
    z2 <- (input$slider_cnf_range[2] - input$cnf_mean)/input$cnf_sd
    p1 <- pnorm(z1)
    p2 <- pnorm(z2)
    
    ex1 <- paste("The proportion of values greater than", input$slider_cnf_range[1], "is", round(1-p1, 4))
    ex2 <- paste("The proportion of values less than", input$slider_cnf_range[2], "is", round(p2, 4))
    ex3 <- paste("The proportion of values between", input$slider_cnf_range[1], "and", input$slider_cnf_range[2], "is", round(p2-p1, 4))
    
    if (input$cnf_type == 1) return(ex1)
    if (input$cnf_type == 2) return(ex2)
    if (input$cnf_type == 3) return(ex3)
    
  })

  ## Inverse Normal Proportion problem - Step 1: Plot Standard Normal Curve
  output$inf_plot <- renderPlot({
    x <- seq(-4, 4, by = 0.01)
    y <- dnorm(x)
    newdata <- data.frame(x,y)
    above <- max(qnorm(input$inf_pcts[1]/100), -4)  # don't want -Inf and Inf
    below <- min(qnorm(input$inf_pcts[2]/100), 4)  # same idea
    
    ## get locations for text and text to display
    if (above > -4 & below < 4){
      x.bounds <- c(mean(c(-4, above)),mean(c(above, below)),mean(c(below, 4)))
      pctiles <- c(input$inf_pcts[1], input$inf_pcts[2]-input$inf_pcts[1], 100-input$inf_pcts[2])
    }
    if (above > -4 & below == 4){
        x.bounds <- c(mean(c(-4, above)),mean(c(above, 4)))
        pctiles <- c(input$inf_pcts[1], 100-input$inf_pcts[1])
    }
    if (above == -4 & below < 4){
        x.bounds <- c(mean(c(-4, below)),mean(c(below, 4)))
        pctiles <- c(input$inf_pcts[2], 100-input$inf_pcts[2])
    }
    if (above == -4 & below == 4){
      x.bounds <- c(-4, 0)
      pctiles <- c(0, 100)
    }
    if (above >= 4){
      x.bounds <- c(0, 4)
      pctiles <- c(100, 0)
    }
    if (below <= -4){
      x.bounds <- c(-4, 0)
      pctiles <- c(0, 100)
    }

    y.bounds <- -0.01

    ## four steps to the graph    
    n1 <- ggplot(data=newdata, aes(x=x,y=y))+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    n2 <- n1 + geom_area(data=newdata[(x>=above & x<=below),], color="black", linetype="dashed", fill="lightgray")
    n3 <- n2 + geom_area(data=newdata, color="black", fill=NA) + labs(x="z-scores", y="Density")
    n4 <- n3 + annotate("text", x=x.bounds, y = y.bounds, label = paste0(pctiles,"%"))
    print(n4)
    
  })

  ## Step 2: z-scores, low
  output$inf_zscore_low <- renderText({
    above <- max(qnorm(input$inf_pcts[1]/100), -4)
    
    if (input$inf_pcts[1] %% 10 == 1) pct <- paste0(input$inf_pcts[1], "st")
    if (input$inf_pcts[1] %% 10 == 2) pct <- paste0(input$inf_pcts[1], "nd")
    if (input$inf_pcts[1] %% 10 == 3) pct <- paste0(input$inf_pcts[1], "rd")
    if (input$inf_pcts[1] %% 10 > 3 | input$inf_pcts[1] %% 10 == 0) pct <- paste0(input$inf_pcts[1], "th")
    
    ex1 <- paste0("The z-score corresponding to the ", pct, " percentile is ", round(above, 2))
    
    if (above > -4) return(ex1) else return("")
    
  })

  ## Step 2: z-scores, high
  output$inf_zscore_high <- renderText({
    below <- min(qnorm(input$inf_pcts[2]/100), 4)
    
    
    if (input$inf_pcts[2] %% 10 == 1) pct <- paste0(input$inf_pcts[2], "st")
    if (input$inf_pcts[2] %% 10 == 2) pct <- paste0(input$inf_pcts[2], "nd")
    if (input$inf_pcts[2] %% 10 == 3) pct <- paste0(input$inf_pcts[2], "rd")
    if (input$inf_pcts[2] %% 10 > 3 | input$inf_pcts[2] %% 10 == 0) pct <- paste0(input$inf_pcts[2], "th")
    
    
    ex1 <- paste0("The z-score corresponding to the ", pct, " percentile is ", round(below, 2))
    
    if (below < 4) return(ex1) else return("")
    
  })
  

  ## Step 3: original scale, low
  output$inf_orig_low <- renderText({
    above <- max(qnorm(input$inf_pcts[1]/100), -4)
    
    x_low <- above*input$inf_sd + input$inf_mean

    if (above > -4){
      ex1 <- paste0("A z-score of ", round(above, 2), " corresponds to a value of ", round(x_low, 2))
    }else{
      ex1 <- ""
    }
    return(ex1)
  })
    
  ## Step 3: original scale, high
  output$inf_orig_high <- renderText({
    below <- min(qnorm(input$inf_pcts[2]/100), 4)
    
    x_high <- below*input$inf_sd + input$inf_mean
    
    if (below < 4){
      ex1 <- paste0("A z-score of ", round(below, 2), " corresponds to a value of ", round(x_high, 2))
    }else{
      ex1 <- ""
    }
    return(ex1)
  })
  
  
  ## Step 4: answer
  output$inf_answer <- renderText({
    above <- max(qnorm(input$inf_pcts[1]/100), -4)
    below <- min(qnorm(input$inf_pcts[2]/100), 4)
    
    x_low <- above*input$inf_sd + input$inf_mean
    x_high <- below*input$inf_sd + input$inf_mean

    
    if (input$inf_pcts[1] %% 10 == 1) pct_low <- paste0(input$inf_pcts[1], "st")
    if (input$inf_pcts[1] %% 10 == 2) pct_low <- paste0(input$inf_pcts[1], "nd")
    if (input$inf_pcts[1] %% 10 == 3) pct_low <- paste0(input$inf_pcts[1], "rd")
    if (input$inf_pcts[1] %% 10 > 3 | input$inf_pcts[1] %% 10 == 0) pct_low <- paste0(input$inf_pcts[1], "th")
    
    if (input$inf_pcts[2] %% 10 == 1) pct_high <- paste0(input$inf_pcts[2], "st")
    if (input$inf_pcts[2] %% 10 == 2) pct_high <- paste0(input$inf_pcts[2], "nd")
    if (input$inf_pcts[2] %% 10 == 3) pct_high <- paste0(input$inf_pcts[2], "rd")
    if (input$inf_pcts[2] %% 10 > 3 | input$inf_pcts[2] %% 10 == 0) pct_high <- paste0(input$inf_pcts[2], "th")
    
        
    if (above > -4 & below < 4){
      ex1 <- paste0("The ", pct_low, " to ", pct_high, " percentiles correspond to the range of ", round(x_low, 2), " to ", round(x_high, 2))
    }
    if (above > -4 & below == 4){
      ex1 <- paste0("The top ", 100-input$inf_pcts[1], "% corresponds to values above ", round(x_low, 2))
    }
    if (above == -4 & below < 4){
      ex1 <- paste0("The bottom ", input$inf_pcts[2], "% corresponds to values below ", round(x_high, 2))    
    }
    if (above == -4 & below == 4){
      ex1 <- "Please select an appropriate range of percentiles"
    }
    return(ex1)
  })

})
