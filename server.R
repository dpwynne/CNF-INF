## Shiny app to demonstrate cumulative normal proportion problems and inverse normal proportion problems
## Server side

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  slider_range <- reactiveValues()
  cnf_vals <- reactiveValues()
  
  observeEvent({
    input$cnf_mean
    input$cnf_sd
    input$cnf_type
  },{
    min_range <- input$cnf_mean - 4*input$cnf_sd
    max_range <- input$cnf_mean + 4*input$cnf_sd
    slider_range$step_size <- 0.1*input$cnf_sd
    slider_range$range <- c(min_range, max_range)
    if (is.null(input$slider_cnf_range)){
      slider_range$bounds <- slider_range$range
    }else{
      if (input$cnf_type == 2){
        if (input$slider_cnf_range[2] >= max_range) slider_range$bounds <- c(min_range, input$slider_cnf_range[1])
        if (input$slider_cnf_range[2] < max_range) slider_range$bounds <- c(min_range, input$slider_cnf_range[2])
      }
      if (input$cnf_type == 1){
        if (input$slider_cnf_range[1] <= min_range) slider_range$bounds <- c(input$slider_cnf_range[2], max_range)
        if (input$slider_cnf_range[1] > min_range) slider_range$bounds <- c(input$slider_cnf_range[1], max_range)
      }
      if (input$cnf_type == 3) slider_range$bounds <- c(min(input$slider_cnf_range, max_range), max(input$slider_cnf_range, min_range))
    }
  
    }, priority = 2)
  

  observeEvent({
    input$cnf_type
    input$cnf_sd
    input$cnf_mean
    input$slider_cnf_range
  },{
    if (input$cnf_type == 1){
      cnf_vals$above <- input$slider_cnf_range[1]
      cnf_vals$below <- slider_range$bounds[2]
      cnf_vals$x.labels <- cnf_vals$above
    }
    if (input$cnf_type == 2){
      cnf_vals$above <- slider_range$bounds[1]
      cnf_vals$below <- input$slider_cnf_range[2]
      cnf_vals$x.labels <- cnf_vals$below
    }
    if (input$cnf_type == 3){    
      cnf_vals$above <- input$slider_cnf_range[1]
      cnf_vals$below <- input$slider_cnf_range[2]
      cnf_vals$x.labels <- c(cnf_vals$above, cnf_vals$below)
    }

    cnf_vals$z1 <- (input$slider_cnf_range[1] - input$cnf_mean)/input$cnf_sd
    cnf_vals$z2 <- (input$slider_cnf_range[2] - input$cnf_mean)/input$cnf_sd
    cnf_vals$p1 <- pnorm(cnf_vals$z1)
    cnf_vals$p2 <- pnorm(cnf_vals$z2)
    cnf_vals$xmin <- input$slider_cnf_range[1]
    cnf_vals$xmax <- input$slider_cnf_range[2]
    
  }, priority = 1)
  
  ## need the custom slider for the CNF version
    output$slider_cnf_bounds <- renderUI({
      if (is.null(slider_range$range)){
        sliderInput("slider_cnf_range", label = "Range of Values", min = -4, max = 4, value = c(-4, 4), step = 0.1)  
      }else{  
        sliderInput("slider_cnf_range", label = "Range of Values", min = slider_range$range[1], max = slider_range$range[2], value = slider_range$bounds, step = slider_range$step_size)
      }
  })

    
    
  ## Step 1: Normal Distribution Plot
  output$cnf_plot <- renderPlot({
    if (is.null(slider_range$range) | is.null(slider_range$step_size)){
      x <- seq(-4, 4, by = 0.01)
    }else{
      x <- seq(slider_range$range[1], slider_range$range[2], by = slider_range$step_size/10)
    }
    y <- dnorm(x, input$cnf_mean, input$cnf_sd)
    
    newdata <- data.frame(x,y)
    y.bounds <- -max(y)/40

    ## four steps to creating the graph
    n1 <- ggplot(data=newdata, aes(x=x,y=y))+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    if (is.null(cnf_vals$above)){
      n2 <- n1 + geom_area(data=newdata, color="black", linetype="dashed", fill="lightgray")
    }else{
      n2 <- n1 + geom_area(data=newdata[(x>=cnf_vals$above & x<=cnf_vals$below),], color="black", linetype="dashed", fill="lightgray")
    }
    n3 <- n2 + geom_area(data=newdata, color="black", fill=NA) + labs(x="Values", y="Density")
    if (!is.null(cnf_vals$x.labels)){
      n4 <- n3 + annotate("text", x=cnf_vals$x.labels, y = y.bounds, label=as.character(cnf_vals$x.labels))
    }else{
      n4 <- n3
    }
    print(n4)
    
  })

  ## Step 2: z-score - low
  output$cnf_zscore_low <- renderText({

    if (is.null(cnf_vals$z1)){
      ex1 <- "The z-score corresponding to x = -4 is -4"
    }else{
      ex1 <- paste0("The z-score corresponding to x = ", cnf_vals$xmin, " is ", round(cnf_vals$z1, 2))
    }
    
    if (input$cnf_type == 1 | input$cnf_type == 3) return(ex1)
    if (input$cnf_type == 2) return("")
  
  })
  
  output$cnf_zscore_high <- renderText({

    if (is.null(cnf_vals$z2)){
      ex2 <- "The z-score corresponding to x = 4 is 4"
    }else{
      ex2 <- paste0("The z-score corresponding to x = ", cnf_vals$xmax, " is ", round(cnf_vals$z2, 2))  
    }
    
    if (input$cnf_type == 1) return("")
    if (input$cnf_type == 2 | input$cnf_type == 3) return(ex2)
  })
  
  ## Step 3: cumulative probabilities - low
  output$cnf_cprob_low <- renderText({
    
    if (is.null(cnf_vals$z1)){
      ex1 <- "The cumulative proportion corresponding to z = -4 is 0"
    }else{
      ex1 <- paste("The cumulative proportion corresponding to z =", round(cnf_vals$z1, 2), "is", round(cnf_vals$p1, 4))
    }
    
    if (input$cnf_type == 1 | input$cnf_type == 3) return(ex1)
    if (input$cnf_type == 2) return("")
  })
    
  ## Step 3: cumulative probabilities - high
  output$cnf_cprob_high <- renderText({
    if (is.null(cnf_vals$z2)){
      ex2 <- "The cumulative proportion corresponding to z = 4 is 1"
    }else{
      ex2 <- paste("The cumulative proportion corresponding to z =", round(cnf_vals$z2, 2), "is", round(cnf_vals$p2, 4))
    }

    if (input$cnf_type == 1) return("")
    if (input$cnf_type == 2 | input$cnf_type == 3) return(ex2)
  
  })
  
  ## Step 4: answer
  output$cnf_answer <- renderText({

  if (!is.null(cnf_vals$p2)){  
    ex1 <- paste("The proportion of values greater than", cnf_vals$xmin, "is", round(1-cnf_vals$p1, 4))
    ex2 <- paste("The proportion of values less than", cnf_vals$xmax, "is", round(cnf_vals$p2, 4))
    ex3 <- paste("The proportion of values between", cnf_vals$xmin, "and", cnf_vals$xmax, "is", round(cnf_vals$p2-cnf_vals$p1, 4))
  }else{
    ex1 <- ex2 <- ex3 <- ""
  }
    
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
