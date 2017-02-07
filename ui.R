## Shiny app to demonstrate cumulative normal proportion problems and inverse normal proportion problems
## UI side

library(shiny)

shinyUI(navbarPage("Normal Distribution Proportion Problems",
  
  tabPanel("Values to Proportions",
           sidebarPanel(
             numericInput("cnf_mean", label="Mean", value=0),
             numericInput("cnf_sd", label="Standard Deviation", value=1),
             radioButtons("cnf_type", label="Type of Problem", 
                          choices = list("Above X" = 1, "Below Y" = 2, "Between X and Y" = 3), selected = 2),
             uiOutput("slider_cnf_bounds")
           ),
           mainPanel(
             h4("Step 1: Draw Normal Distribution and Shade Area"),
             plotOutput("cnf_plot", width="480px", height="400px"),
             h4("Step 2: Compute Z-Scores"),
             textOutput("cnf_zscore_low"),
             textOutput("cnf_zscore_high"),
             h4("Step 3: Find Cumulative Proportions"),
             textOutput("cnf_cprob_low"),
             textOutput("cnf_cprob_high"),
             h4("Step 4: Answer Question"),
             textOutput("cnf_answer")
           )
           ),
  tabPanel("Percentiles to Values",
           sidebarPanel(
             numericInput("inf_mean", label="Mean", value=0),
             numericInput("inf_sd", label="Standard Deviation", value=1),
             sliderInput("inf_pcts", label="Range of Percentiles", min = 0, max = 100, value = c(0, 100), step = 1)
           ),
           mainPanel(
             h4("Step 1: Draw Standard Normal Distribution and Shade Area"),
             plotOutput("inf_plot", width="480px", height="400px"),
             h4("Step 2: Compute Z-Scores"),
             textOutput("inf_zscore_low"),
             textOutput("inf_zscore_high"),
             h4("Step 3: Convert to Original Scale"),
             textOutput("inf_orig_low"),
             textOutput("inf_orig_high"),
             h4("Step 4: Answer Question"),
             textOutput("inf_answer")
           )
           
           ))
)
