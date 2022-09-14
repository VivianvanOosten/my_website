#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

    
shinyUI(fluidPage(

    titlePanel("Danceability of Songs"),
    ('This dashboard was created to allow for easy model-fitting to the spotify dataset. 
         You will be able to explore the dataset and pick the variables you want to model with.'),

    tabsetPanel(
        # Tab for the exploration part
        tabPanel('Exploration',
            sidebarLayout(
                sidebarPanel(
                    # Radio buttons to pick the predictor in the plot
                    uiOutput('var_buttons'),
                    
                    strong("Do you want to see smooth lines?"),
                    checkboxInput("smooth",
                                  "Yes",
                                  value = 0)
                ),
        
                mainPanel(
                    h4("Displaying the variable of your choice"),
                    plotOutput("choose_vars")
                )
            )
        ),
        
        #tab for the modelling part
        tabPanel('Modelling',
            sidebarLayout(
                 sidebarPanel(
                     #picking the model variables
                     uiOutput('model_var_buttons'),
                     
                     # This could be used to create a plot with only that variable later
                     #uiOutput('plotting_model_buttons'),
                     
                     selectInput('model_choice',
                                 'Choose the model to use',
                                 choices = c('Linear Regression' = 'lm',
                                             'Step Regression' = 'step',
                                             'Polynomial Regression' = 'poly')),
                     
                     #depending on which type of model, we show some options
                     uiOutput('modelling_options')
                 ),
                 mainPanel(
                     h4('Fitted model'),
                     
                     #printing the summary output here
                     verbatimTextOutput('regression')
                 )
             )
        )
    )
))
