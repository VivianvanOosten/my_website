#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)


shinyServer(function(input, output) {
    
    ## ---------------- General variables --------------------------
    #columns so we can change it easily
    columns_to_use <- c("Year",
                        "BPM",
                        "Energy", 
                        "Loudness",
                        "Valence",
                        "Length", 
                        "Acousticness", 
                        "Speechiness", 
                        "Popularity")
    
   
    
    #dataframe defined outside of output for reusability
    spotify <- read_csv("datasets assignment 2/Spotify-2000.csv") %>%
        na.omit() %>% #some datacleaning 
        rename(
            'Loudness' = 'Loudness (dB)',
            'Length' = 'Length (Duration)',
            'BPM' = 'Beats Per Minute (BPM)'
        ) %>%
        select(columns_to_use, 'Danceability','Title','Artist') 
    
    #making a dataframe for easy plotting
    display_spotify <- pivot_longer(spotify, cols = columns_to_use)
    
    
    
    ## ------------------ Exploration tab --------------------------
    #interactive UI to be able to easily change the columns later
    output$var_buttons <- renderUI({
        varSelectInput("plotting_var",
                     "Choose variable to display on the x-axis:",
                     data = select(spotify, columns_to_use)
        )
    })

    output$choose_vars <- renderPlot({
        # Scatterplot based on our values
        p1 <- ggplot(spotify, aes_string(x = input$plotting_var, y = 'Danceability')) +
                    geom_point() +
                    scale_y_continuous(breaks = seq(0,100,5)) + 
                    theme_minimal() +
                    labs( # adding labels
                        #title = 'Danceability and a variable of your choice',
                        y = 'Danceability (out of 100)',
                        x = input$plotting_var)

        
        # Smoothing line 
        p2 <- if (input$smooth)
            {p1 + geom_smooth()}
              else p1
        
        print(p2)    
    })
    
    
    ## ------------------ Modelling tab --------------------------
    #picking the variables to plot
    output$model_var_buttons <- renderUI({
        # choosing which variables go in the model
        selectInput("modelling_var",
                     "Fitting on these variables",
                     choices = columns_to_use,
                    multiple = TRUE
        )
    }) # end of model_var_buttons UI
    
    output$plotting_model_buttons <- renderUI({ 
        #choosing which variables to plot
        # currently not used
        selectInput("plot_model_var",
                    "Plotting this variable",
                    choices = input$modelling_var,
                    selected = character(0)
        )
    }) # end of plotting_model_buttons UI
    
    output$modelling_options <- renderUI({ #some choices for our models
        if (input$model_choice == 'step'){
            sliderInput('nr_steps',
                        'Select number of steps',
                        min = 1, max = 10, step = 1,
                        value = 3)
        } else if (input$model_choice == 'poly'){
            sliderInput('nr_degrees',
                        'Select number of degrees',
                        min = 1, max = 5,
                        value = 2)
        }
    }) # end of modelling options UI
    
    formula <- reactive({ 
        # most annoying part of the code
        # if you think of a better way to do this pls let me know
        
        #starts off with danceability
        form <- 'Danceability ~'
        first <- TRUE
        
        if (input$model_choice == 'lm'){
            for (variable in input$modelling_var){
                if (first){ #first one is a little different
                    form <- paste(form, variable)
                    first <- FALSE
                } else { #rest needs +'s
                    form <- paste(form, variable, sep = ' + ')
                }
            }
        } else if (input$model_choice == 'step'){
            for (variable in input$modelling_var){
                if (first){#adding the cut variable
                    form <- paste0(form, 'cut(', variable,',',input$nr_steps, ')')
                    first <- FALSE
                } else {
                    form <- paste(form, ' + cut(', variable, ',', input$nr_steps, ')')
                }
            }
        } else if (input$model_choice == 'poly'){
            for (variable in input$modelling_var){
                if (first){
                    form <- paste0(form, 'poly(', variable,',',input$nr_degrees, ')')
                    first <- FALSE
                } else {
                    form <- paste(form, ' + poly(', variable, ',', input$nr_degrees, ')')
                }
            }
        }
        print(form)
        
        form
    })
    
    output$regression <- renderPrint({ # making LM predictions
        validate( #checking to make sure we have inputs (otherwise error)
            need(input$modelling_var, message = 'First select modelling variables')
        )
        model <- lm(formula =  formula(), data = spotify) 
        summary(model)
    })
    
    #unused code
  #  output$model_plot <- renderPlot({
  #      chosen_model <- switch(input$model_choice,
  #                            'lm' = lm_model(),
  #                            'step' = step(),
  #                            'poly' = poly()
  #      )
  #      print(input$plot_model_var)
  #      if(input$model_choice == 'lm'){
  #          plot(chosen_model)
  #      }
  #  })
  #  
    
    
})

#ggplot(data = dia.filtered(), mapping = aes(x = carat, y = price, color = cut)) +
#    geom_point() +
#    geom_line(data = tibble(carat = carat_pred, price = y_pred.lm), 
#              size = input$linear,
#              col = 'red') +
#    geom_line(data = tibble(carat = carat_pred, price = y_pred.poly), 
#              size = input$poly,
#              col = 'blue') +
#    geom_line(data = tibble(carat = carat_pred, price = y_pred.step), 
#              size = input$step,
#              col = 'green')  +
#    scale_colour_brewer(palette = input$colors) 
#})
