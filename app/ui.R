#### setup UI for word prediction app
#### written by shelby bachman, 2020
#### last updated 3 may 2020

# define UI which shows predictions and a histogram
shinyUI(fluidPage(theme = shinytheme('yeti'),

    titlePanel('Word prediction app'),

    # sidebar to enter string of word(s)
    sidebarLayout(
        sidebarPanel(
            textInput('input_string',
                      'Enter a few words: ',
                      value = ''),
            br(),
            #submitButton('Predict!')
            actionButton('go', 'Predict!')
            ),

        # show 5 most likely next words
        # and histogram of the probabilities of the most likely 50
        mainPanel(
        	#h3('Five most likely next words:'),
        	#textOutput('prediction_words'),
        	#hr(),
        	h3('Most likely next words:'),
            plotOutput('prediction_plot', width = '100%')
        )
    )
))
