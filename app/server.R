#### setup server for word prediction app
#### written by shelby bachman, 2020
#### last updated 3 may 2020

shinyServer(function(input, output, session) {
  
    source('functions/predict_next_word.R')
  
    # based on action button
    prediction_df <- eventReactive(input$go, {
      modelInput <- input$input_string # user inputted string
      predict_next_word(modelInput, n_to_return = 50, return_df = TRUE) # predict next word
    })

    # list of 5 most likely next words
    # output$prediction_words <- renderText({
    #   data <- prediction_df()
    #   data$end[1:5]
    # })

    # vertical barplot of 50 most likely words
    output$prediction_plot <- renderPlot({
      data <- prediction_df()
      sum_probs <- sum(data$prob)
      data <- data %>%
          mutate(prob_adj = prob / sum_probs)
      data <- data[1:50,]
      ggplot(data, aes(x = reorder(end, prob_adj), y = prob_adj)) +
        geom_bar(fill = '#738678', colour = '#222222', stat = "identity") +
        labs(x = '', y = '') +
        theme_pubr(base_size = 14) + 
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              line = element_blank()) +
        coord_flip()
    }, height = 800, width = 400)


})