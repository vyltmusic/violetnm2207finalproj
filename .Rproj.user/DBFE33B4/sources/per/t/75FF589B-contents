library(shiny)
library(tidyverse)
library(dnddata)

dnd_classes <- c("Artificer", "Barbarian", "Bard", "Blood Hunter", "Cleric", "Druid", "Fighter", "Monk", "Paladin", "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")

dnd_stats <- c("HP", "AC", "Str", "Dex", "Con", "Int", "Wis", "Cha")

class_anyall_options <- c("Show characters with at least one of the selected classes", "Show characters with only the selected classes")

any(is.na(select(dnd_chars_unique, dnd_stats)))
# rowSums(is.na(select(dnd_chars_unique, dnd_stats))) > 0L

dnd_chars_unique_edited <- dnd_chars_unique
# for(i in dnd_stats){
#   j <- dnd_chars_unique_edited[[i]]
#   j <- dnd_chars_unique_edited[["HP"]]
#   #dnd_chars_unique_edited <- 
#     dnd_chars_unique_edited %>%
#     filter(j > (quantile(j, 0.75) + 1.5 * (quantile(j, 0.75) - quantile(j, 0.25))))
# }
  filter(dnd_chars_unique, rowSums(is.na(select(dnd_chars_unique, dnd_stats))) > 0L)
for(i in dnd_classes){
  dnd_chars_unique_edited <- dnd_chars_unique_edited %>%
    mutate(newcol = grepl(i, dnd_chars_unique_edited$justClass))
  j <- colnames(dnd_chars_unique_edited)
  colnames(dnd_chars_unique_edited) <- c(j[1:(length(j)-1)], i)
}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("DnD 5e Statistics Visualization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      # textInput(inputId = "caption",
      #           label = "Caption:",
      #           value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      # selectInput(inputId = "class",
      #             label = "Choose a class:",
      #             choices = dnd_classes),
      selectInput(inputId = "stat",
                  label = "Choose a statistic:",
                  choices = dnd_stats),
      checkboxGroupInput("class",
                         label = "Choose one or more classes:",
                         choices = dnd_classes),
      radioButtons("class_anyall",
                   label = "Note: Having no classes selected is equivalent to selecting all classes.",
                   choices = class_anyall_options,
                   inline = TRUE),
      # Input: Numeric entry for number of obs to view ----
      # numericInput(inputId = "obs",
      #              label = "Number of observations to view:",
      #              value = 10),
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      sliderInput(inputId = "min_level",
                   label = "Minimum level:",
                   value = 1,
                   min = 1,
                   max = 20),
      sliderInput(inputId = "max_level",
                  label = "Maximum level:",
                  value = 1,
                  min = 1,
                  max = 20),
      sliderInput(inputId = "outlier_range",
                  label = "Outlier Range (Scaling factor of IQR):",
                  value = 3,
                  min = 1.5,
                  max = 10,
                  step = 0.5),
      p("1.5 * IQR is usually considered a mild outlier, whereas 3 * IQR is usually considered an extreme outlier.")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      
      # h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      # tableOutput("view"),
      plotOutput(outputId = "distPlot"),
      h5("High outliers:"),
      tableOutput("outliersHigh"),
      h5("Low outliers:"),
      tableOutput("outliersLow"),
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
# 
#   levelInput <- reactive({
#     input$level
#   })
  observe({
    updateSliderInput(session, "min_level", max = input$max_level)
    updateSliderInput(session, "max_level", min = input$min_level)
  })
  
  class_anyallInput <- reactive({
    case_when(
      input$class_anyall == class_anyall_options[1] ~ TRUE,
      input$class_anyall == class_anyall_options[2] ~ FALSE,
      .default = TRUE
    )
  })
  datasetInput <- reactive({
    mutate(dnd_chars_unique_edited, selectedClass_any = if_any(.cols = input$class), selectedClass_all = if_all(.cols = input$class), selectedStat = dnd_chars_unique_edited[[input$stat]])
  })
  #datasetInput <- reactive({
   # filter(dnd_chars_unique_edited, level == input$level) %>%
      #.[[input$class]]
   # switch(input$class,
           #"Artificer" = dnd_chars_unique_edited["Artificer"], "Barbarian", "Bard", "Blood Hunter", "Cleric", "Druid", "Fighter", "Monk", "Paladin", "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard",
          # "rock" = rock,
          # "pressure" = pressure,
          # "cars" = cars)
  #})
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    if(class_anyallInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level) %>%
      select(justClass, level, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    summary(dataset)
  })
  # ifelse(class_anyallInput(), (selectedClass_any == TRUE), (selectedClass_all == TRUE))
  
  output$distPlot <- renderPlot({
    if(class_anyallInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat <= q3 + (input$outlier_range * iqr), selectedStat >= q1 - (input$outlier_range * iqr))
    # x    <- dataset$selectedStat
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "a",
    #      main = "Histogram")
    
    ggplot(dataset, aes(x = selectedStat)) +
      geom_bar() +
      labs(x = input$stat, title = paste("Frequency of ", input$stat, " values between levels ",input$min_level," and ",input$max_level, sep=""), caption = paste("(Classes: ",ifelse(nchar(input$class) < 2, "All", paste(input$class, collapse=", ")),")", sep=""))
    
  })
  
  # output$distPlot2 <- renderPlot({
  #   if(class_anyallInput()){
  #     dataset <- filter(datasetInput(), selectedClass_any == TRUE)
  #   } else {
  #     dataset <- filter(datasetInput(), selectedClass_all == TRUE)
  #   }
  #   dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
  #   # x    <- dataset$selectedStat
  #   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # hist(x, breaks = bins, col = "#75AADB", border = "white",
  #   #      xlab = "a",
  #   #      main = "Histogram")
  #   ggplot(dataset, aes(x = selectedStat, fill = processedRace)) +
  #     geom_bar(position = "fill")
  #   
  # })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    if(class_anyallInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    head(dataset, n = input$obs)
  })
  
  output$outliersHigh <- renderTable({
    if(class_anyallInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat > q3 + (input$outlier_range * iqr)) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    dataset
  })
  
  output$outliersLow <- renderTable({
    if(class_anyallInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat < q1 - (input$outlier_range * iqr)) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    dataset
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
