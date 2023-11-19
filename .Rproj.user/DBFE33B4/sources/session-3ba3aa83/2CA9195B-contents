#App-DnD

library(shiny)
library(tidyverse)
library(dnddata)

dnd_classes <- c("Artificer", "Barbarian", "Bard", "Blood Hunter", "Cleric", "Druid", "Fighter", "Monk", "Paladin", "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")

dnd_stats <- c("HP", "AC", "Str", "Dex", "Con", "Int", "Wis", "Cha")

class_anyall_options <- c("Show characters with at least one of the selected classes", "Show characters with only the selected classes", "Show characters with exactly the selected classes")

graph_options <- c("Bar Graph", "Histogram")

dnd_chars_unique_edited <- dnd_chars_unique
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
      
      selectInput(inputId = "stat",
                  label = "Choose a DnD statistic:",
                  choices = dnd_stats),
      checkboxGroupInput("class",
                         label = "Choose one or more classes:",
                         choices = dnd_classes),
      radioButtons("class_anyall",
                   label = "Note: If no classes are selected, option 1 is equivalent to showing all characters in the dataset.",
                   choices = class_anyall_options,
                   inline = TRUE),
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
      numericInput(inputId = "obs",
                   label = "Number of observations per page:",
                   value = 10,
                   min = 1),
      numericInput(inputId = "page",
                  label = "Observation page:",
                  value = 1,
                  min = 1),
      sliderInput(inputId = "outlier_range",
                  label = "Outlier Range (Scaling factor of IQR):",
                  value = 3,
                  min = 1.5,
                  max = 10,
                  step = 0.5),
      p("Note: A scaling factor of 1.5 is usually considered a mild outlier, whereas 3 is usually considered an extreme outlier."),
      radioButtons("graph",
                   label = "Choose your type of graph:",
                   choices = graph_options,
                   inline = TRUE),
      conditionalPanel(
        condition = "input.graph == 'Histogram'",
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 2,
                    max = 50,
                    value = 30)
      )
    ),
   
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("count"),
      h5("Summary:"),
      verbatimTextOutput("summary"),
      plotOutput(outputId = "distPlot", click = "plot_click"),
      verbatimTextOutput("click_info"),
      textOutput("obs_title"),
      tableOutput("observations"),
      h5("High outliers:"),
      tableOutput("outliersHigh"),
      h5("Low outliers:"),
      tableOutput("outliersLow"),
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  click_tick <- reactive ({
    (input$plot_click$x > 0)
  })
  
  observeEvent(input$obs, {
    if(input$min_level > input$max_level){
      updateSliderInput(session, "min_level", value = input$max_level)
    }
  })
  
  observeEvent(input$min_level, {
    if(input$min_level > input$max_level){
      updateSliderInput(session, "min_level", value = input$max_level)
    }
  })
  
  observeEvent(input$max_level, {
    if(input$max_level < input$min_level){
      updateSliderInput(session, "max_level", value = input$min_level)
    }
  })
  
  class_anyallInput <- reactive({
    case_when(
      input$class_anyall == class_anyall_options[1] ~ 1,
      input$class_anyall == class_anyall_options[2] ~ 2,
      input$class_anyall == class_anyall_options[3] ~ 3,
      .default = 1
    )
  })
  
  datasetInput <- reactive({
    
    selected_vector_any <- dnd_chars_unique_edited %>%
      transmute(selected = if_any(.cols = all_of(input$class))) %>%
      pull()
    
    selected_vector_all <- dnd_chars_unique_edited %>%
      transmute(selected = if_all(.cols = all_of(input$class))) %>%
      pull()
    
    not_selected_vector_any <- dnd_chars_unique_edited %>%
      transmute(not_selected = if_any(.cols = all_of(setdiff(dnd_classes, input$class)))) %>%
      pull()
    
    dnd_chars_unique_edited <- dnd_chars_unique_edited %>%
      mutate(selectedStat = .data[[input$stat]],
             selectedClass_any = selected_vector_any,
             selectedClass_only = !not_selected_vector_any & selected_vector_any,
             selectedClass_all = !not_selected_vector_any  & selected_vector_all)
  })
  
  datasetInput_selected <- reactive({
    if(class_anyallInput() == 1){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else if(class_anyallInput() == 2) {
      dataset <- filter(datasetInput(), selectedClass_only == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
    dataset
  })
  
  datasetInput_no_outliers <- reactive({
    dataset <- datasetInput_selected()
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat <= q3 + (input$outlier_range * iqr), selectedStat >= q1 - (input$outlier_range * iqr)) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    dataset
  })

  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    if(class_anyallInput() == 1){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else if(class_anyallInput() == 2) {
      dataset <- filter(datasetInput(), selectedClass_only == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level) %>%
      select(level, input$stat)
    summary(dataset)
  })
  
  output$count <- renderText({
    count_no_outliers <- nrow(datasetInput_no_outliers())
    count <- nrow(datasetInput_selected())
    paste("Total Observation Count: ", count_no_outliers, " (", count - count_no_outliers, " outliers omitted)", sep = "")
  })
  
  output$distPlot <- renderPlot({
    if(class_anyallInput() == 1){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else if(class_anyallInput() == 2) {
      dataset <- filter(datasetInput(), selectedClass_only == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level >= input$min_level, level <= input$max_level)
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat <= q3 + (input$outlier_range * iqr), selectedStat >= q1 - (input$outlier_range * iqr))

    if(input$graph == graph_options[1]){
      ggplot(dataset, aes(x = selectedStat)) +
        geom_bar() +
        labs(x = input$stat, title = paste("Bar Graph of Frequency of ", input$stat, " values between levels ",input$min_level," and ",input$max_level, sep=""), caption = paste("(Classes: ",ifelse(nchar(input$class) < 2, "All", paste(input$class, collapse=", ")),")", sep=""))
    } else {
      x <- dataset$selectedStat
      ggplot(dataset, aes(x = selectedStat)) +
        geom_histogram(binwidth = length(unique(x)) / (input$bins - 1)) +
        labs(x = input$stat, title = paste("Histogram of Frequency of ", input$stat, " values between levels ",input$min_level," and ",input$max_level, sep=""), caption = paste("(Classes: ",ifelse(nchar(input$class) < 2, "All", paste(input$class, collapse=", ")),")", sep=""))
    }
  })
  
  output$click_info <- renderText({
    if(length(input$plot_click) > 0){
      dataset <- datasetInput_selected()
      vector <- dataset[[input$stat]]
      selected_x <- round(input$plot_click$x, 0)
      paste("Click on the graph to view details\n", input$stat, ": ", selected_x, "\nCount: ", length(vector[vector == selected_x]), sep="")
    } else {
      paste("Click on the graph to view details")
    }
  })
  
  
  
  output$obs_title <- renderText({
    page <- (input$page) * input$obs
    paste("Observations (Showing entries ", page - (input$obs - 1)," - " ,page, "):", sep = "")
  })
  
  
  output$observations <- renderTable({
    dataset <- datasetInput_no_outliers()
    page <- (input$page) * input$obs
    dataset[(page - (input$obs - 1)):page,]
  })
  
  output$outliersHigh <- renderTable({
    dataset <- datasetInput_selected()
    q1 <- quantile(dataset$selectedStat, 0.25)
    q3 <- quantile(dataset$selectedStat, 0.75)
    iqr <- q3 - q1
    dataset <- dataset %>%
      filter(selectedStat > q3 + (input$outlier_range * iqr)) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    dataset
  })
  
  output$outliersLow <- renderTable({
    dataset <- datasetInput_selected()
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
