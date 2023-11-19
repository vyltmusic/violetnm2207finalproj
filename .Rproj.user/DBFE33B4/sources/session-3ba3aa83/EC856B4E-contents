#App-DnD2

library(shiny)
library(tidyverse)
library(dnddata)

dnd_classes <- c("Artificer", "Barbarian", "Bard", "Blood Hunter", "Cleric", "Druid", "Fighter", "Monk", "Paladin", "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")

dnd_stats <- c("Str", "Dex", "Con", "Int", "Wis", "Cha")

dnd_chars_unique_edited <- dnd_chars_unique
filter(dnd_chars_unique, rowSums(is.na(select(dnd_chars_unique, dnd_stats))) > 0L)
for(i in dnd_classes){
  dnd_chars_unique_edited <- dnd_chars_unique_edited %>%
    mutate(newcol = grepl(i, dnd_chars_unique_edited$justClass))
  j <- colnames(dnd_chars_unique_edited)
  colnames(dnd_chars_unique_edited) <- c(j[1:(length(j)-1)], i)
}

tier_options <- c("Tier 1 (Levels 1 - 4)", "Tier 2 (Levels 5 - 10)", "Tier 3 (Levels 11 - 16)", "Tier 4 (Levels 17 - 20)")

summary_options <- c("Mean", "Median", "Mode")

mode <- function(x, na.rm = FALSE) {
  
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("DnD 5e Statistics Visualization - Exploring the Fighter"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      checkboxGroupInput("stats",
                         label = "Choose one or more statistics:",
                         choices = dnd_stats,
                         selected = dnd_stats),
      radioButtons("multiclass",
                   label = "Include multiclass fighters?",
                   choices = c("Yes", "No"),
                   inline = TRUE),
      radioButtons("summary",
                   label = "Choose a summary statistic:",
                   choices = summary_options,
                   inline = TRUE),
      radioButtons("tier",
                   label = "Gameplay tier:",
                   choices = tier_options,
                   inline = FALSE),
      numericInput(inputId = "obs",
                   label = "Number of observations per page:",
                   value = 10,
                   min = 1),
      numericInput(inputId = "page",
                   label = "Observation page:",
                   value = 1,
                   min = 1),
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(

      textOutput("count"),
      plotOutput(outputId = "distPlot", click = "plot_click"),
      verbatimTextOutput("click_info"),
      textOutput("obs_title"),
      tableOutput("observations"),
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  tierInput <- reactive({
    if(input$tier == tier_options[1]){
      tier <- 1:4
    } else if(input$tier == tier_options[2]){
      tier <- 5:10
    } else if(input$tier == tier_options[3]){
      tier <- 11:16
    } else if(input$tier == tier_options[4]){
      tier <- 17:20
    }
    tier
  })
  
  multiclassInput <- reactive({
    case_when(
      input$multiclass == "Yes" ~ TRUE,
      input$multiclass == "No" ~ FALSE,
      .default = 1
    )
  })
  
  click_tick <- reactive ({
    (input$plot_click$x > 0)
  })
  
  datasetInput <- reactive({
    selected_vector_any <- dnd_chars_unique_edited %>%
      transmute(selected = if_any(.cols = "Fighter")) %>%
      pull()
    
    selected_vector_all <- dnd_chars_unique_edited %>%
      transmute(selected = if_all(.cols = "Fighter")) %>%
      pull()
    
    not_selected_vector_any <- dnd_chars_unique_edited %>%
      transmute(not_selected = if_any(.cols = all_of(setdiff(dnd_classes, "Fighter")))) %>%
      pull()
    
    dnd_chars_unique_edited <- dnd_chars_unique_edited %>%
      mutate(selectedClass_any = selected_vector_any,
             selectedClass_only = !not_selected_vector_any & selected_vector_any,
             selectedClass_all = !not_selected_vector_any  & selected_vector_all)
  })
  
  datasetInput_selected <- reactive({
    if(multiclassInput()){
      dataset <- filter(datasetInput(), selectedClass_any == TRUE)
    } else {
      dataset <- filter(datasetInput(), selectedClass_all == TRUE)
    }
    dataset <- filter(dataset, level %in% tierInput()) %>%
      select(level, processedRace, justClass, subclass, HP, AC, Str, Dex, Con, Int, Wis, Cha)
    dataset
  })
  
  plotData <- reactive({
    dataset <- datasetInput_selected()

    sum_stat <- c()
    for(i in input$stats){
      if(input$summary == "Mean"){
        sum_stat <- c(sum_stat, mean(dataset[[i]]))
      } else if(input$summary == "Median"){
        sum_stat <- c(sum_stat, median(dataset[[i]]))
      } else {
        sum_stat <- c(sum_stat, mode(dataset[[i]]))
      }
    }

    plot_data <- data.frame(Statistics = input$stats, SumStat = sum_stat)
    plot_data
  })
  
  
  output$count <- renderText({
    count <- nrow(datasetInput_selected())
    paste("Total Observation Count: ", count, sep = "")
  })
  
  output$distPlot <- renderPlot({
    ggplot(plotData(), aes(x = factor(Statistics, level = dnd_stats), y = SumStat)) +
      geom_bar(stat="identity") +
      labs(x = "Ability Scores", y = input$summary, title = paste("Bar Graph of the ", tolower(input$summary), "s of DnD 5e ability scores for Fighters from levels ",first(tierInput())," to ",last(tierInput()), sep="")) +
      scale_y_continuous(limits = c(0, 20.2))
  })
  
  output$click_info <- renderText({
    if(length(input$plot_click) > 0){
      selected_x <- plotData()[round(input$plot_click$x, 0), 1]
      paste("Click on the graph to view details\n", "Ability score: ", selected_x, "\n", input$summary, ": ", round(filter(plotData(), Statistics == selected_x)[2], 2), sep="")
    } else {
      paste("Click on the graph to view details")
    }
  })
  
  output$obs_title <- renderText({
    page <- (input$page) * input$obs
    paste("Observations (Showing entries ", page - (input$obs - 1)," - " ,page, "):", sep = "")
  })
  
  
  output$observations <- renderTable({
    dataset <- datasetInput_selected()
    page <- (input$page) * input$obs
    dataset[(page - (input$obs - 1)):page,]
  })
}

# Create Shiny app ----
shinyApp(ui, server)
