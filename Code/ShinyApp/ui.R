library(shiny)
shinyUI(fluidPage(
  headerPanel("Body Fat Calculator (For Adult Male)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Please select the model you prefer:",
                  list("Weight + Height", "Weight + Abdomen"), 
                  selected = "Weight + Height"),
      selectInput("metric", "Please select your preferance on measurement:",
                   list("The Metric System", "The Imperial Unit"), 
                  selected = "The Metric System"),
      conditionalPanel(
        condition = "input.metric == 'The Metric System'", 
        numericInput("weight1", "Weight: (kg)", value = 75)),
      conditionalPanel(
        condition = "input.metric == 'The Imperial Unit'", 
        numericInput("weight2", "Weight: (lb)", value = 165)),
      conditionalPanel(
        condition = "input.metric == 'The Metric System' && input.model == 'Weight + Abdomen'",
        numericInput("abdomen1", "Abdomen: (cm)", value = 90)),
      conditionalPanel(
        condition = "input.metric == 'The Imperial Unit' && input.model == 'Weight + Abdomen'",
        numericInput("abdomen2", "Abdomen: (inch)", value = 35)),
      conditionalPanel(
        condition = "input.metric == 'The Metric System' && input.model == 'Weight + Height'",
        numericInput("height1", "Height: (cm)", value = 175)),
      conditionalPanel(
        condition = "input.metric == 'The Imperial Unit' && input.model == 'Weight + Height'",
        numericInput("height2", "Height: (inch)", value = 70)),
      br(),
      br(),
      #actionButton("submit", "Click to Calculate."),
      helpText("If you have questions, please email to:"),
      helpText("sshen82@wisc.edu"),
      helpText("hwang789@wisc.edu"),
      helpText("xzhang2278@wisc.edu"),
      helpText("csh46@wisc.edu")
    ),
    mainPanel(
      fluidRow(column(5,h4(div(tableOutput("ref"), style = "font-size:80%"))),
               column(7,h4(plotOutput("plot")))),
      helpText("This table comes from https://www.acefitness.org/education-and-resources/lifestyle/blog/112/what-are-the-guidelines-for-percentage-of-body-fat-loss."),
      h2(htmlOutput("conclusion"))
    )
  )
))
  
