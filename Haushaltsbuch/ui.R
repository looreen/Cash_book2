
library(shiny)
source('first script.R')
source('categories.R')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Deine Monatsuebersicht"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Bitte lade hier die Original-CSV-Datei hoch!",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            )
            # ,
            # tags$hr(),
            # checkboxInput("header", "Header", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput('chart1'),
            tableOutput('table1')
        )
    )
))
