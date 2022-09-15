#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Cálculo de métricas"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'archivo',
        'Cargar archivo CSV',
        buttonLabel = 'Buscar...',
        placeholder = 'Sin archivo seleccionado aún'
      ),
      helpText(
        'El archivo debe ser texto separado por comas, con puntos',
        'para decimales; además debe tener una columna "m" con',
        'valores medidos in situ, y otra columna "e", con valores',
        'estimados por el modelo (siempre hablando de Clorofila-a)'
      ),
      tags$h4("Enlaces:"),
      tags$a("Archivo CSV de ejemplo", href = 'ejemplo.csv'),
      tags$br(),
      tags$a("Documentación de las métricas", href = 'metricas.pdf')
    ),

    mainPanel(
      plotOutput("scatter"),
      DT::DTOutput('metricas_out')
      ),

  )
))
