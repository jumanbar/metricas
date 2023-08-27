library(shiny)

shinyUI(
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      tags$a("Documentación de las métricas", href = 'metricas.pdf'),
      tags$hr(),
      tags$h5("punto"),
      tableOutput("click_pts"),
      tags$hr(),
      tags$h5("area"),
      tableOutput("brush_pts")
    ),

    mainPanel(
      width = 9,
      fluidRow(
        column(width = 8,
               align = "center",
               plotOutput("scatter",
                          click = "plot_click",
                          brush = "plot_brush",
                          width = "550px",
                          height = "550px"
                          ),
               tags$br(),
               textOutput("click")
               ),
        column(width = 4, DT::DTOutput('metricas_out'))
        )
      )
    )
)
