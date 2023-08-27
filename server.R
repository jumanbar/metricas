library(shiny)

source("R/funciones.R", local = TRUE, encoding = "UTF-8")

shinyServer(function(input, output) {

  d <- reactive({
    file <- input$archivo
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "txt"),
                  "Por favor, subir un archivo con extensión .csv o .txt"))
    out <- read.csv(file$datapath, header = TRUE, stringsAsFactors = FALSE)
    names(out) <- tolower(names(out))
    return(out)
  })

  output$hover <- renderText({
    paste(input$plot_hover$x, "-", input$plot_hover$y)
  })

  output$click <- renderText({
    xy <- input$plot_click
    req(xy)
    paste(round(xy$x, 2), "-", round(xy$y, 2))
  })

  output$click_pts <- renderTable({
    xy <- input$plot_click
    res <- nearPoints(d(), input$plot_click, "m", "e", threshold = 10)
    if (nrow(res) == 0)
      return()
    res
  })

  output$brush_pts <- renderTable({
    xy <- input$plot_brush
    res <- brushedPoints(d(), input$plot_brush, "m", "e")
    if (nrow(res) == 0)
      return()
    res
  })

  output$scatter <- renderPlot({
        g_em(d(), alpha = .7)
    })

  output$metricas_out <- DT::renderDT({
    out <- calc_metricas(d())
    out$valor <- round(out$valor, 2)
    DT::datatable(
      extensions = 'Buttons',
      class = 'cell-border stripe',
      rownames = FALSE,
      options = list(
        dom = "Bf",
        pageLength = nrow(out),
        paging = FALSE,
        buttons = list("copy", "csv", "excel")
      ),
      data = out
    )
  })

})
