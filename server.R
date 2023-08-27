library(shiny)

source("R/funciones.R", local = TRUE, encoding = "UTF-8")

shinyServer(function(input, output) {

	output$scatter <- renderPlot({
		g_em(d(), alpha = .7)
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

}

