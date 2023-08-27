library(shiny)

srvfun <- function(input, output) {

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

}

# Define server logic required to draw a histogram
shinyServer(srvfun)

