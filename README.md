ui <- fluidPage(
  titlePanel(" "),
  sidebarLayout(
   sidebarPanel(
    fileInput("csvs",
                label = "Subir serie de tiempo en formato .csv",
                multiple = TRUE),
    h4("UAM Azcapotzalco"),
    helpText("Desarrollado en R Shiny, por Jorge Stephano Soriano Urbán"),    
    textInput("caption", "Estado de la República Mexicana:", ""),
    textInput("caption1", "Actividad económica:", ""),
    sliderInput("ForecastPer",
                "Número de períodos a pronosticar:",
                min = 1,
                max = 25,
                value = 5,
              )
               ),

    mainPanel(
      h2(".::::::Time Series Forecasting::::::."),
      h1(""),
      h4("Gráfica de la serie en niveles:"),
      plotOutput("niveles"),
      h4("Estadístico Ljung-Box:"),
      plotOutput("plot"),
      h4("Diferencias de la serie:"),
      plotOutput("difer"),
      h4("Pronóstico:"),
      plotOutput("pron"),
      h4("Valores del pronóstico:"),
      tableOutput("values_pron"),
      tableOutput("values1_pron")
    )
  )
)


server <- function(input, output) {

  output$niveles <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anho.min <- min(anios)
      anho.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anho.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anho.min, mes), frequency = frec)
      edo <- input$caption
      act <- input$caption1
      plot(var, main = paste("Actividad ", act, " de ", edo, " de ", anho.min, " a ", anho.max, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act, sep = ""), xlab = 'Año')
    }
  })


  output$plot <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anho.min <- min(anios)
      anho.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anho.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anho.min, mes), frequency = frec)
      modelo <- forecast::auto.arima(var)
      tsdiag(modelo)
    }
  })

  output$difer <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anho.min <- min(anios)
      anho.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anho.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anho.min, mes), frequency = frec)
      n_diff <- forecast::ndiffs(var, test = c("adf"))
      var_diff <- diff(var, n_diff)
      edo <- input$caption
      act <- input$caption1
      plot(var_diff, main = paste("Diferencias de la actividad ", act, " de ", edo, " de ", anho.min, " a ", anho.max, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act, sep = ""), xlab = 'Año')
      abline(h = mean(var_diff), col = 'blue')
    }
  })

  output$pron <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anho.min <- min(anios)
      anho.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anho.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anho.min, mes), frequency = frec)
      pronostico <- forecast::forecast(var, h = input$ForecastPer)
      edo <- input$caption
      act <- input$caption1
      plot(pronostico, main = paste("Pronóstico de la actividad ", act, " de ", edo, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act, sep = ""), xlab = 'Año')
    }
  })
  output$values_pron <- renderTable({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anho.min <- min(anios)
      anho.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anho.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anho.min, mes), frequency = frec)
      pronostico <- forecast::forecast(var, h = input$ForecastPer)
      print(pronostico[2])
    }
  })
}

shinyApp(ui = ui, server = server)
