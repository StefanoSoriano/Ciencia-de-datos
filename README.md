## Predicción automática de series de tiempo (modelos ARIMA y SARIMA)

En este repositorio se encuentra una aplicación (en desarrollo) en lenguaje R que se ejecuta en R Shiny, para forecasting de series de tiempo en modo automático, modelos ARIMA y SARIMA.

#### Captura de pantalla de la aplicación: 
<img src="https://github.com/StefanoSoriano/Mi-primera-web-app-en-R-Shiny/blob/master/Time%20Series%20Forecasting.png?raw=true" alt="drawing"/>

###### Fuente: Elaboración propia en RStudio.

### Script en código HTML y R que se ejecuta en [R Shiny](https://shiny.rstudio.com/)

```r

ui <- fluidPage(
  titlePanel(" "),
  sidebarLayout(
   sidebarPanel(
    h4("                UAM Azcapotzalco"),
    h6("Desarrollado en R Shiny, por Jorge Stephano Soriano Urbán"),
    h1(" "),
    fileInput("csvs",
                label = "Subir serie de tiempo en formato .csv",
                multiple = TRUE),
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
      h1("              .::::::Time Series Forecasting::::::."),
      h6("                                                  
                                                     ARIMA and SARIMA models"),
      h1("                  "),
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
      anio.min <- min(anios)
      anio.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anio.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anio.min, mes), frequency = frec)
      edo <- input$caption
      act <- input$caption1
      plot(var, main = paste("Actividad ", act, " de ", edo, " de ", anio.min, " a ", anio.max, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act, sep = ""), xlab = 'Año')
    }
  })


  output$plot <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      var <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      var_ts <- var[, 2]
      var <- var[, 1]
      anios <- cbind(as.numeric(substr(var, 1, 4)))
      anio.min <- min(anios)
      anio.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anio.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anio.min, mes), frequency = frec)
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
      anio.min <- min(anios)
      anio.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anio.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anio.min, mes), frequency = frec)
      n_diff <- forecast::ndiffs(var, test = c("adf"))
      var_diff <- diff(var, n_diff)
      edo <- input$caption
      act <- input$caption1
      plot(var_diff, main = paste("Diferencias de la actividad ", act, " de ", edo, " de ", anio.min, " a ", anio.max, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act, sep = ""), xlab = 'Año')
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
      anio.min <- min(anios)
      anio.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anio.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anio.min, mes), frequency = frec)
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
      anio.min <- min(anios)
      anio.max <- max(anios)
      mes <- cbind(as.numeric(gsub("(.*)/", "", var)))
      mes <- mes[1,]
      frec <- dplyr::filter(as.data.frame(anios), anios == anio.min + 1)
      frec <- length(frec[, 1])
      var <- stats::ts(var_ts, start = c(anio.min, mes), frequency = frec)
      pronostico <- forecast::forecast(var, h = input$ForecastPer)
      print(pronostico[2])
    }
  })
}

shinyApp(ui = ui, server = server)

```
