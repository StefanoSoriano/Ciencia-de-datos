## Predicción automática de series de tiempo (modelos ARIMA y SARIMA)

En este repositorio se encuentra una aplicación (en desarrollo) en lenguaje R que se ejecuta en R Shiny, para forecasting de series de tiempo en modo automático, modelos ARIMA y SARIMA.

#### Captura de pantalla de la aplicación 

<img src="https://github.com/StefanoSoriano/Mi-primera-web-app-en-R-Shiny/blob/master/Time%20Series%20Forecasting.png?raw=true" alt="drawing"/>
###### Fuente: Elaboración propia en RStudio.

### Script en código HTML y R que se ejecuta en [R Shiny](https://shiny.rstudio.com/)
```r

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
    sliderInput("PerAdelante",
                "Número de períodos a pronosticar",
                min = 1,
                max = 25,
                value = 5,
              )
               ),

    mainPanel(
      h2(".::::::Time Series Forecasting::::::."),
      h1(""),
      h4("Gráfica de la serie en niveles:"),
      plotOutput("nivelesPerAdelante"),
      h4("Estadístico Ljung-Box:"),
      plotOutput("plotPerAdelante"),
      h4("Diferencias de la serie:"),
      plotOutput("diferPerAdelante"),
      h4("Pronóstico:"),
      plotOutput("pronPerAdelante"),
      h4("Valores del pronóstico:"),
      tableOutput("values_pronPerAdelante"),
      tableOutput("values1_pronPerAdelante"),

      plotOutput("niveles_y"),
      plotOutput("difer_y"),
      plotOutput("tsdiag_y"),
      plotOutput("pron_y"),
      tableOutput("values_pron_y"),
      tableOutput("values1_pron_y"),


      plotOutput("niveles_z"),
      plotOutput("difer_z"),
      plotOutput("tsdiag_z"),
      plotOutput("pron_z"),
      tableOutput("values_pron_z"),
      tableOutput("values1_pron_z")
    )
  )
)


server <- function(input, output) {

  output$nivelesPerAdelante <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      varPerAdelante <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      varPerAdelante_ts <- varPerAdelante[, 2]
      varPerAdelante <- varPerAdelante[, 1]
      anios <- cbind(as.numeric(substr(varPerAdelante, 1, 4)))
      anho.minPerAdelante <- min(anios)
      anho.maxPerAdelante <- max(anios)
      mesPerAdelante <- cbind(as.numeric(gsub("(.*)/", "", varPerAdelante)))
      mesPerAdelante <- mesPerAdelante[1,]
      frecPerAdelante <- dplyr::filter(as.data.frame(anios), anios == anho.minPerAdelante + 1)
      frecPerAdelante <- length(frecPerAdelante[, 1])
      varPerAdelante <- stats::ts(varPerAdelante_ts, start = c(anho.minPerAdelante, mesPerAdelante), frequency = frecPerAdelante)
      edoPerAdelante <- input$caption
      actPerAdelante <- input$caption1
      plot(varPerAdelante, main = paste("Actividad ", actPerAdelante, " de ", edoPerAdelante, " de ", anho.minPerAdelante, " a ", anho.maxPerAdelante, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', actPerAdelante, sep = ""), xlab = 'Año')
    }
  })


  output$plotPerAdelante <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      varPerAdelante <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      varPerAdelante_ts <- varPerAdelante[, 2]
      varPerAdelante <- varPerAdelante[, 1]
      anios <- cbind(as.numeric(substr(varPerAdelante, 1, 4)))
      anho.minPerAdelante <- min(anios)
      anho.maxPerAdelante <- max(anios)
      mesPerAdelante <- cbind(as.numeric(gsub("(.*)/", "", varPerAdelante)))
      mesPerAdelante <- mesPerAdelante[1,]
      frecPerAdelante <- dplyr::filter(as.data.frame(anios), anios == anho.minPerAdelante + 1)
      frecPerAdelante <- length(frecPerAdelante[, 1])
      varPerAdelante <- stats::ts(varPerAdelante_ts, start = c(anho.minPerAdelante, mesPerAdelante), frequency = frecPerAdelante)
      modeloPerAdelante <- forecast::auto.arima(varPerAdelante)
      tsdiag(modeloPerAdelante)
    }
  })

  output$diferPerAdelante <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      varPerAdelante <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      varPerAdelante_ts <- varPerAdelante[, 2]
      varPerAdelante <- varPerAdelante[, 1]
      anios <- cbind(as.numeric(substr(varPerAdelante, 1, 4)))
      anho.minPerAdelante <- min(anios)
      anho.maxPerAdelante <- max(anios)
      mesPerAdelante <- cbind(as.numeric(gsub("(.*)/", "", varPerAdelante)))
      mesPerAdelante <- mesPerAdelante[1,]
      frecPerAdelante <- dplyr::filter(as.data.frame(anios), anios == anho.minPerAdelante + 1)
      frecPerAdelante <- length(frecPerAdelante[, 1])
      varPerAdelante <- stats::ts(varPerAdelante_ts, start = c(anho.minPerAdelante, mesPerAdelante), frequency = frecPerAdelante)

      n_diffPerAdelante <- forecast::ndiffs(varPerAdelante, test = c("adf"))
      var_diffPerAdelante <- diff(varPerAdelante, n_diffPerAdelante)
      edoPerAdelante <- input$caption
      actPerAdelante <- input$caption1
      plot(var_diffPerAdelante, main = paste("Diferencias de la actividad ", actPerAdelante, " de ", edoPerAdelante, " de ", anho.minPerAdelante, " a ", anho.maxPerAdelante, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', actPerAdelante, sep = ""), xlab = 'Año')
      abline(h = mean(var_diffPerAdelante), col = 'blue')
    }
  })

  output$pronPerAdelante <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      varPerAdelante <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      varPerAdelante_ts <- varPerAdelante[, 2]
      varPerAdelante <- varPerAdelante[, 1]
      anios <- cbind(as.numeric(substr(varPerAdelante, 1, 4)))
      anho.minPerAdelante <- min(anios)
      anho.maxPerAdelante <- max(anios)
      mesPerAdelante <- cbind(as.numeric(gsub("(.*)/", "", varPerAdelante)))
      mesPerAdelante <- mesPerAdelante[1,]
      frecPerAdelante <- dplyr::filter(as.data.frame(anios), anios == anho.minPerAdelante + 1)
      frecPerAdelante <- length(frecPerAdelante[, 1])
      varPerAdelante <- stats::ts(varPerAdelante_ts, start = c(anho.minPerAdelante, mesPerAdelante), frequency = frecPerAdelante)
      pronosticoPerAdelante <- forecast::forecast(varPerAdelante, h = input$PerAdelante)
      edoPerAdelante <- input$caption
      actPerAdelante <- input$caption1
      plot(pronosticoPerAdelante, main = paste("Pronóstico de la actividad ", actPerAdelante, " de ", edoPerAdelante, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', actPerAdelante, sep = ""), xlab = 'Año')
    }
  })
  output$values_pronPerAdelante <- renderTable({
    if (length(input$csvs$datapath[1]) == 0) {

    } else {
      varPerAdelante <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
      varPerAdelante_ts <- varPerAdelante[, 2]
      varPerAdelante <- varPerAdelante[, 1]
      anios <- cbind(as.numeric(substr(varPerAdelante, 1, 4)))
      anho.minPerAdelante <- min(anios)
      anho.maxPerAdelante <- max(anios)
      mesPerAdelante <- cbind(as.numeric(gsub("(.*)/", "", varPerAdelante)))
      mesPerAdelante <- mesPerAdelante[1,]
      frecPerAdelante <- dplyr::filter(as.data.frame(anios), anios == anho.minPerAdelante + 1)
      frecPerAdelante <- length(frecPerAdelante[, 1])
      varPerAdelante <- stats::ts(varPerAdelante_ts, start = c(anho.minPerAdelante, mesPerAdelante), frequency = frecPerAdelante)
      pronosticoPerAdelante <- forecast::forecast(varPerAdelante, h = input$PerAdelante)
      print(pronosticoPerAdelante[2])
    }
  })
}

shinyApp(ui = ui, server = server)

```

