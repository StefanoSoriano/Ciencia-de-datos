## Predicción automática de series de tiempo (modelos ARIMA y SARIMA)

En este repositorio se encuentra una aplicación (en desarrollo) en lenguaje R que se ejecuta en R Shiny, para forecasting de series de tiempo en modo automático, modelos ARIMA y SARIMA.

### Script en lenguaje HTML y R que se ejecuta en [R Shiny](https://shiny.rstudio.com/)

```r
ui <- fluidPage(
   titlePanel(" "),
   sidebarLayout(
      sidebarPanel(
        fileInput("csvs",
                   label = "Subir variables en formato .csv",
                   multiple = TRUE),
                      h4("UAM Azcapotzalco"),
                      helpText("Desarrollado en R Shiny. Por Jorge Stephano Soriano Urbán"),
                       textInput("caption", "Estado I:", ""),
                          textInput("caption1", "Actividad I:", ""),
                            textInput("caption_", "Estado II:", ""),
                              textInput("caption2", "Actividad II:", ""),
                                 textInput("caption__", "Estado III:", ""),
                                    textInput("caption3", "Actividad III:", ""),
                            sliderInput("perAdelante", "Número de periodos hacia adelante:",
                                      min = 1, max = 15,
                                                  value = 5)
),
      
      mainPanel(
         h2("Forecasting"),
         h5("..."),
         plotOutput("niveles_x"),
         plotOutput("difer_x"),
         plotOutput("tsdiag_x"),
         plotOutput("pron_x"),
         tableOutput("values_pron_x"),
         tableOutput("values1_pron_x"),
         
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

    output$niveles_x <- renderPlot({
    var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
    var_x_ts <- var_x[, 2]
    var_x <- var_x[, 1]
    anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
    anho.min_x <- min(anhos)
    anho.max_x <- max(anhos)
    mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
    mes_x <- mes_x[1,]
    frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
    frec_x <- length(frec_x[,1])
    var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
    edo_x <- input$caption
    act_x <- input$caption1
    plot(var_x, main = paste("Actividad ",act_x, " de ", edo_x," de ", anho.min_x, " a ", anho.max_x,".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_x,sep =""), xlab = 'Año')

})

    output$plot_x <- renderPlot({
 var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
 var_x_ts <- var_x[, 2]
 var_x <- var_x[, 1]
 anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
 anho.min_x <- min(anhos)
 anho.max_x <- max(anhos)
 mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
 mes_x <- mes_x[1,]
 frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
 frec_x <- length(frec_x[, 1])
 var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
modelo_x <- forecast::auto.arima(var_x)

tsdiag(modelo_x)

})


    output$difer_x <- renderPlot({
var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
var_x_ts <- var_x[, 2]
var_x <- var_x[, 1]
anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
anho.min_x <- min(anhos)
anho.max_x <- max(anhos)
mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
mes_x <- mes_x[1,]
frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
frec_x <- length(frec_x[, 1])
var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)

        n_diff_x <- forecast::ndiffs(var_x, test = c("adf"))
        var_diff_x <- diff(var_x, n_diff_x)
edo_x <- input$caption
act_x <- input$caption1
        plot(var_diff_x, main = paste("Primeras difrenecias de la actividad ", act_x, " de ", edo_x, " de ", anho.min_x, " a ", anho.max_x, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_x, sep = ""), xlab = 'Año')
        abline(h = mean(var_diff_x), col = 'blue')

})

    output$tsdiag_x <- renderPlot({
var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
var_x_ts <- var_x[, 2]
var_x <- var_x[, 1]
anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
anho.min_x <- min(anhos)
anho.max_x <- max(anhos)
mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
mes_x <- mes_x[1,]
frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
frec_x <- length(frec_x[, 1])
var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
mod_x <- forecast::auto.arima(var_x)
tsdiag(mod_x)


        })

        output$pron_x <- renderPlot({
    var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
    var_x_ts <- var_x[, 2]
    var_x <- var_x[, 1]
    anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
    anho.min_x <- min(anhos)
    anho.max_x <- max(anhos)
    mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
    mes_x <- mes_x[1,]
    frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
    frec_x <- length(frec_x[, 1])
    var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
adelante <- as.data.frame(sliderValues())
adelante <- as.numeric(adelante[2])
    pronostico_x <- forecast::forecast(var_x, h = adelante)
edo_x <- input$caption
act_x <- input$caption1
plot(pronostico_x, main = paste("Pronóstico de la actividad ", act_x, " de ", edo_x,".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_x, sep = ""), xlab = 'Año')


        })
    
    output$values_pron_x <- renderTable ({
    var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
    var_x_ts <- var_x[, 2]
    var_x <- var_x[, 1]
    anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
    anho.min_x <- min(anhos)
    anho.max_x <- max(anhos)
    mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
    mes_x <- mes_x[1,]
    frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
    frec_x <- length(frec_x[, 1])
    var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
adelante <- as.data.frame(sliderValues())
adelante <- as.numeric(adelante[2])
    pronostico_x <- forecast::forecast(var_x, h = adelante)
    print(paste("Valores del pronóstico, ", adelante ," periodos hacia adelante.", sep = ""))
    
})

        output$values1_pron_x <- renderTable({
        var_x <- read.csv(input$csvs$datapath[1], header = T, sep = ",")
        var_x_ts <- var_x[, 2]
        var_x <- var_x[, 1]
        anhos <- cbind(as.numeric(substr(var_x, 1, 4)))
        anho.min_x <- min(anhos)
        anho.max_x <- max(anhos)
        mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x)))
        mes_x <- mes_x[1,]
        frec_x <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_x + 1)
        frec_x <- length(frec_x[, 1])
        var_x <- stats::ts(var_x_ts, start = c(anho.min_x, mes_x), frequency = frec_x)
adelante <- as.data.frame(sliderValues())
adelante <- as.numeric(adelante[2])
        pronostico_x <- forecast::forecast(var_x, h = adelante)
        
        (values.predict <- pronostico_x$mean)
        

        })

    ###### variable y

    output$niveles_y <- renderPlot({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    edo_y <- input$caption_
    act_y <- input$caption2
    plot(var_y, main = paste("Actividad ", act_y, " de ", edo_y, " de ", anho.min_y, " a ", anho.max_y, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_y, sep = ""), xlab = 'Año')

    })

    output$plot_y <- renderPlot({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    modelo_y <- forecast::auto.arima(var_y)

        tsdiag(modelo_y)

    })


    output$difer_y <- renderPlot({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)

        n_diff_y <- forecast::ndiffs(var_y, test = c("adf"))
    var_diff_y <- diff(var_y, n_diff_y)
    edo_y <- input$caption_
    act_y <- input$caption_2
    plot(var_diff_y, main = paste("Primeras difrenecias de la actividad ", act_y, " de ", edo_y, " de ", anho.min_y, " a ", anho.max_y, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_y, sep = ""), xlab = 'Año')
    abline(h = mean(var_diff_y), col = 'blue')

    })

    output$tsdiag_y <- renderPlot({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    mod_y <- forecast::auto.arima(var_y)
    tsdiag(mod_y)


    })

    output$pron_y <- renderPlot({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_y <- forecast::forecast(var_y, h = adelante)
    edo_y <- input$caption_
    act_y <- input$caption2
    plot(pronostico_y, main = paste("Pronóstico de la actividad ", act_y, " de ", edo_y, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_y, sep = ""), xlab = 'Año')


    })

    output$values_pron_y <- renderTable({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_y <- forecast::forecast(var_y, h = adelante)
    print(paste("Valores del pronóstico, ", adelante, " periodos hacia adelante.", sep = ""))

    })

    output$values1_pron_y <- renderTable({
    var_y <- read.csv(input$csvs$datapath[2], header = T, sep = ",")
    var_y_ts <- var_y[, 2]
    var_y <- var_y[, 1]
    anhos <- cbind(as.numeric(substr(var_y, 1, 4)))
    anho.min_y <- min(anhos)
    anho.max_y <- max(anhos)
    mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y)))
    mes_y <- mes_y[1,]
    frec_y <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_y + 1)
    frec_y <- length(frec_y[, 1])
    var_y <- stats::ts(var_y_ts, start = c(anho.min_y, mes_y), frequency = frec_y)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_y <- forecast::forecast(var_y, h = adelante)

        (values.predict <- pronostico_y$mean)


 
 })

    ###### variable z

    output$niveles_z <- renderPlot({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    edo_z <- input$caption__
    act_z <- input$caption3
    plot(var_z, main = paste("Actividad ", act_z, " de ", edo_z, " de ", anho.min_z, " a ", anho.max_z, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_z, sep = ""), xlab = 'Año')

    })

    output$plot_z <- renderPlot({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    modelo_z <- forecast::auto.arima(var_z)

        tsdiag(modelo_z)

    })


    output$difer_z <- renderPlot({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)

        n_diff_z <- forecast::ndiffs(var_z, test = c("adf"))
    var_diff_z <- diff(var_z, n_diff_z)
    edo_z <- input$caption__
    act_z <- input$caption__2
    plot(var_diff_z, main = paste("Primeras difrenecias de la actividad ", act_z, " de ", edo_z, " de ", anho.min_z, " a ", anho.max_z, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_z, sep = ""), xlab = 'Año')
    abline(h = mean(var_diff_z), col = 'blue')

    })

    output$tsdiag_z <- renderPlot({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    mod_z <- forecast::auto.arima(var_z)
    tsdiag(mod_z)


    })

    output$pron_z <- renderPlot({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_z <- forecast::forecast(var_z, h = adelante)
    edo_z <- input$caption__
    act_z <- input$caption3
    plot(pronostico_z, main = paste("Pronóstico de la actividad ", act_z, " de ", edo_z, ".", sep = ""), las = 1, sub = 'Fuente: INEGI', ylab = paste('Actividad ', act_z, sep = ""), xlab = 'Año')


    })


    output$values_pron_z <- renderTable({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_z <- forecast::forecast(var_z, h = adelante)
    print(paste("Valores del pronóstico, ", adelante, " periodos hacia adelante.", sep = ""))

    })

    output$values1_pron_z <- renderTable({
    var_z <- read.csv(input$csvs$datapath[3], header = T, sep = ",")
    var_z_ts <- var_z[, 2]
    var_z <- var_z[, 1]
    anhos <- cbind(as.numeric(substr(var_z, 1, 4)))
    anho.min_z <- min(anhos)
    anho.max_z <- max(anhos)
    mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z)))
    mes_z <- mes_z[1,]
    frec_z <- dplyr::filter(as.data.frame(anhos), anhos == anho.min_z + 1)
    frec_z <- length(frec_z[, 1])
    var_z <- stats::ts(var_z_ts, start = c(anho.min_z, mes_z), frequency = frec_z)
    adelante <- as.data.frame(sliderValues())
    adelante <- as.numeric(adelante[2])
    pronostico_z <- forecast::forecast(var_z, h = adelante)

        (values.predict <- pronostico_z$mean)

})


    output$var_names_x <- renderTable({

    
    print(input$csvs$name[1])
    

    })

output$var_names_y <- renderTable({

    print(input$csvs$name[2])
   

})

    output$var_names_z <- renderTable({

        print(input$csvs$name[3])


    })

    
   
   
    sliderValues <- reactive({

        data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$perAdelante)),
      stringsAsFactors = FALSE)

    })
}

shinyApp(ui = ui, server = server)
```

