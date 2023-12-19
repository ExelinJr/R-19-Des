library(ggplot2)
library(shiny)
library(datasets)
#Example Apps
runExample("03_reactivity")

ex.ui = fluidPage(
  titlePanel("Kalkulator Perkalian"),
  sidebarLayout(
    sidebarPanel(
      numericInput("nomorsatu", 'Masukan Angka Pertama', value = 0),
      numericInput("nomordua", "Masukan Angka Kedua", value = 0),
      numericInput("uas", "Masukan Nilai UAS", value = 0),
      actionButton('hitung', 'Button Hitung')
    ),
    mainPanel(
      textOutput("hasil")
    )
  )
)

ex.server = function(input, output){
  observeEvent(input$hitung, { #observeEvent( nama variable button )
    kali = input$nomorsatu * input$nomordua
    output$hasil = renderText({paste("Hasilnya adalah", kali)})
  })
}

shinyApp(ui = ex.ui, server = ex.server)


data('marketing', package = 'datarium')
marketing

#Apps Model
lm.model = lm(sales~youtube+facebook, data=marketing)
lm.model
summary(lm.model)

fbyt.ui = fluidPage(
  titlePanel("Kalkulator Feature"),
  sidebarLayout(
    sidebarPanel(
      numericInput("youtube", 'Masukan Nilai Feature Youtube', value = 0),
      numericInput("facebook", "Masukan Nilai Feature Facebook", value = 0),
      actionButton('lm', 'Linear Regression')
    ),
    mainPanel(
      textOutput("hasil")
    )
  )
)

fbyt.server = function(input, output){
  observeEvent(input$lm, {
    predictions = predict(lm.model, newdata = data.frame(youtube = input$youtube, facebook = input$facebook))
    output$hasil = renderText({paste("Predictionnya adalah", predictions)})
  })
}

shinyApp(ui = fbyt.ui, server = fbyt.server)

#Apps Plot
plot.ui = fluidPage(
  titlePanel("Kalkulator Feature"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data", 'Plih Dataset yang diinginkan', choices = c("iris", "mtcars", "co2")),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


plot.server = function(input, output){
  selectedData = reactive({
    switch(input$data, "iris" = iris, "mtcars" = mtcars, "CO2" = CO2)
  })
  plotdata = reactive({
    data = selectedData()
    if(input$data == "iris"){
      x_col = "Sepal.Length"
      y_col = "Sepal.Width"
    }else if (input$data == 'mtcars'){
      x_col = 'mpg'
      y_col = 'disp'
    }else {
      x_col = "uptake"
      y_col = "conc"
    }
    return(data.frame(x = data[,x_col], y = data[,y_col]))
  })
  output$plot = renderPlot({
    ggplot(plotdata(),aes(x,y)) +
      geom_point()+
      labs(title = paste("Hasil Plot dari dataset ", input$data))
  })
}

shinyApp(ui = plot.ui, server = plot.server)

#Nilai Akhir

akhir.ui = fluidPage(
  titlePanel("Kalkulator Nilai"),
  sidebarLayout(
    sidebarPanel(
      numericInput("tugas", 'Masukan Nilai Tugas', value = 0),
      numericInput("uts", "Masukan Nilai UTS", value = 0),
      numericInput("uas", "Masukan Nilai UAS", value = 0),
      actionButton('hitung', 'Button Hitung')
    ),
    mainPanel(
      textOutput("hasil")
    )
  )
)

akhir.server = function(input, output){
  observeEvent(input$hitung, { #observeEvent( nama variable button )
    kali = input$tugas * 0.3 + input$uts * 0.3 + input$uas * 0.4
    output$hasil = renderText({paste("Hasilnya adalah", kali)})
  })
}

shinyApp(ui = akhir.ui, server = akhir.server)
