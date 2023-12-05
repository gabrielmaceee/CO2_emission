#### Shiny 


emission <-read.csv("../CO2 Emissions_Canada.csv")
marque <- unique(emission[,1])
carburant <-function(carb){
  if(carb == "X") return("Essence classique")
  if(carb == "Z") return("Essence premium")
  if(carb == "D") return("Diesel")
  if(carb == "E") return("Ethanol")
  else return("Gaz naturel")
}
size<-marque
c=0
for(i in marque){
  c=c+1
  size[c]=round(mean(emission[emission[,1]==i,"Engine.Size.L."]),1)
}

conso<-marque
c=0
for(i in marque){
  c=c+1
  conso[c]=round(mean(emission[emission[,1]==i,8]),1)
}

emiss<-marque
c=0
for(i in marque){
  c=c+1
  emiss[c]=round(mean(emission[emission[,1]==i,12]),1)/10
}


library(shiny)



ui <- fluidPage(
  title = 'Create plots in selectize input',
  fluidRow(
    column(
      5,
      plotOutput('parcoord'),
      hr(),
      selectizeInput('une_marque', label='Sélectionner une marque',choices = marque
                     )
    ),
    column(7, DT::dataTableOutput('rawdata'))
  )
)


server <-function(input, output, session) {
  
  mapurl <- session$registerDataObj(
    
    name   = 'CO2',
    data   = data,
    filter = function(data, req) {
      
      marque <- marque
      size <- size 
      conso<-conso
      emiss<-emiss
      
      image <- tempfile()
      tryCatch({
        png(image, width = 400, height = 200, bg = 'transparent')
        par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
      }, finally = dev.off())
      
      # send the PNG image back in a response
      shiny:::httpResponse(
        200, 'image/png', readBin(image, 'raw', file.info(image)[, 'size'])
      )
      
    }
  )
 
  output$parcoord <- renderPlot({
    par(mar = c(2, 4, 2, .1))
    data=matrix(c(size,conso,emiss),ncol=3)
    colnames(data)=c("Taille du Moteur","Consommation","CO2")
    rownames(data)=marque
    CO2 <- data
    plot(c(1, 3), range(data), type = 'n', xaxt = 'n', las = 1,
         xlab = '', ylab = '',ylim=c(0,60))
    matlines(t(data), type = 'l', lty = 1, col = 'gray')
    une_marque <- input$une_marque
    if (une_marque != '') {
      lines(1:3, CO2[une_marque, ], lwd = 2, col = 'red')
      text(2,50,une_marque, cex = 2,col="blue")
    }
    axis(1, 1:3, colnames(CO2))
  })

  
  # show raw data
  output$rawdata <- DT::renderDataTable(DT::datatable(
    cbind(Marque = unique(emission[,1]), "Taille moyenne du moteur (L)"=size, "Consommation moyenne (L/100km)"=conso, "Emission moyenne de CO2 (g/10km)"=emiss),
    options = list(pageLength = 10), rownames = FALSE
  ))
}
  
shinyApp(ui = ui, server = server)