# 1. Load data and libraries
library(shiny)
library(Hmisc)

# Assuming you've set your working directory and loaded the necessary data
load("uniData.RData")
source("errbar.2.r")

ui <- fluidPage(
  titlePanel("TH-MYCN Model"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("gene", "Enter Mouse Gene Name:", value = "Meis2"),
      actionButton("plotBtn", "Generate Plot"), 
      downloadButton("downloadPlot", "Download Plot as PDF"),
      width = 2
    ),
    
    mainPanel(
      plotOutput("genePlot", width = "600px", height = "400px")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to store the plot data
  plotData <- reactive({
    req(input$gene)
    gene_name <- input$gene
    targets <- uniData[gene_name, ]
    list(targets = targets, gene_name = gene_name)
  })
  
  observeEvent(input$plotBtn, {
    output$genePlot <- renderPlot({
      data <- plotData()
      targets <- data$targets
      gene_name <- data$gene_name
      
      # Calculate parameters for regression analysis
      mice <- SampleAnnotation$genotype
      levels(mice) <- c("WT", "TG")
      time <- as.factor(SampleAnnotation$age)
      time <- as.numeric(as.character(time))
      types <- SampleAnnotation$group
      
      # Generate the plot
      col_group <- c("red", "grey")
      
      exp <- unlist(targets)
      MIN <- min(exp)
      MAX <- max(exp)
      meansmir <- tapply(exp, types, mean)
      sdsmir <- tapply(exp, types, sd)
      
      # Plotting: adjusting for the title and the legend
      par(mar = c(3, 3, 2, 6), bty = "n")
      
      errbar.2(c(1, 2, 6, 1.1, 2.1, 6.1), y = meansmir, yplus = meansmir + sdsmir, yminus = meansmir - sdsmir, 
               col.errbar = rep(c("black", "black"), each=3), pch=20, xaxt="n", xlim=c(0.8,7), yaxt="n",
               ylim=c(MIN,MAX), ylab=NA, xlab=NA, col=rep(c("red", "black"), each=3))
      
      lines(c(1, 2, 6), y=meansmir[1:3], col="red", lwd=2)
      lines(c(1.1, 2.1, 6.1), y=meansmir[4:6], col="black", lwd=2)
      axis(2, at=c(MIN, MAX), tck = -0.04, las=1, labels=formatC(c(MIN, MAX), digits = 2), cex.axis = 1.2)
      axis(1, at=c(1, 2, 6), tck = -0.04, cex.axis = 1.2)
      
      # Adding the dynamic title
      title(main = gene_name, cex.main = 1.5)
      
      # Adding the legend outside the plot
      legend("topright", inset = c(-0.2, 0), legend = c("TH-MYCN", "Wild type"),
             col = c("red", "black"), lwd = 2, xpd = TRUE, bty = "n", cex = 1.5)
    })
  })
  
  # Download handler for saving the plot as a PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$gene, "plot.pdf", sep = "_")
    },
    content = function(file) {
      data <- plotData()
      targets <- data$targets
      gene_name <- data$gene_name
      
      pdf(file = file, width = 8, height = 6)
      
      # Calculate parameters for regression analysis
      mice <- SampleAnnotation$genotype
      levels(mice) <- c("WT", "TG")
      time <- as.factor(SampleAnnotation$age)
      time <- as.numeric(as.character(time))
      types <- SampleAnnotation$group
      
      # Generate the plot
      col_group <- c("red", "grey")
      
      exp <- unlist(targets)
      MIN <- min(exp)
      MAX <- max(exp)
      meansmir <- tapply(exp, types, mean)
      sdsmir <- tapply(exp, types, sd)
      
      # Plotting: adjusting for the title and the legend
      par(mar = c(3, 3, 2, 6), bty = "n")
      
      errbar.2(c(1, 2, 6, 1.1, 2.1, 6.1), y = meansmir, yplus = meansmir + sdsmir, yminus = meansmir - sdsmir, 
               col.errbar = rep(c("black", "black"), each=3), pch=20, xaxt="n", xlim=c(0.8,7), yaxt="n",
               ylim=c(MIN,MAX), ylab=NA, xlab=NA, col=rep(c("red", "black"), each=3))
      
      lines(c(1, 2, 6), y=meansmir[1:3], col="red", lwd=2)
      lines(c(1.1, 2.1, 6.1), y=meansmir[4:6], col="black", lwd=2)
      axis(2, at=c(MIN, MAX), tck = -0.04, las=1, labels=formatC(c(MIN, MAX), digits = 2), cex.axis = 1.2)
      axis(1, at=c(1, 2, 6), tck = -0.04, cex.axis = 1.2)
      
      # Adding the dynamic title
      title(main = gene_name, cex.main = 1.5)
      
      # Adding the legend outside the plot
      legend("topright", inset = c(-0.2, 0), legend = c("TH-MYCN", "Wild type"),
             col = c("red", "black"), lwd = 2, xpd = TRUE, bty = "n", cex = 1.5)
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
