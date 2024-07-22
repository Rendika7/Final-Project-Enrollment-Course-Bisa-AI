library(shiny)
library(ggplot2)
library(psych)
library(DT)
library(dplyr)
library(corrplot)

# R.version

shinyServer(
  function(input, output){
    warna <-reactive({
      input$warna
    })
    dataeks <- reactive({
      datainput <- input$dataeksternal
      if(is.null(datainput)){return(NULL)} 
      else{p <- read.csv(datainput$datapath, header = TRUE, sep = input$sep)
      
      # Ubah nilai "?" menjadi NaN di semua kolom
      p <- p %>%
        mutate_all(~ifelse(. == "?", NA, .))
      
      # Menghapus kolom 'normalized.losses'
      p <- subset(p, select = -normalized.losses)
      
      # Menghapus baris yang mengandung nilai NA
      p <- na.omit(p)
      
      # Mengubah kolom "engine-size" menjadi tipe integer
      p$price <- as.integer(p$price)
      p$bore <- as.numeric(p$bore)
      p$`engine.size` <- as.numeric(p$`engine.size`)
      p$`highway.mpg` <- as.numeric(p$`highway.mpg`)
      return (p)
      }
      
    })
    
    
    
    # Filter data based on selections
    output$table2 <- DT::renderDataTable(DT::datatable({
      data <- dataeks()
      if (input$Make != "All") {
        data <- data[data$make == input$Make,]
      }
      if (input$BodyStyle != "All") {
        data <- data[data$`body.style` == input$BodyStyle,]
      }
      if (input$NumOfDoors != "All") {
        data <- data[data$`num.of.doors` == input$NumOfDoors,]
      }
      data
    }))
    
    
    output$table3 <- DT::renderDT({
      
      # Jika tidak ada data, kembalikan NULL
      if (is.null(dataeks())) {
        return(NULL)
      }
      
      ambil_data3 <- describe(dataeks())
      

      
      DT::datatable(ambil_data3, options = list(scrollX = TRUE))
    })
    
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataeksternal, sep = input$sep)
      },
      content = function(file) {
        write.csv(dataeks(), file, row.names = FALSE)
      }
    )
    
    
    
    output$plot1 <- renderPlot({
      data <- dataeks()
      ggplot(data, aes(x = engine.size, y = price)) +
        geom_point(size = 2, color = "blue", alpha = 0.6) +  # Mengatur ukuran, warna, dan transparansi titik
        theme_minimal() +  # Memilih tema plot yang minimalis
        theme(axis.text = element_blank(),
              plot.title = element_text(hjust = 0.5)) +   # Menghilangkan label sumbu x dan y  
        labs(title = "Scatter Plot Engine Size vs Price", x = "Engine Size", y = "Price") # Memberikan judul sumbu dan label
    })
    
    output$plot2 <- renderPlot({
      data <- dataeks()
      ggplot(data, aes(x = bore, y = price)) +
        geom_point(size = 2, color = "green", alpha = 0.6) +  # Mengatur ukuran, warna, dan transparansi titik
        theme_minimal() +  # Memilih tema plot yang minimalis
        theme(axis.text = element_blank(),
              plot.title = element_text(hjust = 0.5)) +   # Menghilangkan label sumbu x dan y  
      labs(title = "Scatter Plot Bore vs Price", x = "Bore", y = "Price")  # Memberikan judul sumbu dan label
    })
    
    output$plot3 <- renderPlot({
      data <- dataeks()
      ggplot(data, aes(x = highway.mpg, y = price)) +
        geom_point(size = 2, color = "red", alpha = 0.6) +  # Mengatur ukuran, warna, dan transparansi titik
        theme_minimal() +  # Memilih tema plot yang minimalis
        theme(axis.text = element_blank(),
              plot.title = element_text(hjust = 0.5)) +   # Menghilangkan label sumbu x dan y  
        labs(title = "Scatter Highway-MPG vs Price", x = "Highway", y = "Price")  # Memberikan judul sumbu dan label
    })
    
    
    
    output$click_info <- renderPrint({
      data <- dataeks()
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
      nearPoints(data, input$plot1_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
      data <- dataeks()
      brushedPoints(data, input$plot1_brush)
    })
    
    
    
    
    
    output$click_info <- renderPrint({
      data <- dataeks()
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
      nearPoints(data, input$plot2_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
      data <- dataeks()
      brushedPoints(data, input$plot2_brush)
    })
    
    
    
    
    
    
    
    output$plot4 <- renderPlot({
      # Jika tidak ada data, kembalikan NULL
      if (is.null(dataeks())) {
        return(NULL)
      }
      
      data <- dataeks()
      
      # Memilih hanya kolom yang dipilih oleh pengguna sebagai variabel independen
      selected_vars <- input$independent_vars
      
      # Menghitung korelasi Pearson antara variabel-variabel tersebut
      correlation_matrix <- cor(data[selected_vars])
      
      # Membuat plot matriks korelasi
      corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 0, mar = c(5, 2, 5, 2))  # Menyesuaikan margin plot
      
      # Menambahkan judul pada plot
      title("Correlation Matrix", line = 0.5)  # Menentukan posisi judul agar lebih baik
    })
    
    

  }
)
