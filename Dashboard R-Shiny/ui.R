library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(readr)
library(dplyr)

df <- read_csv("C:/Users/rendi/ITTS DATA SCIENCE/Semester 6/SIB - BISA AI/Materi/1. Data Science/4. Data Science with R/Enroll Bisa AI Course - Data Science With R/Data Visualization R/data/Automobile_data.csv")
# Ubah nilai "?" menjadi NaN di semua kolom
df <- df %>%
  mutate_all(~ifelse(. == "?", NA, .))

# Menghapus kolom 'normalized.losses'
df_clean <- subset(df, select = -`normalized-losses`)

# Menghapus baris yang mengandung nilai NA
df_clean <- na.omit(df_clean)




fluidPage(
  tags$head(
    tags$style(HTML("
      .padding-around {
        padding: 210px; /* Sesuaikan ukuran padding sesuai kebutuhan */
      }
    "))
  ),
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "AUTO MOBILE DASHBOARD", 
      titleWidth = 300,
      dropdownMenu(
        type = "notifications",
        notificationItem(icon = icon("bell"), status = "success", "Notification 1"),
        notificationItem(icon = icon("bell"), status = "warning", "Notification 2")
      )
    ),
    dashboardSidebar(
      HTML('<center> <img src = "Image_2.jpg" width = "200"> </center>')
    ),
    dashboardBody(
      tabsetPanel(
        tabPanel("Home", icon = icon("home"), 
                 h3("Selamat Datang di Dashboard Automobie Dataset", 
                    style = "color:blue; font-family: 'Poppins', sans-serif; font-weight: bold; text-align: center; text-transform: uppercase; letter-spacing: 0px;"),
                 h5("Dataset ini adalah kumpulan data yang berasal dari Yearbook Otomotif Ward tahun 1985. Sumber data tersebut meliputi Spesifikasi Mobil Impor dan Truk 1985, Yearbook Otomotif Ward 1985. Manual Auto Pribadi, Kantor Layanan Asuransi, 160 Water Street, New York, NY 10038. Laporan Tabrakan Asuransi, Institut Keselamatan Jalan Raya, Watergate 600, Washington, DC 20037.", 
                    style = "color: #333333; font-family: 'Helvetica Neue', sans-serif; font-size: 10px; line-height: 1.5; text-align: center;"),
                 tags$a(href = "https://www.kaggle.com/datasets/toramky/automobile-dataset/data", "Visit The Data Source", style = "color: blue; margin-right: 10px; display: flex; justify-content: center;"),
                 div(
                   style = "display: flex; justify-content: center;",
                   tags$img(src = "dataset-cover.jpg", height = 300)
                 )
        ),
        tabPanel("Input Data", icon = icon("file"), 
                 div(
                   style = "display: flex; flex-direction: column; align-items: center;",
                   br(),
                   fileInput("dataeksternal", "Choose Data From Computer", buttonLabel = "Choose data"),
                   fluidRow(
                     column(8,
                            radioButtons("sep", "Choose Separator", choices = c(comma = ",", semicolon = ";", tab = "\t"), selected = ",", inline = TRUE)),
                     column(4, downloadButton("downloadData", "Download"))
                   ), # Button Download
                   h3("⬇️ Display Data - Automobile Dataset ⬇️"),
                   fluidRow(
                     column(4,
                            selectInput("Make", "Make Type:", c("All", unique(as.character(df_clean$make))))
                     ),
                     column(4,
                            selectInput("BodyStyle", "Body Style:", c("All", unique(as.character(df_clean$`body-style`))))
                     ),
                     column(4,
                            selectInput("NumOfDoors", "Num of Doors:", c("All", unique(as.character(df_clean$`num-of-doors`))))
                     )
                   ),
                   DT::DTOutput("table2"),
                   h3("⬇️ Summary (Descriptive Statistics From Data) ⬇️"),
                   DT::DTOutput("table3")
                 )
        ),
        tabPanel("Dashboard", icon = icon("chart-line"), 
                 # Add dashboard content here
                 br(),
                 fluidRow(
                   column(6, plotOutput("plot1", height = 550, click = "plot1_click", brush = brushOpts(id = "plot1_brush"))),
                   column(6, plotOutput("plot2", height = 550, click = "plot2_click", brush = brushOpts(id = "plot2_brush")))
                 ),
                 br(),
                 fluidRow(
                   column(
                     width = 6,
                     h4("Points near click"),
                     verbatimTextOutput("click_info")
                   ),
                   column(
                     width = 6,
                     h4("Brushed points"),
                     verbatimTextOutput("brush_info")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12, plotOutput("plot3", height = 450, click = "plot3_click", brush = brushOpts(id = "plot3_brush")))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          plotOutput("plot4", height = 450),
                          br(),
                          div(
                            style = "display: flex; justify-content: center;",
                            pickerInput("independent_vars", "Select Independent Variables:",
                                        choices = c("engine.size", "bore", "highway.mpg"),
                                        selected = c("engine.size"),
                                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"),
                                        multiple = TRUE
                            )
                          )
                   )
                 )
        )
      )
    )
  )
)
