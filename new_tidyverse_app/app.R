library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)
library(shinydashboard)
library(data.table)

shinyjs::useShinyjs()

ui <- dashboardPage(
  skin = "purple", 
  dashboardHeader(title = "基礎tidyr網站"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("上傳你的data", tabName = "上傳你的data"),
      menuItem("select應用", tabName = "select應用"),
      menuItem("filter應用", tabName = "filter應用"),
      menuItem("mutate應用", tabName = "mutate應用"),
      menuItem("group by 加 sumarise的應用",tabName = "groupby加sumarise的應用"),
      menuItem("pivot的應用",tabName = "pivot的應用",
               menuSubItem("pivot_width的應用",))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "上傳你的data",
              box(
                title = "上傳csv檔案", width = 4,
                br(),
                fileInput("data", "請提供你的數據", accept = ".csv")
              ),
              box(title = "介紹",width=8,
                  includeMarkdown("tidyverse介紹.Rmd")),
              box(
                title = "View Your Table", width = 12),
              DT::dataTableOutput("output")
      ),
      tabItem(tabName = "select應用",
              box(title= "這是用select()函數製作的功能",width=4,
                  selectInput("選擇欄位", "選擇欄位", choices = character(0), multiple = TRUE)
              ),
              box(
                title = "View Your Table", width = 8,
                DT::dataTableOutput("select表格"))
      ),
      tabItem(tabName = "filter應用",
              box(title= "這是用filter函數製作的功能",width=4,
                  selectInput("篩選欄位", "篩選欄位", choices = c("無", character(0)), multiple = TRUE),
                  textInput("輸入資料", "輸入資料")
              ),
              box(
                title = "View Your Table", width = 8,
                DT::dataTableOutput("篩選表格"))
      ),
      tabItem(tabName = "mutate應用",
              box(title= "這是用mutate函數製作的功能",width=12,
                  textInput("新增欄位", "新增欄位"),
                  textInput("輸入資料2", "輸入資料"),
                  actionButton("apply_mutate", "應用")
              ),
              box(
                title = "View Your Table", width = 8,
                DT::dataTableOutput("mutate表格"))
      ),
      tabItem(tabName = "groupby加sumarise的應用",
              box(
                selectInput("選擇欄位g", "選擇欄位", choices = character(0), multiple = TRUE),
                actionButton("apply_group", "應用")
              ),
              box(
                title = "View Your Table", width = 12,
                DT::dataTableOutput("group表格"))
      ),
      tabItem(tabName = "pivot的應用",
              box(title= "這是用pivot_wider函數製作的功能",width=6,
                  selectInput("value欄位", "value欄位", choices = character(0)),
                  selectInput("name欄位", "name欄位", choices = character(0)),
                  actionButton("apply_pivot_w","應用pivot_wider"),
                  DT::dataTableOutput("pivot_w表格")
              ),
              box(title= "這是用pivot_longer函數製作的功能",width=6,
                  selectInput("value_l欄位", "value欄位", choices = character(0),multiple = TRUE),
                  actionButton("apply_pivot_l","應用pivot_longer"),
                  DT::dataTableOutput("pivot_l表格")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    fread(input$data$datapath)
  })
  
  output$output <- DT::renderDataTable(
    { data() },
    editable = TRUE
  )
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "選擇欄位", choices = 欄位, selected = 欄位[1])
  })
  
  output$select表格 = renderDT({
    data()%>% select(input$選擇欄位)
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "篩選欄位", choices = c("無", 欄位), selected = "無")
  })
  
  
  output$篩選表格 = renderDT({
    if (input$篩選欄位 != "無" && input$輸入資料 != "") {
      data()%>% 
        filter(.data[[input$篩選欄位]] == input$輸入資料)
    } else {
      data()
    }
  })
  
  
  output$mutate表格 = renderDT({
    if (input$apply_mutate > 0 && !is.null(input$輸入資料2)) {
      data_mutate <- data() %>%
        mutate(!!input$新增欄位:= !!rlang::parse_expr(input$輸入資料2))
    } else {data_mutate <- data() }
    data_mutate
  })
  
  observe({
    欄位g <- names(data())
    updateSelectInput(session, "選擇欄位g", choices = c("無", 欄位g), selected = "無")
  })
  
  output$group表格 = renderDT({
    if (input$apply_group > 0) {
      data_group <- data() %>%
        group_by(across(all_of(input$選擇欄位g)))%>%
        summarise(n = n(), .groups = "drop")
    } else {
      data_group <- data()
    }
    data_group
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "value欄位", choices = 欄位, selected = 欄位[1])
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "name欄位", choices = 欄位, selected = 欄位[2])
  })
  
  output$pivot表格 = renderDT({
    if (input$apply_pivot_w > 0) {
      data_pivot <- data() %>%
        pivot_wider(names_from = input$name欄位,values_from = input$value欄位)
    } 
    if (input$apply_pivot_l >0) {
      data_pivot <- data() %>%
        pivot_longer(c(input$name欄位,input$value欄位),names_to = input$name欄位,values_to = input$value欄位)
    }
    else {
      data_pivot <- data()
    }
    data_pivot
  })
  
}

shinyApp(ui = ui, server = server)
