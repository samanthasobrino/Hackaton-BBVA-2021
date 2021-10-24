library("shiny")
library("smooth")
library("rAmCharts")
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(readr)
library(dplyr)
predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

#Siempre se usara un fluidPage 
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = img(src="BBVA.png", height=45, width=100)),
    dashboardSidebar(
      sidebarUserPanel(h5("Vinculación a tu medida",style="color: white;font-weight: bold;font-size:3"),
                       subtitle = h6("Bienvenido, ingresa los datos:",style="font-size:1")),
      sidebarMenu(
        # Input: Select a file ----
        fileInput("file1", "Sube tu archivo en CSV",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Horizontal line 
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Nombre de columnas", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separado por",
                     choices = c("Coma" = ",",
                                 "Punto y coma" = ";",
                                 "Espacio" = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Comillas:",
                     choices = c("Ninguna" = "",
                                 "Comillas dobles" = '"',
                                 "Comillas simples" = "'"),
                     selected = '"'),
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Visualización",
                     choices = c("Encabezado" = "Encabezado",
                                 "Todo" = "Todo"),
                     selected = "Encabezado"))),
    
    dashboardBody(tabsetPanel(type="tabs",
                                    # Aqui­ vamos a poner un output, que es lo que visualizaremos
                                    tabPanel("Tabla",tableOutput("contents")),
                                    tabPanel("Análisis Descriptivo",
                                             fluidRow(
                                               column(12,box(
                                                 plotOutput("historico"), solidHeader = FALSE, height="auto", width = "50%")
                                               )
                                             ),
                                             fluidRow(
                                               column(4,box(
                                                 tableOutput("historico2"), solidHeader = FALSE, height="auto", width = "50%")
                                               ),column(4,box(
                                                 tableOutput("historico3"), solidHeader = FALSE, height="auto", width = "50%")
                                               ),column(4,box(
                                                 tableOutput("historico4"), solidHeader = FALSE, height="auto", width = "50%")
                                               )
                                             )),
                                    tabPanel("Recomendaciones",
                                             fluidRow(titlePanel(h3("Clientes con Segmentación comercial 1")),
                                               column(12,box(
                                                 tableOutput("salida"), solidHeader = FALSE, height="auto", width = "auto")
                                               )
                                             ),
                                             fluidRow(titlePanel(h3("Clientes con Segmentación comercial 3")),
                                               column(12,box(
                                                 tableOutput("salida2"), solidHeader = FALSE, height="auto", width = "auto")
                                               )
                                             ),
                                             fluidRow(titlePanel(h3("Clientes con Segmentación comercial 4")),
                                               column(12,box(
                                                 tableOutput("salida3"), solidHeader = FALSE, height="auto", width = "auto")
                                               )
                                             )
                                             )
                              )
      
      )
  )
)

server <- function(input, output, session) {

  output$contents <- renderTable({
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    if(input$disp == "Encabezado") {
      return(head(df))
    }
    else {
      return(df)
    }})
  
  output$historico <- renderPlot({
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_1<-lista[[1]]
    data_2<-lista[[2]]
    data_3<-lista[[3]]
    
    set.seed(15486)
    vec<-c(2,3,8,9,12,16,19)
    (clusters_1 <- kmeans(x = data_1[,vec], centers = 5))
    summary(clusters_1)
    plot_uno<-ggplot(data = data_1, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+ggtitle("Segmento Comercial 1")+
      geom_point(size = 2.5)+
      theme_bw()
    
    
    set.seed(13432)
    vec<-c(2,3,12,13,14,19)
    (clusters_2 <- kmeans(x = data_2[,vec], centers = 4))
    summary(clusters_2)
    plot_dos<-ggplot(data = data_2, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+ggtitle("Segmento Comercial 3")+
      geom_point(size = 2.5)+
      theme_bw()
    
    set.seed(123654)
    vec<-c(2,3,13,14,16)
    (clusters_3 <- kmeans(x = data_3[,vec], centers = 3))
    plot_tres<-ggplot(data = data_3, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+ggtitle("Segmento Comercial 4")+
      geom_point(size = 2.5)+
      theme_bw()
    
    grid.arrange(plot_uno,plot_dos,plot_tres,ncol=3)

})
  
  output$historico2 <- renderTable({
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_1<-lista[[1]]

    uno<-data_1 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))

    return(uno)
    
  })
  
  output$historico3 <- renderTable({
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_2<-lista[[2]]
    
    dos=data_2 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    return(dos)
    
    
  })
  
  output$historico4 <- renderTable({
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_3<-lista[[3]]
    
    tres=data_3 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    return(tres)
    
    
  })
  
  
  output$salida <- renderTable({
    
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_1<-lista[[1]]
    
    set.seed(15486)
    vec<-c(2,3,8,9,12,16,19)
    clusters_1 <- kmeans(x = data_1[,vec], centers = 5)

    uno<-data_1 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    Tabla_1<-data.frame(CLUSTER=c(1,2,3,4,5),PRODUCTOS_RECOMENDADOS=c("TARJETAS,CONSUMO","TARJETAS,CONSUMO","TARJETAS,CONSUMO,AUTOS,HIPOTECARIO",
                                                             "TARJETAS,CONSUMO,AUTOS,HIPOTECARIO","TARJETAS,CONSUMO,AUTOS"))
    
    file=input$file1$datapath
    header=input$header
    sep=input$sep
    quote=input$quote
    
    prueba <- read.csv(file,
                     header =header,
                     sep =sep ,
                     quote =quote)
    
    aux_1<-prueba[which(prueba$SEGMENTO_COMERCIAL_1==1),]
    aux_1<-select(aux_1,"INGRESO_MENSUAL","EDAD","ESTADO_CIVIL_V","ESTADO_CIVIL_D","AFILIACION_BANCA_ONLINE","ANTIGUEDAD","SALDO_MEDIO_CL")
    aux_1$CLUSTER<-predict.kmeans(clusters_1,aux_1)
    aux_1<-left_join(aux_1,uno,by="CLUSTER")
    aux_1<-left_join(aux_1,Tabla_1,by="CLUSTER")
    aux_1<-aux_1[,-(3:4)]

    Final<-aux_1
    Final$INGRESO_MENSUAL<-round(Final$INGRESO_MENSUAL*6173.434+1951.009,2)
    Final$EDAD<-round(Final$EDAD*14.95511+43.57403,0)
    Final$ANTIGUEDAD<-round(Final$ANTIGUEDAD*5.845214+27.85965,0)
    
    return(Final)
    
  })
  
  output$salida2 <- renderTable({
    
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_2<-lista[[2]]

    set.seed(13432)
    vec<-c(2,3,12,13,14,19)
    clusters_2 <- kmeans(x = data_2[,vec], centers = 4)
    
    dos=data_2 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    file=input$file1$datapath
    header=input$header
    sep=input$sep
    quote=input$quote
    
    prueba <- read.csv(file,
                       header =header,
                       sep =sep ,
                       quote =quote)
    
    Tabla_2<-data.frame(CLUSTER=c(1,2,3,4),PRODUCTOS_RECOMENDADOS=c("PR_COMERCIALES,TJ_EMPRESAS,CONSUMO,AUTOS","TJ_EMPRESAS,LEASING","PR_COMERCIALES,TJ_EMPRESAS,CONSUMO",
                                                           "PR_COMERCIALES,CONSUMO"))
    
    aux_2<-prueba[which(prueba$SEGMENTO_COMERCIAL_1==3),]
    aux_2<-select(aux_2,"INGRESO_MENSUAL","EDAD", "AFILIACION_BANCA_ONLINE","GRUPO_RIESGO_1","GRUPO_RIESGO_2", "SALDO_MEDIO_CL")
    aux_2$CLUSTER<-predict.kmeans(clusters_2,aux_2)
    aux_2<-left_join(aux_2,dos,by="CLUSTER")
    aux_2<-left_join(aux_2,Tabla_2,by="CLUSTER")
    aux_2<-aux_2[,-3]
    Final<-aux_2
    Final$INGRESO_MENSUAL<-round(Final$INGRESO_MENSUAL*6173.434+1951.009,2)
    Final$EDAD<-round(Final$EDAD*14.95511+43.57403,0)
    
    return(Final)
    
  })
  
  output$salida3 <- renderTable({
    data <- read_csv("base_super_final.csv")
    data<-data[,-1]
    lista<-split(data,data$SEGMENTO_COMERCIAL_1)
    data_3<-lista[[3]]
    
    set.seed(123654)
    vec<-c(2,3,13,14,16)
    clusters_3 <- kmeans(x = data_3[,vec], centers = 3)
    
    tres=data_3 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    Tabla_3<-data.frame(CLUSTER=c(1,2,3),PRODUCTOS_RECOMENDADOS=c("COMEXT,CARTERA,LEASING,PR_COMERCIALES,TJ_EMPRESAS","CARTERA,LEASING,PR_COMERCIALES,TJ_EMPRESAS",
                                                         "COMEXT,CARTERA,PR_COMERCIALES,TJ_EMPRESAS"))
    
    
    file=input$file1$datapath
    header=input$header
    sep=input$sep
    quote=input$quote
    
    prueba <- read.csv(file,
                       header =header,
                       sep =sep ,
                       quote =quote)
    
    aux_3<-prueba[which(prueba$SEGMENTO_COMERCIAL_1==4),]
    aux_3<-select(aux_3,"INGRESO_MENSUAL","EDAD", "GRUPO_RIESGO_1", "GRUPO_RIESGO_2", "ANTIGUEDAD")
    aux_3$CLUSTER<-predict.kmeans(clusters_3,aux_3)
    aux_3<-left_join(aux_3,tres,by="CLUSTER")
    aux_3<-left_join(aux_3,Tabla_3,by="CLUSTER")
    
    Final<-aux_3
    
    Final$INGRESO_MENSUAL<--1*round(Final$INGRESO_MENSUAL*6173.434+1951.009,2)
    Final$EDAD<-round(Final$EDAD*14.95511+43.57403,0)
    Final$ANTIGUEDAD<-round(Final$ANTIGUEDAD*5.845214+27.85965,0)
    
    return(Final)
    
  })
   
}

shinyApp(ui = ui, server = server)