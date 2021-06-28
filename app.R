library(tidyverse)
library(plotly)
library(shiny)
library(rvest)

b2018 <- read.csv("b2018.csv")
b2019 <- read.csv("b2019.csv")
b2020 <- read.csv("b2020.csv") 

scrap <- function(i){
  
  df.final <- NULL
  
  n <- "li"
  
  linhas <- 
    guardar[i] %>%
    html_nodes(n)
  
  for (j in 1:10) {
    
    final <- linhas[j]
    
    # gols
    gols <- final %>%
      html_nodes(".bg-blue") %>%
      html_text() %>%
      strsplit(" x ") %>%
      unlist()
    gols.mandante <- gols[1]
    gols.visitante <- gols[2]
    
    # times
    times <- final %>%
      html_nodes("img") %>%
      html_attr("title")
    mandante <- times[1]
    visitante <- times[2]
    
    # data e local
    n <- ".partida-desc"
    x <- final %>%
      html_nodes(n) %>%
      html_text() %>%
      str_replace_all("[\r\n]" , "")
    x <- gsub("  ","",x)
    x <- x[x != "1 alteração"]
    x <- x[x != "2 alterações"]
    x <- x[x != "3 alterações"]
    x <- x[x != "4 alterações"]
    x <- gsub("Como foi o jogo","",x)
    x <- gsub("1 alteração","",x)
    x <- gsub(".*, \\.*","",x)
    x <- gsub("-Jogo:.*","",x)
    x <- str_trim(x, side = c("right"))
    data <- x[1]
    hora <- word(data,2)
    data <- word(data,1)
    estadio <- x[2]
    estadio <- gsub("- (.+) -","-",estadio)
    
    rodada <- i
    
    if (!is.null(gols.mandante)){
      df <- data.frame(rodada=rodada,
                       mandante=mandante,
                       gols.mandante=gols.mandante,
                       gols.visitante=gols.visitante,
                       visitante=visitante,
                       estadio=estadio,
                       data=data,
                       hora=hora)
      
      df.final <- rbind(df.final,df)
    }
  }
  df.final
}

results <- b2018
results <- rbind(results,b2019)
results <- rbind(results,b2020)

url <- "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/"
ano <- "2021"
site <- read_html(paste0(url,ano))

# separa rodadas
n <- ".swiper-slide"
guardar <- site %>%
  html_nodes(n)

# raspa cada jogo de cada rodada
b2021 <- scrap(1)
for (i in 2:38) {
  if (is.null(scrap(i))){
    break
  }
  else {
    b2021 <- rbind(b2021,scrap(i))
  }
}
results <- rbind(results,b2021)

# limpeza
results$data <- as.Date(results$data,format = "%d/%m/%Y")
results$rodada <- as.numeric(results$rodada)
results$gols.mandante <- as.numeric(results$gols.mandante)
results$gols.visitante <- as.numeric(results$gols.visitante)

results$mandante <- gsub("Atlético - GO","Atlético-GO",results$mandante)
results$visitante <- gsub("Atlético - GO","Atlético-GO",results$visitante)

results$mandante <- gsub("Atlético - MG","Atlético-MG",results$mandante)
results$visitante <- gsub("Atlético - MG","Atlético-MG",results$visitante)

results$mandante <- gsub("Atlético Mineiro","Atlético-MG",results$mandante)
results$visitante <- gsub("Atlético Mineiro","Atlético-MG",results$visitante)

results$mandante <- gsub("America Fc - MG","America-MG",results$mandante)
results$visitante <- gsub("America Fc - MG","America-MG",results$visitante)

results$mandante <- gsub("America - MG","America-MG",results$mandante)
results$visitante <- gsub("America - MG","America-MG",results$visitante)

results$mandante <- gsub("Athletico Paranaense","Athletico-PR",results$mandante)
results$visitante <- gsub("Athletico Paranaense","Athletico-PR",results$visitante)

results$mandante <- gsub("(.+) - .*","\\1",results$mandante)
results$visitante <- gsub("(.+) - .*","\\1",results$visitante)

FILTRO <- function(time) {
  results <-
    results %>%
    filter(visitante==time|mandante==time)
  results
}

lista.times <- results %>% filter(data > "2021-05-01")
lista.times <- sort(unique(lista.times$mandante))
filtrado <- FILTRO("Bahia")

ui <- fluidPage(
  div(style="margin-left:30px;margin-top:30px;",
      h2("Campeonato Brasileiro 2021 - Série A"),
      h4("Comparação do desempenho atual de cada clube com os três anos anteriores")),
  div(style="margin-left:30px;margin-top:30px;",
      selectInput("times",NULL,lista.times,selected ="Bahia",width=180)),
  plotlyOutput("desempenho",height = 520),
  div(style="margin-left:25px;margin-top:25px",
      htmlOutput("creditos"))
)

server <- function(session,input,output) {
  
  output$creditos <- renderText("<p style='color:gray'>Atualização automática via web scraping.<br><b>Fonte:</b> <i>https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a</i></p>")
  
  observeEvent(input$times,{
    
    filtrado <- FILTRO(input$times)
    
    time <- input$times
    
    fig<-plot_ly(
      type = 'scatter',
      mode = 'lines+markers',
      hovertemplate = paste(
        '%{text}',
        '<extra></extra>')) %>%
      layout(hovermode = "closest",
             plot_bgcolor='white',
             paper_bgcolor='white',
             xaxis=list(range = c(0,39), tickformat=',d',title = "jogos\n", dtick = 2,showspikes=TRUE,automargin=TRUE),
             yaxis=list(range = c(-1,91), title = "pontos", dtick = 3,showspikes=TRUE,automargin=TRUE)
      )
    
    for (i in 1:4) {
      if (i == 1){
        INICIO <- "2018-01-01"
        FIM <- "2019-01-01"
      }
      if (i == 2){
        INICIO <- "2019-01-01"
        FIM <- "2020-01-01"
      }
      if (i == 3) {
        INICIO <- "2020-01-01"
        FIM <- "2021-03-01"
      }
      if (i == 4) {
        INICIO <- "2021-05-01"
        FIM <- "2022-01-01"
      }
      
      ano <-
        filtrado %>%
        filter(data>=INICIO & data<=FIM)
      
      if (nrow(ano)>0){
        ano <-
          ano %>%
          mutate(pontos=ifelse(gols.mandante==gols.visitante,1,
                               ifelse((mandante==time &
                                         gols.mandante>gols.visitante)|
                                        (visitante==time &
                                           gols.mandante<gols.visitante),
                                      3,0))) %>%
          arrange(rodada) %>%
          mutate(pontos=cumsum(pontos)) %>%
          mutate(rodada=1:length(rodada))
      
      a <- c(ano$rodada)
      b <- c(ano$pontos)
      
      texto <- paste("<b>",c(as.character(ano$data)),"-",ano$estadio,"</b>\n",
                     ano$mandante," ",ano$gols.mandante,
                     "x",
                     ano$gols.visitante," ",ano$visitante)
      fig <-
        fig %>%
        add_trace(x = a,
                  y = b,
                  text = c(texto),
                  marker = list(color = ifelse(i==1,"#D7DBDD",
                                               ifelse(i==2,"#B2BABB",
                                                      ifelse(i==3,"#85929E",
                                                             "#A93226"))),
                                width = 3),
                  line = list(color = ifelse(i==1,"#D7DBDD",
                                             ifelse(i==2,"#B2BABB",
                                                    ifelse(i==3,"#85929E",
                                                           "#A93226"))),
                              width = 2),
                  name=gsub("\\-.*","",INICIO))
      }
    }
    
    output$desempenho <- renderPlotly(fig)
    
  })
  
    
}

shinyApp(ui,server)