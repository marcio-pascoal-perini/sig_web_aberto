library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringi)
library(DT)
library(geojsonio)
library(leaflet)
library(ggplot2)

########
## ui ##
########

header <- dashboardHeader(
  title = 'SIG Web Aberto',
  dropdownMenu(type = 'notifications',
               badgeStatus = 'info',
               headerText = '3 novos itens',
               notificationItem(
                 icon = icon('th'),
                 text = tags$span('IPCA15 - Índice Nacional de', tags$br(), 'Preços ao Consumidor Amplo 15'),
                 href = '?item=ibge&sub=ipca151'
               ),
               notificationItem(
                 icon = icon('th'),
                 text = tags$span('SINAPI - Sistema Nacional de', tags$br(), 'Pesquisa de Custos e Índices da', tags$br(), 'Construção Civil'),
                 href = '?item=ibge&sub=sinapi1'
               ),
               notificationItem(
                 icon = icon('th'),
                 text = tags$span('Jundiaí/SP - Organizações', tags$br(), 'da Sociedade Civil'),
                 href = '?item=cb&sub=jundiai1'
               )               
  )
)

sidebar <- dashboardSidebar(
  sidebarMenuOutput('menu')
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$meta(name = 'description', content = 'SIG Web Aberto'),
    tags$meta(name = 'keywords', content = 'sig,sistema,informação,indicadores,geográfica,geotecnologia,cartogramas,mapas,gráficos,R,linguagem R,shiny,leaflet,ibge,pnad,caged,região,metropolitana,são paulo'),
    tags$meta(name = 'author', content = 'OkishiSystems'),
    tags$link(href = 'geral.css', rel = 'stylesheet'),
    tags$script(src = 'geral.js')
  ),
  tabItems(
    tabItem(tabName = 'tabINICIO',
            uiOutput('pageINICIO')
    ),
    tabItem(tabName = 'tabCAGED1',
            uiOutput('pageCAGED1')
    ),
    tabItem(tabName = 'tabPNAD1',
            uiOutput('pagePNAD1')
    ),
    tabItem(tabName = 'tabRMSP1',
            uiOutput('pageRMSP1')
    ),
    tabItem(tabName = 'tabPIMPF1',
            uiOutput('pagePIMPF1')
    ),
    tabItem(tabName = 'tabPMS1',
            uiOutput('pagePMS1')
    ),
    tabItem(tabName = 'tabIPCA1',
            uiOutput('pageIPCA1')
    ),
    tabItem(tabName = 'tabIPCA151',
            uiOutput('pageIPCA151')
    ),    
    tabItem(tabName = 'tabJUNDIAI1',
            uiOutput('pageJUNDIAI1')
    ),
    tabItem(tabName = 'tabSINAPI1',
            uiOutput('pageSINAPI1')
    )    
  )      
)
 
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = 'green'
)

############
## server ##
############

server <- function(input, output, session) {
  options(scipen = 1000, OutDec = ',', warn = -1)
  tryCatch({
    logInfo <- data.frame(
      ip_address = c(session$request[['REMOTE_ADDR']]),
      host_name = c('?'),
      date = c(Sys.time())
    )
    if (file.exists('log.csv')) {
      write.table(logInfo, file = 'log.csv', sep = ';', append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)    
    } else {
      write.table(logInfo, file = 'log.csv', sep = ';', append = FALSE, quote = FALSE, col.names = TRUE, row.names = FALSE)
    }
  })

  output$menu <- renderMenu({
    qs <- getQueryString(session = session)
    if (length(qs) > 0) {
      updateQueryString(queryString = '?item=inicio', session = session, mode = 'replace')
    }      
    i <- ifelse(is.null(qs$item), '', qs$item)
    s <- ifelse(is.null(qs$sub), '', qs$sub)
    sidebarMenu(
      id = 'raiz',
      menuItem(
        text = tags$span('Início / Contato'),
        icon = icon('home'),
        tabName = 'tabINICIO',
        startExpanded = ifelse(stri_cmp_equiv(i, 'inicio', strength = 2), TRUE, FALSE)
      ),
      menuItem(
        text = tags$span('IBGE - Instituto Brasileiro', tags$br(), 'de Geografia e Estatística'),
        icon = icon('th'),
        tabName = 'tabIBGE',
        startExpanded = ifelse(stri_cmp_equiv(i, 'ibge', strength = 2), TRUE, FALSE),
        menuSubItem(
          text = tags$span('IPCA - Divulgação Regional', title = 'Índice Nacional de Preços ao Consumidor Amplo'),
          tabName = 'tabIPCA1',
          selected = ifelse(stri_cmp_equiv(s, 'ipca1', strength = 2), TRUE, FALSE)
        ),
        menuSubItem(
          text = tags$span('IPCA15 - Divulgação Regional', title = 'Índice Nacional de Preços ao Consumidor Amplo 15'),
          tabName = 'tabIPCA151',
          selected = ifelse(stri_cmp_equiv(s, 'ipca151', strength = 2), TRUE, FALSE)
        ),        
        menuSubItem(
          text = tags$span('PIM-PF - Divulgação Regional', title = 'Pesquisa Industrial Mensal - Produção Física'),
          tabName = 'tabPIMPF1',
          selected = ifelse(stri_cmp_equiv(s, 'pimpf1', strength = 2), TRUE, FALSE)
        ),
        menuSubItem(
          text = tags$span('PMS - Divulgação Regional', title = 'Pesquisa Mensal de Serviços'),
          tabName = 'tabPMS1',
          selected = ifelse(stri_cmp_equiv(s, 'pms1', strength = 2), TRUE, FALSE)
        ),        
        menuSubItem(
          text = tags$span('PNAD - Divulgação Regional', tags$br(), 'e Trimestral', title = 'Pesquisa Nacional por Amostra de Domicílios'),
          tabName = 'tabPNAD1',
          selected = ifelse(stri_cmp_equiv(s, 'pnad1', strength = 2), TRUE, FALSE)
        ),
        menuSubItem(
          text = tags$span('SINAPI - Divulgação Regional', title = 'Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil'),
          tabName = 'tabSINAPI1',
          selected = ifelse(stri_cmp_equiv(s, 'sinapi1', strength = 2), TRUE, FALSE)
        )
      ),
      menuItem(
        text = tags$span('ME - Ministério de Economia'),
        icon = icon('th'),
        tabName = 'tabMTPS',
        startExpanded = ifelse(stri_cmp_equiv(i, 'mtps', strength = 2), TRUE, FALSE),
        menuSubItem(
          text = tags$span('CAGED - Informações', tags$br(), 'individuais e setoriais', title = 'Cadastro Geral de Empregados e Desempregados'),
          tabName = 'tabCAGED1',
          selected = ifelse(stri_cmp_equiv(s, 'caged1', strength = 2), TRUE, FALSE)
        )
      ),
      menuItem(
        text = tags$span('RMSP - Região Metropolitana' , tags$br(), 'de São Paulo'),
        icon = icon('th'),
        tabName = 'tabRMSP',
        startExpanded = ifelse(stri_cmp_equiv(i, 'rmsp', strength = 2), TRUE, FALSE),
        menuSubItem(
          text = tags$span('Indicadores'),
          tabName = 'tabRMSP1',
          selected = ifelse(stri_cmp_equiv(s, 'rmsp1', strength = 2) | stri_cmp_equiv(s, 'cioeste1', strength = 2), TRUE, FALSE)
        )
      ),
      menuItem(
        text = tags$span('Cidades do Brasil'),
        icon = icon('th'),
        tabName = 'tabCB',
        startExpanded = ifelse(stri_cmp_equiv(i, 'cb', strength = 2), TRUE, FALSE),
        menuSubItem(
          text = tags$span('Jundiaí/SP - Organizações', tags$br(), 'da Sociedade Civil'),
          tabName = 'tabJUNDIAI1',
          selected = ifelse(stri_cmp_equiv(s, 'jundiai1', strength = 2), TRUE, FALSE)
        )
      )       
    )
  })

  withProgress(
    message = 'Carregando itens...',
    value = 0, {
      source(file = 'INICIO.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)
      source(file = 'CAGED1.R', encoding = 'UTF-8', local = TRUE)  
      incProgress(1 / 10)      
      source(file = 'PNAD1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)      
      source(file = 'RMSP1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)      
      source(file = 'PIMPF1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)      
      source(file = 'PMS1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10) 
      source(file = 'IPCA1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)
      source(file = 'IPCA151.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)      
      source(file = 'JUNDIAI1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)
      source(file = 'SINAPI1.R', encoding = 'UTF-8', local = TRUE)
      incProgress(1 / 10)
    }
  )  
  
  output$pageINICIO <- renderUI({
    INICIO_Admin(session = session)        
    INICIO_Pagina(session = session)    
  })  

  output$pageCAGED1 <- renderUI({
    CAGED1_Admin(session = session)        
    CAGED1_Pagina(session = session)    
  })  
  
  output$pagePNAD1 <- renderUI({
    PNAD1_Admin(session = session)        
    PNAD1_Pagina(session = session)    
  })  
  
  output$pageRMSP1 <- renderUI({
    RMSP1_Admin(session = session)        
    RMSP1_Pagina(session = session)    
  })    

  output$pagePIMPF1 <- renderUI({
    PIMPF1_Admin(session = session)        
    PIMPF1_Pagina(session = session)    
  })      
  
  output$pagePMS1 <- renderUI({
    PMS1_Admin(session = session)        
    PMS1_Pagina(session = session)    
  }) 

  output$pageIPCA1 <- renderUI({
    IPCA1_Admin(session = session)        
    IPCA1_Pagina(session = session)    
  }) 

  output$pageIPCA151 <- renderUI({
    IPCA151_Admin(session = session)        
    IPCA151_Pagina(session = session)    
  })   
  
  output$pageJUNDIAI1 <- renderUI({
    JUNDIAI1_Admin(session = session)        
    JUNDIAI1_Pagina(session = session)    
  })   
  
  output$pageSINAPI1 <- renderUI({
    SINAPI1_Admin(session = session)        
    SINAPI1_Pagina(session = session)    
  })   
  
  #observeEvent(input$raiz, {if (input$raiz == 'tabPIMPF1') {}})
}

#########
##     ##
#########

shinyApp(ui, server)
