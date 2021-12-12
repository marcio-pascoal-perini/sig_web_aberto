############
## Página ##
############

JUNDIAI1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosJUNDIAI1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Jundiaí/SP - Organizações da Sociedade Civil',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloJUNDIAI1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaJUNDIAI1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaJUNDIAI1', width = '100%'),
    tags$div(id = 'separator2', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

JUNDIAI1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))
  
  ## variáveis e vetores ##
  
  natureza <- 'total'
  naturezas <- c('Associações Privadas' = 'associacoes_privadas',
                 'Desconhecidas' = 'desconhecidas',
                 'Fundações Privadas' = 'fundacoes_privadas',
                 'Organizações Religiosas' = 'organizacoes_religiosas',
                 'Organizacoes Sociais' = 'organizacoes_sociais',
                 'Total' = 'total'
  )
  
  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma', 'Tabela' = 'tabela')
  
  fontes <- 'IPEA'  
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Natureza Juridica:</b> %s; <b>Fontes:</b> %s.',
        names(naturezas[naturezas == natureza]),
        fontes
      )
    )
  }  
  
  camadaBairrosJundiai <- function() {
    temp <- subset(dadosJundiai, codigo != 9999, select = c('codigo', natureza))
    names(temp)[names(temp) == natureza] <- 'valor'
    sp::merge(bairrosJundiai, temp, by.x = 'codigo', by.y = 'codigo')
  }
  
  colunasTabela <- function() {
    c('codigo',
      'bairro',
      'regiao',
      'associacoes_privadas',
      'desconhecidas',
      'fundacoes_privadas',
      'organizacoes_religiosas',
      'organizacoes_sociais',
      'total'
    )
  }  
  
  titulosTabela <- function() {
    c('Código',
      'Bairro',
      'Região',
      'Associações Privadas',
      'Desconhecidas',
      'Fundações Privadas',
      'Organizações Religiosas',
      'Organizacões Sociais',
      'Total'
    )
  }  
  
  dadosTabela <- function() {
    subset(dadosJundiai, select = colunasTabela())
  }  
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s', names(naturezas[naturezas == natureza]))
          camada <- camadaBairrosJundiai()
          if (natureza == 'desconhecidas' | natureza == 'organizacoes_sociais') {
            qpal <- colorBin('Blues', domain = camada$valor, na.color = '#a9a9a9', bins = c(1, 2))  
          } else {
            qpal <- colorBin('Blues', domain = camada$valor, na.color = '#a9a9a9', bins = unique(quantile(camada$valor, names = FALSE, na.rm = TRUE, probs = seq(0, 1, length = 7), type = 1)))
          }
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Bairro: %s</strong><br/><strong>Região: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$bairro,
                             camada$regiao,
                             ifelse(is.na(camada$valor), no = format(x = camada$valor, big.mark = '.'), yes = '')
          ) %>%
            lapply(htmltools::HTML)
          if (inicial) {          
            session$output$cartogramaJUNDIAI1 <- renderLeaflet({
              isolate({
                m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14))
                # m <- addTiles(map = m)                
                m <- addProviderTiles(map = m, provider = 'CartoDB.Positron')
                m <- setView(map = m, lng = -46.91285, lat = -23.1944, zoom = 11)
                m <- addEasyButton(map = m,
                                   button = easyButton
                                   (
                                     icon = 'fa-undo',
                                     title = 'Voltar às coordenadas iniciais',
                                     onClick = JS('function(btn, map){map.setView([-23.1944, -46.91285], 11);}')
                                   )
                )
                m <- addPolygons(map = m,
                                 data = camada,
                                 fillColor = ~qpal(valor),
                                 weight = 2,
                                 opacity = 1,
                                 color = '#FFFFFF',
                                 fillOpacity = 0.7,
                                 label = rotulos,
                                 labelOptions = labelOptions(
                                   style = list('font-weight' = 'normal', padding = '3px 8px'),
                                   textsize = '15px',
                                   direction = 'auto'
                                 )
                )
                m <- addLegend(
                  map = m,
                  pal = qpal,
                  values = camada$valor,
                  opacity = 0.7,
                  title = titulo,
                  position = 'bottomright',
                  labFormat = labelFormat(big.mark = '.', between = ' a '),
                  na.label = 'Indisponível'
                )  
                m
              })
            })
          } else {
            isolate({
              m <- leafletProxy(mapId = 'cartogramaJUNDIAI1', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
              m <- addPolygons(map = m,
                               data = camada,
                               fillColor = ~qpal(valor),
                               weight = 2,
                               opacity = 1,
                               color = '#FFFFFF',
                               fillOpacity = 0.7,
                               label = rotulos,
                               labelOptions = labelOptions(
                                 style = list('font-weight' = 'normal', padding = '3px 8px'),
                                 textsize = '15px',
                                 direction = 'auto'
                               )
              )
              m <- addLegend(
                map = m,
                pal = qpal,
                values = camada$valor,
                opacity = 0.7,
                title = titulo,
                position = 'bottomright',
                labFormat = labelFormat(big.mark = '.', between = ' a '),
                na.label = 'Indisponível'
              )                
            })
          } 
          show(id = 'cartogramaJUNDIAI1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaJUNDIAI1')
          hide(id = 'separator1')
        }
        incProgress(1 / 2)
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaJUNDIAI1 <- DT::renderDataTable({
            isolate({
              DT::datatable(
                data = dadosTabela(),
                colnames = titulosTabela(),
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              ) %>% formatStyle(columns = natureza, backgroundColor = '#e8e8e8')        
            })
          })  
          show(id = 'tabelaJUNDIAI1')
          show(id = 'separator2')      
        } else {
          hide(id = 'tabelaJUNDIAI1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 2)
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonFiltrosJUNDIAI1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectNaturezaJUNDIAI1',
                             label = 'Natureza Juridica:',
                             choices = naturezas,
                             selectize = FALSE,
                             selected = natureza
                 )
          ),
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesJUNDIAI1',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkJUNDIAI1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelJUNDIAI1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkJUNDIAI1, {
    ## Fechar modal ##
    removeModal()        
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    natureza <<- session$input$selectNaturezaJUNDIAI1
    visoes <<- session$input$checkboxVisoesJUNDIAI1
    ## Subtitulo ##
    output$textSubtituloJUNDIAI1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelJUNDIAI1, {
    updateSelectInput(inputId = 'selectNaturezaJUNDIAI1', session = session, selected = natureza)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesJUNDIAI1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaJUNDIAI1_state, {
    tryCatch({
      info <- input$tabelaJUNDIAI1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloJUNDIAI1 <- renderText({
    isolate({
      subTitulo()
    })
  })
  
  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'bairrosJundiai')) {
        bairrosJundiai <<- geojsonio::geojson_read(x = 'dados/geojsons/jundiai-bairros.geojson', what = 'sp')        
      }            
      incProgress(1 / 3)      
      if (!exists(x = 'bairrosJundiaiOk')) {
        for (coluna in colnames(bairrosJundiai@data)) {
          if (is.factor(bairrosJundiai@data[ , coluna])) {
            caractere <- as.character(bairrosJundiai@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            bairrosJundiai@data[ , coluna] <<- as.factor(caractere)
          }
        }
        bairrosJundiaiOk <<- TRUE
      }      
      incProgress(1 / 3)            
      load(file = 'dados/JUNDIAI1/dados-jundiai.Rda')
      incProgress(1 / 3)
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)  
}
