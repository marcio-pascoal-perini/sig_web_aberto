############
## Página ##
############

PIMPF1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosPIMPF1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Pesquisa Industrial Mensal - Produção Física',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloPIMPF1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaPIMPF1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoPIMPF1', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaPIMPF1', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),    
    dataTableOutput(outputId = 'tabelaPIMPF1', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

PIMPF1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))
  
  ## variáveis e vetores ##
  
  mes <- 202112
  meses <- c('Dezembro 2021' = 202112,
             'Novembro 2021' = 202111,
             'Outubro 2021' = 202110,
             'Setembro 2021' = 202109,
             'Agosto 2021' = 202108,
             'Julho 2021' = 202107,
             'Junho 2021' = 202106,
             'Maio 2021' = 202105,
             'Abril 2021' = 202104,
             'Março 2021' = 202103,
             'Fevereiro 2021' = 202102,
             'Janeiro 2021' = 202101,
             'Dezembro 2020' = 202012,
             'Novembro 2020' = 202011,
             'Outubro 2020' = 202010,
             'Setembro 2020' = 202009,
             'Agosto 2020' = 202008,
             'Julho 2020' = 202007,
             'Junho 2020' = 202006,
             'Maio 2020' = 202005,
             'Abril 2020' = 202004,
             'Março 2020' = 202003,
             'Fevereiro 2020' = 202002,
             'Janeiro 2020' = 202001
  )
  
  atividade <- 129314
  atividades <- c('Indústria Geral' = 129314,
                  'Indústrias Extrativas' = 129315, 
                  'Indústrias de Transformação' = 129316
  )
  
  tipo = 3139
  tipos = c('Variação Percentual Mensal (Base: igual mês do ano anterior)' = 3139,  
            'Variação Percentual Acumulada no Ano (Base: igual período do ano anterior)' = 3140,  
            'Variação percentual Acumulada nos Últimos 12 Meses (Base: últimos 12 meses anteriores)' = 3141
  )
  
  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')
  
  fonte <- 'IBGE - Tabela 3653'
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Seção e Atividade Industrial:</b> %s; <b>Tipo de índice:</b> %s; <b>Mês:</b> %s; <b>Fonte:</b> %s.',
        names(atividades[atividades == atividade]),
        names(tipos[tipos == tipo]),
        names(meses[meses == mes]),
        fonte
      )
    )
  }
  
  camadaEstados <- function() {
    temp <- subset(dadosEstados, D1C == mes & D2C == atividade & D4C == tipo, select = c('D3C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(estados, temp, by.x = 'codigo', by.y = 'D3C')
  }
  
  dadosGrafico <- function() {
    temp <- subset(dadosEstados, D1C == mes & D2C == atividade & D4C == tipo & !is.na(V), select = c('D3N', 'V'))
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$V)])
    return(temp)
  }
  
  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D2C == atividade & D4C == tipo, select = c('D1C', 'D1N', 'V'))
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$D1C)])
    return(temp)
  }
  
  colunasTabela <- function() {
    c('D3C', 'D3N', 'D1N', 'D2N', 'D4N', 'V')
  }  
  
  titulosTabela <- function() {
    c('Código', 'Estado', 'Mês', 'Seção e Atividade Industrial', 'Tipo de índice', '%')
  }  
  
  dadosTabela <- function() {
    subset(dadosEstados, D1C == mes & D2C == atividade & D4C == tipo, select = colunasTabela())
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s<br>%s', names(atividades[atividades == atividade]), names(meses[meses == mes]))
          camada <- camadaEstados()
          caixas = unique(quantile(camada$valor, names = FALSE, na.rm = TRUE, probs = seq(0, 1, length = 7), type = 1))
          qpal <- colorBin('Blues', domain = camada$valor, na.color = '#a9a9a9', bins = caixas)
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             ifelse(is.na(camada$valor), no = paste(format(x = camada$valor, big.mark = '.'),'%', sep = ''), yes = '')
          ) %>%
            lapply(htmltools::HTML)
          if (inicial) {          
            session$output$cartogramaPIMPF1 <- renderLeaflet({
              isolate({
                m <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 10))
                m <- addTiles(map = m)
                m <- setView(map = m, lng = -49.3810, lat = -15.3251, zoom = 3)
                m <- addEasyButton(map = m,
                                   button = easyButton
                                   (
                                     icon = 'fa-undo',
                                     title = 'Voltar às coordenadas iniciais',
                                     onClick = JS('function(btn, map){map.setView([-15.3251, -49.3810], 3);}')
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
                  labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%'),
                  na.label = 'Indisponível'
                )  
                m
              })
            })
          } else {
            isolate({
              m <- leafletProxy(mapId = 'cartogramaPIMPF1', session = session)
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
                labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%'),
                na.label = 'Indisponível'
              )                
            })
          } 
          show(id = 'cartogramaPIMPF1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaPIMPF1')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) {
          session$output$graficoPIMPF1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$D3N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#94c1d8') +
                labs(title = sprintf('%s - %s', names(atividades[atividades == atividade]), names(meses[meses == mes])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoPIMPF1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoPIMPF1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes) {
          session$output$histogramaPIMPF1 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D1N, y = dh$V)) +
                geom_line(colour = '#94c1d8', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D1C == mes, '#ff0000', '#808080'), size = 4) +
                labs(title = sprintf('%s\n%s', names(atividades[atividades == atividade]), names(tipos[tipos == tipo])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaPIMPF1')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaPIMPF1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 4)                
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaPIMPF1 <- DT::renderDataTable({
            isolate({
              DT::datatable(
                data = dadosTabela(),
                colnames = titulosTabela(),
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              )        
            })
          })  
          show(id = 'tabelaPIMPF1')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaPIMPF1')
          hide(id = 'separator4')      
        }
        incProgress(1 / 4)
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonFiltrosPIMPF1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectMesPIMPF1',
                             label = 'Mês:',
                             choices = meses,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectMesPIMPF1), mes, session$input$selectMesPIMPF1)
                 )
          ),
          column(width = 6,
                 selectInput(inputId = 'selectAtividadePIMPF1',
                             label = 'Seções e Atividades Industriais:',
                             choices = atividades,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectAtividadePIMPF1), atividade, session$input$selectAtividadePIMPF1)
                 )
          )
        ),  
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectTipoPIMPF1',
                             label = 'Tipo de Índice:',
                             choices = tipos,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectTipoPIMPF1), tipo, session$input$selectTipoPIMPF1)
                 )
          )          
        ),
        fluidRow(
          column(width = 12,
                 checkboxGroupInput(inputId = 'checkboxVisoesPIMPF1',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Gráfico' = 'grafico',
                                                'Histograma' = 'histograma',                                                
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkPIMPF1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelPIMPF1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkPIMPF1, {
    ## Fechar modal ##
    removeModal()        
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    mes <<- session$input$selectMesPIMPF1
    atividade <<- session$input$selectAtividadePIMPF1
    tipo <<- session$input$selectTipoPIMPF1
    visoes <<- session$input$checkboxVisoesPIMPF1
    ## Subtitulo ##
    output$textSubtituloPIMPF1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelPIMPF1, {
    updateSelectInput(inputId = 'selectMesPIMPF1', session = session, selected = mes)
    updateSelectInput(inputId = 'selectAtividadePIMPF1', session = session, selected = atividade)
    updateSelectInput(inputId = 'selectTipoPIMPF1', session = session, selected = tipo)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesPIMPF1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaPIMPF1_state, {
    tryCatch({
      info <- input$tabelaPIMPF1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloPIMPF1 <- renderText({
    isolate({
      subTitulo()
    })
  })
  
  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'estados')) {
        estados <<- geojsonio::geojson_read(x = 'dados/geojsons/estados.geojson', what = 'sp')        
      }      
      incProgress(1 / 4)      
      if (!exists(x = 'estadosOk')) {
        for (coluna in colnames(estados@data)) {
          if (is.factor(estados@data[ , coluna])) {
            caractere <- as.character(estados@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            estados@data[ , coluna] <<- as.factor(caractere)
          }
        }
        estadosOk <<- TRUE
      }
      incProgress(1 / 4)
      load(file = 'dados/PIMPF1/dados-estados.Rda')
      incProgress(1 / 4)
      load(file = 'dados/PIMPF1/dados-brasil.Rda')
      incProgress(1 / 4)  
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
