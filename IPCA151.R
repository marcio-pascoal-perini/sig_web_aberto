############
## Página ##
############

IPCA151_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosIPCA151',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Índice Nacional de Preços ao Consumidor Amplo 15',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloIPCA151'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaIPCA151', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'grafico1IPCA151', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'grafico2IPCA151', width = '100%', height = '500px'),    
    tags$div(id = 'separator3', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaIPCA151', width = '100%', height = '500px'),
    tags$div(id = 'separator4', style = 'padding: 5px;'),    
    dataTableOutput(outputId = 'tabelaIPCA151', width = '100%'),
    tags$div(id = 'separator5', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

IPCA151_Admin <- function(session) {
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
             'Janeiro 2020' = 202001,
             'Dezembro 2019' = 201912,
             'Novembro 2019' = 201911,
             'Outubro 2019' = 201910,
             'Setembro 2019' = 201909,
             'Agosto 2019' = 201908,
             'Julho 2019' = 201907,
             'Junho 2019' = 201906,
             'Maio 2019' = 201905,
             'Abril 2019' = 201904,
             'Março 2019' = 201903,
             'Fevereiro 2019' = 201902,
             'Janeiro 2019' = 201901
  )
  
  grupo <- 7169
  grupos <- c('Alimentação e Bebidas' = 7170, 
              'Artigos de Residência' = 7486,
              'Comunicação' = 7786,
              'Despesas Pessoais' = 7712,
              'Educação' = 7766,
              'Habitação' = 7445,
              'Saúde e Cuidados Pessoais' = 7660,              
              'Transportes' = 7625,              
              'Vestuário' = 7558,
              'Índice geral' = 7169              
  )
  
  tipo = 355
  tipos = c('Variação Mensal' = 355,
            'Variação Acumulada no Ano' = 356,
            'Peso Mensal' = 357
  )
  
  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma',
              'Gráfico - Regiões Metropolitanas' = 'grafico1',
              'Gráfico - Grupos de Produtos e Serviços' = 'grafico2',
              'Histograma' = 'histograma',
              'Tabela' = 'tabela'
  )
  
  fonte <- 'IBGE - Tabelas 1705 e 7062'
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Grupos de Produtos e Serviços:</b> %s; <b>Tipo de índice:</b> %s; <b>Mês:</b> %s; <b>Fonte:</b> %s.',
        names(grupos[grupos == grupo]),
        names(tipos[tipos == tipo]),
        names(meses[meses == mes]),
        fonte
      )
    )
  }
  
  camadaRMs <- function() {
    temp <- subset(dadosRMs, D2C == mes & D3C == grupo & D4C == tipo, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(rms, temp, by.x = 'codigo', by.y = 'D1C')
  }
  
  dadosGrafico1 <- function() {
    temp <- subset(dadosRMs, D2C == mes & D3C == grupo & D4C == tipo & !is.na(V), select = c('D1N', 'V'))
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$V)])
    return(temp)
  }
  
  dadosGrafico2a <- function() {
    temp <- subset(dadosBrasil, D2C == mes & D3C != 7169 & D4C == tipo & !is.na(V), select = c('D3N', 'V'))
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$V)])
    return(temp)
  }
  
  dadosGrafico2b <- function() {
    temp <- subset(dadosBrasil, D2C == mes & D4C == tipo & !is.na(V), select = c('D3N', 'V'))
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$V)])
    return(temp)
  }
  
  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D3C == grupo & D4C == tipo & !is.na(V), select = c('D2C', 'D2N', 'V'))
    temp$D2N <- factor(temp$D2N, levels = temp$D2N[order(temp$D2C)])
    return(temp)
  }  
  
  titulosTabela <- function() {
    c('Código', 'Região', 'Mês', 'Grupos de Produtos e Serviços', 'Tipo de índice', '%')
  }  
  
  colunasTabela <- function() {
    c('D1C', 'D1N', 'D2N', 'D3N', 'D4N', 'V')
  }  
  
  dadosTabela <- function() {
    subset(dadosRMs, D2C == mes & D3C == grupo & D4C == tipo, select = colunasTabela())
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s<br>%s', names(grupos[grupos == grupo]), names(meses[meses == mes]))
          camada <- camadaRMs()
          if (grupo == 7169 & tipo == 357) {
            qpal <- colorBin('#62ba81', domain = camada$valor, na.color = '#a9a9a9', bins = c(-100, 100))
          } else {
            qpal <- colorBin('Greens', domain = camada$valor, na.color = '#a9a9a9', bins = unique(quantile(camada$valor, names = FALSE, na.rm = TRUE, probs = seq(0, 1, length = 7), type = 1)))            
          }
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Região Metropolitana: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             ifelse(is.na(camada$valor), no = paste(format(x = camada$valor, big.mark = '.'),'%', sep = ''), yes = '')
          ) %>%
            lapply(htmltools::HTML)
          if (inicial) {          
            session$output$cartogramaIPCA151 <- renderLeaflet({
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
                m <- addCircleMarkers(map = m,
                                      data = camada,
                                      fillColor = ~qpal(valor),
                                      weight = 4,
                                      opacity = 1,
                                      color = '#FFFFFF',
                                      fillOpacity = 0.7,
                                      radius = 13,
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
              m <- leafletProxy(mapId = 'cartogramaIPCA151', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
              m <- addCircleMarkers(map = m,
                                    data = camada,
                                    fillColor = ~qpal(valor),
                                    weight = 4,
                                    opacity = 1,
                                    color = '#FFFFFF',
                                    fillOpacity = 0.7,
                                    radius = 13,
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
          show(id = 'cartogramaIPCA151')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaIPCA151')
          hide(id = 'separator1')
        }
        incProgress(1 / 5)
        ## Gráfico 1 ##
        if ('grafico1' %in% visoes) {
          session$output$grafico1IPCA151 <- renderPlot({
            isolate({
              dg <- dadosGrafico1() 
              ggplot(data = dg, aes(x = dg$D1N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#62ba81') +
                labs(title = sprintf('%s - %s', names(grupos[grupos == grupo]), names(meses[meses == mes])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'grafico1IPCA151')
          show(id = 'separator2')      
        } else {
          hide(id = 'grafico1IPCA151')
          hide(id = 'separator2')      
        }
        incProgress(1 / 5)
        ## Gráfico 2 ##
        if ('grafico2' %in% visoes) {
          if (grupo == 7169 & tipo == 357) {
            session$output$grafico2IPCA151 <- renderPlot({
              isolate({
                dg <- dadosGrafico2a()
                pie(x = dg$V,
                    labels = sprintf('%s\n%s%s', dg$D3N, dg$V, '%'),
                    col = c('#ff0000', '#ff8000', '#ffff00', '#40ff00', '#00ffff', '#0000ff', '#bf00ff', '#663300', '#ffcccc'),
                    border = NA,
                    radius = 1,
                    main = sprintf('%s - %s', names(tipos[tipos == tipo]), names(meses[meses == mes]))
                )
              })
            })                                        
          } else {
            session$output$grafico2IPCA151 <- renderPlot({
              isolate({
                dg <- dadosGrafico2b()
                ggplot(data = dg, aes(x = dg$D3N, y = dg$V)) +
                  geom_bar(stat = 'identity', position = 'identity', colour = '#ffffff', fill = ifelse(dg$D3N == names(grupos[grupos == grupo]), '#ff6f00', '#62ba81')) +
                  labs(title = sprintf('%s - %s', names(tipos[tipos == tipo]), names(meses[meses == mes])), x = '', y = '') +
                  geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                        axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                        axis.text.y = element_text(face = 'bold', size = 10)
                  )
              })
            })          
          }
          show(id = 'grafico2IPCA151')
          show(id = 'separator3')      
        } else {
          hide(id = 'grafico2IPCA151')
          hide(id = 'separator3')      
        }
        incProgress(1 / 5)
        ## Histograma ##
        if ('histograma' %in% visoes) {
          session$output$histogramaIPCA151 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D2N, y = dh$V)) +
                geom_line(colour = '#62ba81', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D2C == mes, '#ff0000', '#808080'), size = 4) +
                labs(title = sprintf('%s - %s', names(grupos[grupos == grupo]), names(tipos[tipos == tipo])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaIPCA151')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaIPCA151')
          hide(id = 'separator3')      
        }
        incProgress(1 / 5)
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaIPCA151 <- DT::renderDataTable({
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
          show(id = 'tabelaIPCA151')
          show(id = 'separator5')      
        } else {
          hide(id = 'tabelaIPCA151')
          hide(id = 'separator5')      
        }
        incProgress(1 / 5)
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonFiltrosIPCA151, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectMesIPCA151',
                             label = 'Mês:',
                             choices = meses,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectMesIPCA151), mes, session$input$selectMesIPCA151)
                 )
          ),
          column(width = 6,
                 selectInput(inputId = 'selectGrupoIPCA151',
                             label = 'Grupos de Produtos e Serviços:',
                             choices = grupos,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectGrupoIPCA151), grupo, session$input$selectGrupoIPCA151)
                 )
          )
        ),  
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectTipoIPCA151',
                             label = 'Tipo de Índice:',
                             choices = tipos,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectTipoIPCA151), tipo, session$input$selectTipoIPCA151)
                 )
          )          
        ),
        fluidRow(
          column(width = 12,
                 checkboxGroupInput(inputId = 'checkboxVisoesIPCA151',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Gráfico - Regiões Metropolitanas' = 'grafico1',
                                                'Gráfico - Grupos de Produtos e Serviços' = 'grafico2',
                                                'Histograma' = 'histograma',
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkIPCA151', label = 'Ok'),
          actionButton(inputId = 'buttonCancelIPCA151', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkIPCA151, {
    ## Fechar modal ##
    removeModal()        
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    mes <<- session$input$selectMesIPCA151
    grupo <<- session$input$selectGrupoIPCA151
    tipo <<- session$input$selectTipoIPCA151
    visoes <<- session$input$checkboxVisoesIPCA151
    ## Subtitulo ##
    output$textSubtituloIPCA151 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelIPCA151, {
    updateSelectInput(inputId = 'selectMesIPCA151', session = session, selected = mes)
    updateSelectInput(inputId = 'selectAtividadeIPCA151', session = session, selected = grupo)
    updateSelectInput(inputId = 'selectTipoIPCA151', session = session, selected = tipo)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesIPCA151', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaIPCA151_state, {
    tryCatch({
      info <- input$tabelaIPCA151_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloIPCA151 <- renderText({
    isolate({
      subTitulo()
    })
  })
  
  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'rms')) {
        rms <<- geojsonio::geojson_read(x = 'dados/geojsons/rms.geojson', what = 'sp')        
      }      
      incProgress(1 / 4)      
      if (!exists(x = 'rmsOk')) {
        for (coluna in colnames(rms@data)) {
          if (is.factor(rms@data[ , coluna])) {
            caractere <- as.character(rms@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            rms@data[ , coluna] <<- as.factor(caractere)
          }
        }
        rmsOk <<- TRUE
      }
      incProgress(1 / 4)
      load(file = 'dados/IPCA151/dados-rms.Rda')
      incProgress(1 / 4)
      load(file = 'dados/IPCA151/dados-brasil.Rda')
      incProgress(1 / 4) 
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)  
}
