############
## Página ##
############

CAGED1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosCAGED1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Cadastro Geral de Empregados e Desempregados',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloCAGED1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaCAGED1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoCAGED1', width = '100%', height = '500px'),
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaCAGED1', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaCAGED1', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

CAGED1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))

  ## variáveis e vetores ##

  abrangencia <- 'estados'
  abrangencias <- c('Estados' = 'estados', 'Capitais' = 'capitais')
  
  mmm <- '2019/12 Dezembro'
  meses <- c('2019/12 Dezembro' = '2019/12 Dezembro',
             '2019/11 Novembro' = '2019/11 Novembro',
             '2019/10 Outubro' = '2019/10 Outubro',
             '2019/09 Setembro' = '2019/09 Setembro',
             '2019/08 Agosto' = '2019/08 Agosto',
             '2019/07 Julho' = '2019/07 Julho',
             '2019/06 Junho' = '2019/06 Junho',
             '2019/05 Maio' = '2019/05 Maio',
             '2019/04 Abril' = '2019/04 Abril',
             '2019/03 Março' = '2019/03 Março',
             '2019/02 Fevereiro' = '2019/02 Fevereiro',
             '2019/01 Janeiro' = '2019/01 Janeiro',
             '2018/12 Dezembro' = '2018/12 Dezembro',
             '2018/11 Novembro' = '2018/11 Novembro',
             '2018/10 Outubro' = '2018/10 Outubro',
             '2018/09 Setembro' = '2018/09 Setembro',
             '2018/08 Agosto' = '2018/08 Agosto',
             '2018/07 Julho' = '2018/07 Julho',             
             '2018/06 Junho' = '2018/06 Junho',
             '2018/05 Maio' = '2018/05 Maio',
             '2018/04 Abril' = '2018/04 Abril',
             '2018/03 Março' = '2018/03 Março',
             '2018/02 Fevereiro' = '2018/02 Fevereiro',
             '2018/01 Janeiro' = '2018/01 Janeiro'
             #'2017/12 Dezembro' = '2017/12 Dezembro',
             #'2017/11 Novembro' = '2017/11 Novembro',
             #'2017/10 Outubro' = '2017/10 Outubro',
             #'2017/09 Setembro' = '2017/09 Setembro',
             #'2017/08 Agosto' = '2017/08 Agosto',
             #'2017/07 Julho' = '2017/07 Julho',
             #'2017/06 Junho' = '2017/06 Junho'
  )
  
  informacao <- 'variacoes'
  informacoes <- c('Admitidos' = 'admitidos', 'Desligados' = 'desligados', 'Variação absoluta' = 'variacoes')
  
  variavel <- 'total'
  variaveis <- c('Faixa etária' = 'faixa_etaria',
                 'Grau de instrução' = 'grau_de_instrucao',
                 'Setor' = 'setor',
                 'Sexo' = 'sexo',
                 'Total' = 'total'
  )
  
  faixa <- 'ate_17'
  faixas <- c('Até 17 anos' = 'ate_17',
              'De 18 a 24 anos' = 'de_18_a_24',
              'De 25 a 29 anos' = 'de_25_a_29',
              'De 30 a 39 anos' = 'de_30_a_39',
              'De 40 a 49 anos' = 'de_40_a_49',
              'De 50 a 64 anos' = 'de_50_a_64',
              '65 anos ou mais' = 'de_65_ou_mais'
  )
  
  grau <- 'analfabeto'  
  graus <- c('Analfabeto' = 'analfabeto',
             'Até a 5ª incompleto' = 'ate_5a_incompleto',
             '5ª completo fundamental' = 'de_5a_completo_fundamental',
             '6ª a 9ª fundamental' = 'de_6a_a_9a_fundamental',
             'Fundamental completo' = 'fundamental_completo',
             'Médio incompleto' = 'medio_incompleto',
             'Médio completo' = 'medio_completo',
             'Superior incompleto' = 'superior_incompleto',
             'Superior completo' = 'superior_completo'
  )
  
  setor <- 'agropecuaria'   
  setores <- c('Agropecuária' = 'agropecuaria',
               'Industria' = 'industria',
               'Comércio' = 'comercio',
               'Construção civil' = 'construcao_civil',
               'Serviços' = 'servicos'
  )
  
  sexo <- 'feminino'
  sexos <- c('Feminino' = 'feminino', 'Masculino' = 'masculino')
  
  total <- 'total'
  totais <- c('Total' = 'total')  
  
  categoria <- total
  categorias <- totais
  
  registrosPorPagina <- 10
  
  fonte <- 'MTPS/SPPE/DES/CGET - CAGED LEI 4.923/65'
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')
  
  ## funções ##

  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Abrangência:</b> %s; <b>Mês:</b> %s; <b>Informação:</b> %s; <b>Variável:</b> %s; <b>Fonte:</b> %s.',
        names(abrangencias[abrangencias == abrangencia]),
        names(meses[meses == mmm]),
        names(informacoes[informacoes == informacao]),
        names(variaveis[variaveis == variavel]),
        fonte
      )
    )
  }

  camadaEstados <- function() {
    switch(informacao,
           'admitidos' = {
             temp <- subset(admitidosEstados, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(estados, temp, by.x = 'codigo', by.y = 'codigo')
           },
           'desligados' = {
             temp <- subset(desligadosEstados, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(estados, temp, by.x = 'codigo', by.y = 'codigo')
           },
           'variacoes' = {
             temp <- subset(variacoesEstados, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(estados, temp, by.x = 'codigo', by.y = 'codigo')
           }
    )          
  }
  
  camadaCapitais <- function() {
    switch(informacao,
           'admitidos' = {
             temp <- subset(admitidosCapitais, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(capitais, temp, by.x = 'codigo', by.y = 'codigo')
           },
           'desligados' = {
             temp <- subset(desligadosCapitais, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(capitais, temp, by.x = 'codigo', by.y = 'codigo')
           },
           'variacoes' = {
             temp <- subset(variacoesCapitais, mes == mmm, select = c('codigo', categoria))
             names(temp)[names(temp) == categoria] <- 'valor'
             sp::merge(capitais, temp, by.x = 'codigo', by.y = 'codigo')
           }
    )      
  }
  
  dadosGrafico <- function() {
    switch(abrangencia,
           'estados' = {
             novoCodigo <- 99
             novoNome <- 'Outros'
             temp1 <- switch(informacao,
                             'admitidos' = subset(admitidosEstados, mes == mmm, select = c('codigo', 'nome', categoria)),
                             'desligados' = subset(desligadosEstados, mes == mmm, select = c('codigo', 'nome', categoria)),
                             'variacoes' = subset(variacoesEstados, mes == mmm, select = c('codigo', 'nome', categoria))
             )
           },
           'capitais' = {
             novoCodigo <- 9999999
             novoNome <- 'Outras'
             temp1 <- switch(informacao,
                             'admitidos' = subset(admitidosCapitais, mes == mmm, select = c('codigo', 'nome', categoria)),
                             'desligados' = subset(desligadosCapitais, mes == mmm, select = c('codigo', 'nome', categoria)),
                             'variacoes' = subset(variacoesCapitais, mes == mmm, select = c('codigo', 'nome', categoria))
             )             
           }
    )
    names(temp1)[names(temp1) == categoria] <- 'valor'
    temp2 <- tail(x = temp1[with(temp1, order(x = valor, decreasing = TRUE)), ], n = nrow(temp1) - 8)
    temp2 <- data.frame('codigo' = c(novoCodigo), 'nome' = c(novoNome), valor = c(sum(temp2$valor)))
    temp1 <- head(x = temp1[with(temp1, order(x = valor, decreasing = TRUE)), ], n = 8)
    temp1 <- rbind(temp1, temp2)
    temp1$nome <- factor(temp1$nome, levels = temp1$nome[order(temp1$valor)])
    return(temp1)
  }
  
  dadosHistograma <- function() {
    switch(abrangencia,
           'estados' = {
             temp <- switch(informacao,
                            'admitidos' = subset(admitidosEstados, select = c('mes', categoria)),
                            'desligados' = subset(desligadosEstados, select = c('mes', categoria)),
                            'variacoes' = subset(variacoesEstados, select = c('mes', categoria))
             )
           },
           'capitais' = {
             temp <- switch(informacao,
                            'admitidos' = subset(admitidosCapitais, select = c('mes', categoria)),
                            'desligados' = subset(desligadosCapitais, select = c('mes', categoria)),
                            'variacoes' = subset(variacoesCapitais, select = c('mes', categoria))
             )             
           }
    )
    names(temp)[names(temp) == categoria] <- 'valor'
    temp = aggregate(valor ~ mes, data = temp, sum)
    return(temp)    
  }
  
  colunasTabela <- function() {
    switch(variavel,
           'faixa_etaria' = {
             c('codigo', 'nome', 'mes', 'ate_17',
               'de_18_a_24', 'de_25_a_29', 'de_30_a_39', 'de_40_a_49', 'de_50_a_64',
               'de_65_ou_mais', 'total') 
           },
           'grau_de_instrucao' = {
             c('codigo', 'nome', 'mes', 'analfabeto',
               'ate_5a_incompleto', 'de_5a_completo_fundamental', 'de_6a_a_9a_fundamental', 'fundamental_completo',
               'medio_incompleto', 'medio_completo', 'superior_incompleto', 'superior_completo', 'total'
             )                        
           },
           'setor' =  {
             c('codigo', 'nome', 'mes', 'industria', 'construcao_civil',
               'comercio', 'servicos', 'agropecuaria', 'total')  
           },
           'sexo' = {
             c('codigo', 'nome', 'mes', 'masculino', 'feminino', 'total')
           },
           'total' = {
             c('codigo', 'nome', 'mes', 'total')                       
           }
    ) 
  }  
  
  dadosTabela <- function() {
    switch(abrangencia,
           'estados' = {
             switch(informacao,
                    'admitidos' = subset(admitidosEstados, mes == mmm, select = colunasTabela()),
                    'desligados' = subset(desligadosEstados, mes == mmm, select = colunasTabela()),
                    'variacoes' = subset(variacoesEstados, mes == mmm, select = colunasTabela())
             )
           },
           'capitais' = {
             switch(informacao,
                    'admitidos' = subset(admitidosCapitais, mes == mmm, select = colunasTabela()),
                    'desligados' = subset(desligadosCapitais, mes == mmm, select = colunasTabela()),
                    'variacoes' = subset(variacoesCapitais, mes == mmm, select = colunasTabela())
             )             
           }
    )
  }

  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {    
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- names(categorias[categorias == categoria])
          if (inicial) {
            session$output$cartogramaCAGED1 <- renderLeaflet({
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
                if (abrangencia == 'estados') {
                  camada <- camadaEstados()
                  caixas <- unique(quantile(camada$valor, names = FALSE, prob = seq(0, 1, length = 7), type = 1))
                  qpal <- colorBin('YlOrRd', domain = camada$valor, bins = caixas)
                  rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s: %s</strong>',
                                     camada$codigo,
                                     camada$local,
                                     titulo,
                                     format(x = camada$valor, big.mark = '.')
                  ) %>%
                    lapply(htmltools::HTML)
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
                    labFormat = labelFormat(big.mark = '.', between = ' a ')
                  )        
                } else {
                  camada <- camadaCapitais()
                  caixas <- unique(quantile(camada$valor, names = FALSE, prob = seq(0, 1, length = 7), type = 1))
                  qpal <- colorBin('YlOrRd', domain = camada$valor, bins = caixas)
                  rotulos <- sprintf('<strong>Codigo: %s</strong><br/><strong>Capital: %s</strong><br/><strong>%s: %s</strong>',
                                     camada$codigo,
                                     camada$local,
                                     titulo,
                                     format(x = camada$valor, big.mark = '.')
                  ) %>%
                    lapply(htmltools::HTML)
                  m <- addCircleMarkers(map = m,
                                        data = camada,
                                        fillColor = ~qpal(valor),
                                        weight = 2,
                                        opacity = 1,
                                        color = '#FFFFFF',
                                        fillOpacity = 0.7,
                                        radius = 10,
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
                    labFormat = labelFormat(big.mark = '.', between = ' a ')
                  )        
                } 
                m
              })
            })
          } else {
            isolate({ 
              m <- leafletProxy(mapId = 'cartogramaCAGED1', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
              if (abrangencia == 'estados') {
                camada <- camadaEstados()
                caixas <- unique(quantile(camada$valor, names = FALSE, prob = seq(0, 1, length = 7), type = 1))
                qpal <- colorBin('YlOrRd', domain = camada$valor, bins = caixas)
                rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s: %s</strong>',
                                   camada$codigo,
                                   camada$local,
                                   titulo,
                                   format(x = camada$valor, big.mark = '.')
                ) %>%
                  lapply(htmltools::HTML)
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
                  labFormat = labelFormat(big.mark = '.', between = ' a ')
                )        
              } else {
                camada <- camadaCapitais()
                caixas <- unique(quantile(camada$valor, names = FALSE, prob = seq(0, 1, length = 7), type = 1))
                qpal <- colorBin('YlOrRd', domain = camada$valor, bins = caixas)
                rotulos <- sprintf('<strong>Codigo: %s</strong><br/><strong>Capital: %s</strong><br/><strong>%s: %s</strong>',
                                   camada$codigo,
                                   camada$local,
                                   titulo,
                                   format(x = camada$valor, big.mark = '.')
                ) %>%
                  lapply(htmltools::HTML)
                m %>% addCircleMarkers(data = camada,
                                       fillColor = ~qpal(valor),
                                       weight = 2,
                                       opacity = 1,
                                       color = '#FFFFFF',
                                       fillOpacity = 0.7,
                                       radius = 10,
                                       label = rotulos,
                                       labelOptions = labelOptions(
                                         style = list('font-weight' = 'normal', padding = '3px 8px'),
                                         textsize = '15px',
                                         direction = 'auto'
                                       )
                )
                m %>% addLegend(
                  pal = qpal,
                  values = camada$valor,
                  opacity = 0.7,
                  title = titulo,
                  position = 'bottomright',
                  labFormat = labelFormat(big.mark = '.', between = ' a ')
                )        
              } 
            })
          }
          show(id = 'cartogramaCAGED1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaCAGED1')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) { 
          session$output$graficoCAGED1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$nome, y = dg$valor)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#ffaa00') +
                labs(title = names(categorias[categorias == categoria]), x = '', y = '') +
                geom_text(aes(label = format(x = dg$valor, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })
          show(id = 'graficoCAGED1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoCAGED1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes) { 
          session$output$histogramaCAGED1 <- renderPlot({
            isolate({
              dh <- dadosHistograma()
              ggplot(data = dh, aes(x = dh$mes, y = dh$valor)) +
                geom_bar(position = 'dodge', stat = 'identity', colour = '#ffffff', fill = ifelse(dh$mes == mmm, '#ff6f00', '#ffaa00')) +
                labs(title = names(categorias[categorias == categoria]), x = '', y = '') +
                geom_text(aes(label = format(x = dh$valor, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })
          show(id = 'histogramaCAGED1')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaCAGED1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 4)
        ## Tabela ##
        if ('tabela' %in% visoes) { 
          session$output$tabelaCAGED1 <- DT::renderDataTable({
            isolate({
              titulos <- c('Código' = 'codigo',
                           'Nome' = 'nome',
                           'Mês' = 'mes',
                           'Total' = 'total'
              ) 
              titulos <- c(titulos, categorias)
              DT::datatable(
                data = dadosTabela(),
                colnames = titulos,
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              ) %>% formatStyle(columns = names(categorias[categorias == categoria]), backgroundColor = '#e8e8e8')
            })            
          })
          show(id = 'tabelaCAGED1')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaCAGED1')
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
  
  observeEvent(input$buttonFiltrosCAGED1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectAbrangenciaCAGED1',
                             label = 'Abrangência:',
                             choices = abrangencias,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectAbrangenciaCAGED1), abrangencia, session$input$selectAbrangenciaCAGED1)
                 )
          ),        
          column(width = 6,
                 selectInput(inputId = 'selectMesCAGED1',
                             label = 'Mês:',
                             choices = meses,
                             selectize = FALSE,                             
                             selected = ifelse(is.null(session$input$selectMesCAGED1), mmm, session$input$selectMesCAGED1)
                 )
          )
        ),
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectInformacaoCAGED1',
                             label = 'Informação:',
                             choices = informacoes,
                             selectize = FALSE,                             
                             selected = ifelse(is.null(session$input$selectInformacaoCAGED1), informacao, session$input$selectInformacaoCAGED1)
                 )
          ),        
          column(width = 6,
                 selectInput(inputId = 'selectVariavelCAGED1',
                             label = 'Variável:',
                             choices = variaveis,
                             selectize = FALSE,                             
                             selected = ifelse(is.null(session$input$selectVariavelCAGED1), variavel, session$input$selectVariavelCAGED1)
                 )
          )
        ),
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectCategoriaCAGED1',
                             label = 'Categoria:',
                             choices = categorias,
                             selectize = FALSE,                             
                             selected = categoria
                 )
          ),
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesCAGED1',
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
          actionButton(inputId = 'buttonOkCAGED1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelCAGED1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$selectVariavelCAGED1, {
    categorias <<- switch(input$selectVariavelCAGED1,
                          'faixa_etaria' = {faixas},
                          'grau_de_instrucao' = {graus},
                          'setor' =  {setores},
                          'sexo' = {sexos},
                          'total' = {totais}
    ) 
    updateSelectInput(inputId = 'selectCategoriaCAGED1',
                      session = session,
                      choices = categorias
    )
  })
  
  observeEvent(input$buttonOkCAGED1, {
    ## Fechar modal
    removeModal()    
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    abrangencia <<- session$input$selectAbrangenciaCAGED1
    mmm <<- session$input$selectMesCAGED1
    informacao <<- session$input$selectInformacaoCAGED1
    variavel <<- session$input$selectVariavelCAGED1
    categoria <<- session$input$selectCategoriaCAGED1
    visoes <<- session$input$checkboxVisoesCAGED1
    ## Subtitulo ##
    output$textSubtituloCAGED1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelCAGED1, {
    updateSelectInput(inputId = 'selectAbrangenciaCAGED1', session = session, selected = abrangencia)
    updateSelectInput(inputId = 'selectMesCAGED1', session = session, selected = mmm)
    updateSelectInput(inputId = 'selectInformacaoCAGED1', session = session, selected = informacao)
    updateSelectInput(inputId = 'selectVariavelCAGED1', session = session, selected = variavel)
    updateSelectInput(inputId = 'selectCategoriaCAGED1', session = session, selected = categoria)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesCAGED1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaCAGED1_state, {
    tryCatch({
      info <- input$tabelaCAGED1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##

  ## Subtitulo ##
  session$output$textSubtituloCAGED1 <- renderText({
    isolate({
      subTitulo()
    })
  })

  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'capitais')) {
        capitais <<- geojsonio::geojson_read(x = 'dados/geojsons/capitais.geojson', what = 'sp')        
      }
      incProgress(1 / 10)  
      if (!exists(x = 'capitaisOk')) {
        for (coluna in colnames(capitais@data)) {
          if (is.factor(capitais@data[ , coluna])) {
            caractere <- as.character(capitais@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            capitais@data[ , coluna] <<- as.factor(caractere)
          }
        }
        capitaisOk <<- TRUE
      }
      incProgress(1 / 10)
      if (!exists(x = 'estados')) {
        estados <<- geojsonio::geojson_read(x = 'dados/geojsons/estados.geojson', what = 'sp')
      }
      incProgress(1 / 10)      
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
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/admitidos-capitais.Rda')
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/desligados-capitais.Rda')
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/variacoes-capitais.Rda')
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/admitidos-estados.Rda')
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/desligados-estados.Rda')
      incProgress(1 / 10)
      load(file = 'dados/CAGED1/variacoes-estados.Rda')
      incProgress(1 / 10)
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
