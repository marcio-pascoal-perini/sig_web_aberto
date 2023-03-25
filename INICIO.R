############
## Página ##
############

INICIO_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  tabPage <- fluidPage(
    fluidRow(column(
      width = 12,
      HTML(
        '<h4 style="text-align: center; line-height: 1.8;">
                  <b>SIG Web Aberto</b> (<b>S</b>istema de <b>I</b>nformação <b>G</b>eográfica para <b>Web</b> de dados <b>Aberto</b>s)
                  </h4>'
      )
    )),
    fluidRow(column(width = 12, HTML('&nbsp;'))),
    fluidRow(column(
      width = 12,
      align = 'center',
      HTML(
        '<span style = "font-weight: normal; font-size: 22px;">Contato</span>'
      )
    )),
    fluidRow(column(width = 12, HTML('&nbsp;'))),
    fluidRow(column(
      width = 6,
      textInput(
        inputId = 'inputNomeINICIO',
        label = 'Nome:',
        placeholder = 'Seu nome',
        width = '100%'
      )
    ),
    column(
      width = 6,
      textInput(
        inputId = 'inputEmailINICIO',
        label = 'Email:',
        placeholder = 'Seu email',
        width = '100%'
      )
    )),
    fluidRow(column(
      width = 12,
      textAreaInput(
        inputId = 'inputMensagemINICIO',
        label = 'Mensagem:',
        placeholder = 'Sua mensagem',
        rows = 10
      )
    )),
    fluidRow(column(
      width = 6,
      actionButton(
        inputId = 'buttonEnviarINICIO',
        class = 'btn-primary',
        style = 'font-weight: 600; color: #FFFFFF;',
        label = 'Enviar mensagem'
      )
    ))
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

INICIO_Admin <- function(session) {
  ## funções ##
  
  verificarEmail <- function(email) {
    grepl(
      '\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>',
      as.character(email),
      ignore.case = TRUE
    )
  }
  
  caixaDeMensagem <- function(titulo, mensagem) {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 's',
        title = HTML(text = titulo),
        HTML(text = mensagem),
        footer = tagList(actionButton(
          inputId = 'buttonOkINICIO', label = 'Ok'
        ))
      )
    )
  }

  enviarEmail2 <- function(nome, email, mensagem) {
    endereco <- 'YOUR ADDRESS ON MAILJET'
    chavePublica <- 'YOUR API KEY'
    chavePrivada <- 'YOUR SECRET KEY'
    emailRemetente <- 'YOUR SENDER EMAIL'
    emailDestinatario <- 'YOUR RECIPIENT EMAIL'
    assunto <- 'Mensagem enviada através do formulário contato'
    assunto <- iconv(x = assunto, from = 'UTF-8', to = 'BIG-FIVE', sub = 'Unicode')
    nome <- iconv(x = nome, from = 'UTF-8', to = 'BIG-FIVE', sub = 'Unicode')
    email <- iconv(x = email, from = 'UTF-8', to = 'BIG-FIVE', sub = 'Unicode')
    dados <- 'Nome: %s\n\nEmail: %s\n\nMensagem: %s\n\n'
    dados <- sprintf(dados, nome, email, mensagem)
    mensagem <- stri_replace_all_fixed(dados, '\n', '<br>')
    mensagem <- iconv(x = mensagem, from = 'UTF-8', to = 'BIG-FIVE', sub = 'Unicode')
    dados <- '{"Messages":[{"From":{"Email":"%s","Name":"%s"},"To":[{"Email":"%s","Name":"%s"}],"Subject":"%s","TextPart":"%s","HTMLPart":"%s"}]}'
    dados <- sprintf(dados, emailRemetente, 'SIG Web Aberto', emailDestinatario, 'Contato', assunto, '', mensagem)
    dados <- jsonlite::minify(dados)
    usuarioSenha <- sprintf('%s:%s', chavePublica, chavePrivada)
    h <- curl::new_handle()
    curl::handle_setopt(handle = h, .list = list(post = TRUE, postfields = dados, verbose = FALSE, httpauth = 1, userpwd = usuarioSenha))
    curl::handle_setheaders(
      handle = h,
      .list = list(
        'Content-Type' = 'application/json; charset="utf-8"',
        'Content-Length' = as.character(stri_length(str = dados)),
        'Accept' = 'application/json'
      )
    )
    tryCatch(
      expr = {
        resposta <- curl::curl_fetch_memory(url = endereco, handle = h)
        if (resposta$status_code %in% c(200, 201, 202)) {
          return('<span>Sua mensagem foi enviada e em breve entraremos em contado.</span>')
        } else {
          return(
            '<span style="color: #FF0000;">Erro:&nbsp;</span><span>A mensagem não pode ser enviada no momento. Tente novamente mais tarde!</span>'
          )
        }
      },
      error = function(e) {
        return(
          '<span style="color: #FF0000;">Erro:&nbsp;</span><span>A mensagem não pode ser enviada no momento. Tente novamente mais tarde!</span>'
        )
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonEnviarINICIO, {
    texto <- ''
    nome <- stri_trim(str = input$inputNomeINICIO, side = 'both')
    email <- stri_trim(str = input$inputEmailINICIO, side = 'both')
    mensagem <-
      stri_trim(str = input$inputMensagemINICIO, side = 'both')
    if (stri_isempty(nome)) {
      texto <-
        '<p style="color: #ff0000;">O campo <i>Nome</i> é obrigatório.</p>'
    }
    if (stri_isempty(email)) {
      texto <-
        paste(
          texto,
          '<p style="color: #ff0000;">O campo <i>Email</i> é obrigatório.</p>',
          sep = ''
        )
    } else {
      if (!verificarEmail(email)) {
        texto <-
          paste(texto,
                '<p style="color: #ff0000;">Email inválido.</p>',
                sep = '')
      }
    }
    if (stri_isempty(mensagem)) {
      texto <-
        paste(
          texto,
          '<p style="color: #ff0000;">O campo <i>Mensagem</i> é obrigatório.</p>',
          sep = ''
        )
    }
    if (stri_isempty(texto)) {
      caixaDeMensagem('Informação',
                      enviarEmail2(
                        nome = nome,
                        email = email,
                        mensagem = mensagem
                      ))
      updateTextInput(inputId = 'inputMensagemINICIO',
                      session = session,
                      value = '')
    } else  {
      caixaDeMensagem('Informação', texto)
    }
  })
  
  observeEvent(input$buttonOkINICIO, {
    removeModal()
  })
}

