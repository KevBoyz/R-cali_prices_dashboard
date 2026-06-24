source("global.R")
source("pages/dashboard.R")
source("pages/visualizacoes.R")
source("pages/estatistica.R")
source("pages/mapa.R")
source("pages/associacao.R")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  div(
    class = "container",
    div(
      class = "navbar",
      tags$a("Dashboard", href = "#", class = "navbar-title",
             onclick = "Shiny.setInputValue('pagina','dashboard',{priority:'event'})"),
      div(
        class = "navbar-links",
        tags$a("Mapa Interativo",        href = "#", class = "nav-link",
               onclick = "Shiny.setInputValue('pagina','mapa',{priority:'event'})"),
        tags$a("Visualizações",          href = "#", class = "nav-link",
               onclick = "Shiny.setInputValue('pagina','visualizacoes',{priority:'event'})"),
        tags$a("Análise de Associação",  href = "#", class = "nav-link",
               onclick = "Shiny.setInputValue('pagina','associacao',{priority:'event'})"),
        tags$a("Estatística Descritiva", href = "#", class = "nav-link",
               onclick = "Shiny.setInputValue('pagina','estatistica',{priority:'event'})")
      )
    ),
    uiOutput("conteudo_principal")
  )
)

server <- function(input, output, session) {

  pagina_atual <- reactive({
    if (is.null(input$pagina)) "dashboard" else input$pagina
  })

  dashboard_server(input, output, session)
  visualizacoes_server(input, output, session)
  estatistica_server(input, output, session)
  mapa_server(input, output, session)
  associacao_server(input, output, session)

  output$conteudo_principal <- renderUI({
    switch(pagina_atual(),
           "dashboard"     = dashboard_ui(),
           "visualizacoes" = visualizacoes_ui(),
           "estatistica"   = estatistica_ui(),
           "mapa"          = mapa_ui(),
           "associacao"    = associacao_ui(),
           div(class = "stats-page", h2("Página não encontrada"))
    )
  })
}

shinyApp(ui, server)
