library(leaflet)

mapa_ui <- function() {
  div(
    class = "stats-page",
    h2("Mapa Interativo"),
    p("Distribuição geográfica dos blocos habitacionais da Califórnia."),

    div(
      class = "viz-block",

      div(
        class = "viz-sidebar",
        style = "overflow-y:auto; max-height:700px;",

        h4("Visualização"),

        selectInput("map_color_var", "Colorir por:",
                    choices  = c(setNames(num_vars, num_vars), "ocean_proximity"),
                    selected = "median_house_value"),

        selectInput("map_size_var", "Tamanho por:",
                    choices  = c("Fixo" = "fixo", setNames(num_vars, num_vars)),
                    selected = "fixo"),

        conditionalPanel(
          "input.map_size_var == 'fixo'",
          sliderInput("map_fixed_size", "Raio dos pontos (px):",
                      min = 2, max = 14, value = 5, step = 1)
        ),

        sliderInput("map_sample", "Pontos exibidos:",
                    min = 500, max = nrow(housing), value = 5000, step = 500),

        hr(),
        h4("Filtros"),

        checkboxGroupInput("map_ocean", "Proximidade ao oceano:",
                           choices  = sort(unique(housing$ocean_proximity)),
                           selected = sort(unique(housing$ocean_proximity))),

        sliderInput("map_value_range", "Valor mediano dos imóveis (USD):",
                    min   = min(housing$median_house_value, na.rm = TRUE),
                    max   = max(housing$median_house_value, na.rm = TRUE),
                    value = range(housing$median_house_value, na.rm = TRUE),
                    step  = 10000),

        sliderInput("map_income_range", "Renda mediana (×$10k):",
                    min   = floor(min(housing$median_income, na.rm = TRUE)),
                    max   = ceiling(max(housing$median_income, na.rm = TRUE)),
                    value = range(housing$median_income, na.rm = TRUE),
                    step  = 0.5),

        sliderInput("map_age_range", "Idade mediana dos imóveis (anos):",
                    min   = min(housing$housing_median_age, na.rm = TRUE),
                    max   = max(housing$housing_median_age, na.rm = TRUE),
                    value = range(housing$housing_median_age, na.rm = TRUE),
                    step  = 1)
      ),
      div(
        class = "viz-plot box",
        style = "display:flex; flex-direction:column; padding:0; overflow:hidden;",
        leafletOutput("mapa_leaflet", height = "480px"),
        div(style = "padding:14px; border-top:1px solid #e2e8f0;",
            uiOutput("mapa_stats"))
      )
    )
  )
}

mapa_server <- function(input, output, session) {

  dados_filtrados <- reactive({
    req(input$map_ocean,
        input$map_value_range, input$map_income_range, input$map_age_range)
    df <- housing
    df <- df[df$ocean_proximity %in% input$map_ocean, ]
    df <- df[!is.na(df$median_house_value) &
               df$median_house_value >= input$map_value_range[1] &
               df$median_house_value <= input$map_value_range[2], ]
    df <- df[!is.na(df$median_income) &
               df$median_income >= input$map_income_range[1] &
               df$median_income <= input$map_income_range[2], ]
    df <- df[!is.na(df$housing_median_age) &
               df$housing_median_age >= input$map_age_range[1] &
               df$housing_median_age <= input$map_age_range[2], ]
    df
  })

  # ── Amostra para renderização (evita lentidão com 20k pontos) ─────────
  dados_mapa <- reactive({
    df <- dados_filtrados()
    if (nrow(df) == 0) return(df)
    df[sample(nrow(df), min(input$map_sample, nrow(df))), ]
  })

  # ── Dados no viewport atual — base para as estatísticas ───────────────
  dados_viewport <- reactive({
    df     <- dados_filtrados()
    bounds <- input$mapa_leaflet_bounds
    if (is.null(bounds) || nrow(df) == 0) return(df)
    df[df$latitude  >= bounds$south & df$latitude  <= bounds$north &
         df$longitude >= bounds$west  & df$longitude <= bounds$east, ]
  })

  # ── Mapa base — renderiza uma vez; atualizações via leafletProxy ───────
  output$mapa_leaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -119.5, lat = 37.0, zoom = 6)
  })

  # ── Atualiza marcadores e legenda via proxy ────────────────────────────
  observe({
    df    <- dados_mapa()
    proxy <- leafletProxy("mapa_leaflet") %>% clearMarkers() %>% clearControls()
    req(nrow(df) > 0, input$map_color_var, input$map_size_var)

    color_var <- input$map_color_var

    # Paleta de cores
    if (color_var == "ocean_proximity") {
      cats <- sort(unique(housing$ocean_proximity))
      pal  <- colorFactor(group_palette[seq_along(cats)], domain = cats)
      cols <- pal(df$ocean_proximity)
    } else {
      pal  <- colorNumeric("YlOrRd", domain = housing[[color_var]], na.color = "#aaaaaa")
      cols <- pal(df[[color_var]])
    }

    # Raio dos pontos
    if (input$map_size_var == "fixo") {
      raios <- rep(input$map_fixed_size, nrow(df))
    } else {
      x     <- df[[input$map_size_var]]
      vmin  <- min(x, na.rm = TRUE); vmax <- max(x, na.rm = TRUE)
      raios <- if (vmax == vmin) rep(5, nrow(df)) else 3 + 9 * (x - vmin) / (vmax - vmin)
    }

    # Popup ao clicar no ponto
    popups <- paste0(
      "<b>ocean_proximity:</b> ",    df$ocean_proximity,                                         "<br>",
      "<b>median_house_value:</b> $", format(df$median_house_value, big.mark = ","),             "<br>",
      "<b>median_income:</b> ",       round(df$median_income, 2),                                "<br>",
      "<b>housing_median_age:</b> ",  df$housing_median_age, " anos",                            "<br>",
      "<b>total_rooms:</b> ",         format(df$total_rooms, big.mark = ","),                    "<br>",
      "<b>total_bedrooms:</b> ",      ifelse(is.na(df$total_bedrooms), "N/A",
                                             format(df$total_bedrooms, big.mark = ",")),          "<br>",
      "<b>population:</b> ",          format(df$population, big.mark = ","),                     "<br>",
      "<b>households:</b> ",          format(df$households, big.mark = ",")
    )

    proxy <- proxy %>%
      addCircleMarkers(
        data        = df,
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = raios,
        color       = "white",
        weight      = 0.4,
        fillColor   = cols,
        fillOpacity = 0.75,
        popup       = popups
      )

    # Legenda
    if (color_var == "ocean_proximity") {
      proxy %>% addLegend("bottomright", pal = pal,
                          values  = sort(unique(housing$ocean_proximity)),
                          title   = "ocean_proximity", opacity = 0.85)
    } else {
      proxy %>% addLegend("bottomright", pal = pal,
                          values    = housing[[color_var]],
                          title     = color_var,
                          opacity   = 0.85,
                          labFormat = labelFormat(big.mark = ","))
    }
  })

  # ── Painel de estatísticas do viewport ────────────────────────────────
  output$mapa_stats <- renderUI({
    df_view <- dados_viewport()
    df_filt <- dados_filtrados()
    n_view  <- nrow(df_view)
    n_filt  <- nrow(df_filt)

    if (n_filt == 0) {
      return(div(style = "color:#888; font-size:0.88em;",
                 "Nenhum bloco corresponde aos filtros aplicados."))
    }

    stat_card <- function(label, value) {
      div(
        style = "display:flex; flex-direction:column; align-items:center;
                 background:#f8fafc; border:1px solid #e2e8f0; border-radius:6px;
                 padding:8px 14px; min-width:130px;",
        div(style = "font-size:0.74em; color:#64748b; margin-bottom:3px; text-align:center;", label),
        div(style = "font-size:0.92em; font-weight:bold; color:#1a2847;", value)
      )
    }

    fmt_usd <- function(x) paste0("$", format(round(x), big.mark = ","))
    fmt_num <- function(x, d = 1) format(round(x, d), nsmall = d)

    caption <- if (is.null(input$mapa_leaflet_bounds)) {
      sprintf("Total com filtros: %s blocos", format(n_filt, big.mark = ","))
    } else {
      sprintf("Área visível: %s blocos (de %s filtrados)",
              format(n_view, big.mark = ","), format(n_filt, big.mark = ","))
    }

    div(
      div(style = "font-size:0.82em; color:#64748b; margin-bottom:10px;", caption),
      div(
        style = "display:flex; flex-wrap:wrap; gap:8px;",
        stat_card("Valor med. — média",    fmt_usd(mean(df_view$median_house_value,   na.rm = TRUE))),
        stat_card("Valor med. — mediana",  fmt_usd(median(df_view$median_house_value, na.rm = TRUE))),
        stat_card("Renda med. — média",    fmt_num(mean(df_view$median_income,         na.rm = TRUE))),
        stat_card("Idade med. — média",    paste0(fmt_num(mean(df_view$housing_median_age, na.rm = TRUE)), " anos")),
        stat_card("População total",       format(sum(df_view$population,  na.rm = TRUE), big.mark = ",")),
        stat_card("Domicílios total",      format(sum(df_view$households,  na.rm = TRUE), big.mark = ","))
      )
    )
  })
}
