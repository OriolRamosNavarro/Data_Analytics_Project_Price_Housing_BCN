# =============================================================================
# mod_mapa.R — Mapa de preus interactiu (leaflet)
# Millores: llegenda gradient, tooltip ampliat, layout millorat
# =============================================================================
library(shiny); library(ggplot2); library(dplyr); library(scales)
library(leaflet); library(shinyWidgets)

mod_mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "plot-container",
      style = "padding:14px 18px; margin-bottom:14px; background:linear-gradient(135deg,#f0fdf4,#eff6ff);",
      tags$div(style = "display:flex; gap:14px; align-items:center;",
        tags$i(class = "fa-solid fa-map-location-dot", style = "font-size:26px; color:#10b981;"),
        tags$div(
          tags$div(style = "font-size:13px; font-weight:700; color:#166534; margin-bottom:2px;",
                   "On es paga més car? Mapa territorial de preus"),
          tags$div(style = "font-size:12px; color:#15803d; line-height:1.5;",
            "Cada cercle representa un barri o districte. La mida i el color indiquen el preu: ",
            tags$strong("vermell = car"), ", ", tags$strong("verd = econòmic"),
            ". Clica qualsevol cercle per veure els detalls."
          )
        )
      )
    ),

    fluidRow(
      column(3,
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Mètrica"),
          selectInput(ns("metrica"), NULL,
            choices = c(
              "Preu per m² (EUR/m²)"  = "preu_m2",
              "Preu total (milers EUR)" = "preu_total",
              "Compravendes (nre. op.)" = "compravendes",
              "Superfície (m²)"         = "superficie"
            ),
            selected = "preu_m2"
          ),
          uiOutput(ns("ui_tipus")),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Granularitat"),
          radioGroupButtons(ns("nivell"), NULL,
            choices = c("Barris (73)" = "barri", "Districtes (10)" = "districte"),
            selected = "barri", justified = TRUE, size = "sm"
          ),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Trimestre"),
          uiOutput(ns("ui_periode"))
        ),
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Estadístiques del trimestre"),
          uiOutput(ns("kpis_lat"))
        )
      ),

      column(9,
        tags$div(class = "map-container",
          tags$div(class = "map-header",
            tags$div(
              tags$div(class = "plot-title", uiOutput(ns("map_titol"))),
              tags$div(class = "plot-subtitle",
                       "Mida i color proporcionals al valor · Clica un cercle per veure detalls")
            ),
            uiOutput(ns("yoy_badge"))
          ),
          leafletOutput(ns("mapa"), height = "420px"),
          tags$div(
            style = "padding:8px 16px; border-top:1px solid #f1f5f9; display:flex; justify-content:space-between; align-items:center;",
            uiOutput(ns("llegenda_gradient")),
            tags$div(style = "font-size:10px; color:#94a3b8;",
                     "Font: Registre de la Propietat · Ajuntament de Barcelona")
          )
        ),

        fluidRow(
          column(12,
            tags$div(class = "plot-container",
              tags$div(class = "plot-header",
                tags$div(class = "plot-title", "Rànquing de barris / districtes"),
                tags$div(class = "plot-subtitle", uiOutput(ns("rank_subtitol")))
              ),
              plotOutput(ns("plot_rank"), height = "360px"),
              tags$div(class = "plot-footer",
                       "Els valors a la dreta de cada barra indiquen el valor exacte de la mètrica seleccionada.")
            )
          )
        )
      )
    )
  )
}

mod_mapa_server <- function(id, df_hab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    periodes_all <- get_periodes(df_hab)

    output$ui_tipus <- renderUI({
      opts <- TIPUS_PER_METRICA[[req(input$metrica)]]
      selectInput(ns("tipus"), "Tipus d'habitatge",
                  choices = setNames(opts, TIPUS_LABEL[opts]), selected = opts[1])
    })

    output$ui_periode <- renderUI({
      sliderTextInput(ns("periode"), NULL, choices = periodes_all,
                      selected = tail(periodes_all, 1), grid = FALSE)
    })

    dades_mapa <- reactive({
      req(input$metrica, input$tipus, input$nivell, input$periode)
      df_hab %>%
        filter(
          nivell == input$nivell, metrica == input$metrica,
          tipus_habitatge == input$tipus, periode == input$periode,
          !is.na(codi), !is.na(valor), valor > 0
        ) %>%
        mutate(codi_int = as.integer(codi))
    })

    dades_geo <- reactive({
      d <- dades_mapa()
      req(nrow(d) > 0)
      if (input$nivell == "barri") {
        d %>% left_join(BARRI_COORDS, by = c("codi_int" = "codi"))
      } else {
        # Ara DISTRICTE_COORDS té els mateixos noms accentuats que el CSV
        d %>% left_join(DISTRICTE_COORDS, by = "nom")
      }
    })

    pal_leaflet <- reactive({
      d <- dades_mapa()
      req(nrow(d) > 0)
      colorNumeric(
        palette = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
        domain  = d$valor, na.color = "#aaaaaa"
      )
    })

    output$map_titol <- renderUI({
      req(input$metrica, input$periode)
      paste0(METRICA_LABEL[input$metrica], " · ", input$periode)
    })

    output$rank_subtitol <- renderUI({
      req(input$periode)
      paste0("Top 15 · ", input$periode, " · per valor descendent")
    })

    output$yoy_badge <- renderUI({
      req(input$periode, input$metrica, input$tipus, input$nivell)
      p_prev <- periodes_all[which(periodes_all == input$periode) - 4]
      if (length(p_prev) == 0) return(NULL)
      bcn_now  <- df_hab %>% filter(nivell == "barcelona", metrica == input$metrica,
                                    tipus_habitatge == input$tipus, periode == input$periode) %>% pull(valor)
      bcn_prev <- df_hab %>% filter(nivell == "barcelona", metrica == input$metrica,
                                    tipus_habitatge == input$tipus, periode == p_prev) %>% pull(valor)
      if (length(bcn_now) == 0 || length(bcn_prev) == 0 || bcn_prev == 0) return(NULL)
      yoy  <- (bcn_now - bcn_prev) / bcn_prev * 100
      col  <- if (yoy >= 0) "#fee2e2" else "#dcfce7"
      tcol <- if (yoy >= 0) "#991b1b" else "#166534"
      icon_ <- if (yoy >= 0) "↑" else "↓"
      tags$div(
        style = paste0("background:", col, "; color:", tcol,
                       "; border-radius:20px; padding:5px 14px; font-size:12px; font-weight:700;"),
        paste0(icon_, " ", sprintf("%+.1f%%", yoy), " vs. any anterior (BCN total)")
      )
    })

    # ── Llegenda gradient ──
    output$llegenda_gradient <- renderUI({
      d <- dades_mapa()
      req(nrow(d) > 0)
      v_min <- floor(min(d$valor, na.rm = TRUE))
      v_med <- round(median(d$valor, na.rm = TRUE))
      v_max <- ceiling(max(d$valor, na.rm = TRUE))
      fmt <- function(v) fmt_valor(v, input$metrica)
      tags$div(
        style = "display:flex; align-items:center; gap:8px;",
        tags$div(style = "font-size:11px; color:#94a3b8; white-space:nowrap;", "Menys car"),
        tags$div(
          style = paste0(
            "width:160px; height:10px; border-radius:5px;",
            "background:linear-gradient(to right,#1a9641,#a6d96a,#ffffbf,#fdae61,#d7191c);"
          )
        ),
        tags$div(style = "font-size:11px; color:#94a3b8; white-space:nowrap;", "Més car"),
        tags$div(style = "font-size:10px; color:#cbd5e1; margin-left:8px;",
                 paste0(fmt(v_min), " – ", fmt(v_med), " – ", fmt(v_max)))
      )
    })

    # ── Mapa leaflet base ──
    output$mapa <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = tileOptions(minZoom = 11, maxZoom = 16)) %>%
        setView(lng = 2.1734, lat = 41.3851, zoom = 12.5)
    })

    # Actualitza markers
    observe({
      d   <- dades_geo()
      pal <- pal_leaflet()
      req(nrow(d) > 0, !is.null(d$lat))
      d <- d %>% filter(!is.na(lat), !is.na(lon), !is.na(valor))
      req(nrow(d) > 0)

      r_min <- if (input$nivell == "barri") 5  else 12
      r_max <- if (input$nivell == "barri") 22 else 38
      d$radius <- rescale(d$valor, to = c(r_min, r_max))

      popup_html <- paste0(
        "<div style='font-family:Inter,sans-serif; min-width:200px;'>",
        "<div style='font-weight:700; font-size:14px; color:#0f172a; margin-bottom:10px; padding-bottom:8px; border-bottom:1px solid #f1f5f9;'>",
        d$nom, "</div>",
        "<table style='width:100%; font-size:12px;'>",
        "<tr><td style='color:#94a3b8; padding:3px 0;'>", METRICA_LABEL[input$metrica], "</td>",
        "<td style='text-align:right; font-weight:700; color:#0f172a;'>",
        mapply(fmt_valor, d$valor, input$metrica), "</td></tr>",
        "<tr><td style='color:#94a3b8; padding:3px 0;'>Trimestre</td>",
        "<td style='text-align:right; font-weight:600; color:#374151;'>", input$periode, "</td></tr>",
        "</table></div>"
      )

      leafletProxy("mapa", session) %>%
        clearShapes() %>%
        addCircleMarkers(
          data = d, lng = ~lon, lat = ~lat,
          radius      = ~radius,
          color       = "white", weight = 1.5, opacity = 1,
          fillColor   = ~pal(valor), fillOpacity = 0.85,
          popup       = popup_html,
          label       = ~paste0(nom, ": ", mapply(fmt_valor, valor, input$metrica)),
          labelOptions = labelOptions(
            style = list("font-family" = "Inter,sans-serif", "font-size" = "12px",
                         "font-weight" = "600", "border" = "none",
                         "box-shadow" = "0 2px 8px rgba(0,0,0,.1)", "border-radius" = "8px",
                         "padding" = "6px 10px"),
            direction = "top", offset = c(0, -8)
          )
        )
    })

    # ── KPIs laterals ──
    output$kpis_lat <- renderUI({
      d <- dades_mapa()
      req(nrow(d) > 0)
      top <- d %>% slice_max(valor, n = 1, with_ties = FALSE)
      bot <- d %>% filter(valor > 0) %>% slice_min(valor, n = 1, with_ties = FALSE)
      med <- median(d$valor, na.rm = TRUE)
      rat <- if (nrow(bot) > 0 && bot$valor > 0) round(top$valor / bot$valor, 1) else NA

      kpi_sm <- function(label, val, sub = NULL, col = "#3b82f6") {
        tags$div(
          style = paste0("background:white; border-radius:10px; padding:12px 14px;",
                         "margin-bottom:10px; border-left:3px solid ", col, ";",
                         "box-shadow:0 1px 3px rgba(0,0,0,.06);"),
          tags$div(style = "font-size:10px; font-weight:700; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8; margin-bottom:4px;", label),
          tags$div(style = "font-size:16px; font-weight:800; color:#0f172a;", val),
          if (!is.null(sub)) tags$div(style = "font-size:10px; color:#94a3b8; margin-top:2px;", sub)
        )
      }

      tagList(
        kpi_sm("Més car", fmt_valor(top$valor, input$metrica),
               substr(top$nom, 1, 26), "#ef4444"),
        kpi_sm("Més econòmic", fmt_valor(bot$valor %||% NA, input$metrica),
               if (nrow(bot) > 0) substr(bot$nom, 1, 26) else "", "#10b981"),
        kpi_sm("Mediana", fmt_valor(med, input$metrica), NULL, "#8b5cf6"),
        if (!is.na(rat))
          kpi_sm("Ràtio màx/mínim", paste0(rat, "x"),
                 "diferència entre el més car i el més econòmic", "#f59e0b")
      )
    })

    # ── Rànquing ──
    output$plot_rank <- renderPlot({
      d <- dades_mapa()
      req(nrow(d) > 0)
      n   <- min(15, nrow(d))
      d_p <- d %>% arrange(desc(valor)) %>% slice_head(n = n) %>%
        mutate(
          nom   = factor(nom, levels = rev(nom)),
          rang  = row_number()
        )

      ggplot(d_p, aes(x = nom, y = valor, fill = valor)) +
        geom_col(width = 0.72, show.legend = FALSE) +
        geom_text(aes(label = mapply(fmt_valor, valor, input$metrica)),
                  hjust = -0.08, size = 3, color = "#374151", fontface = "bold") +
        scale_fill_gradientn(colors = c("#1a9641", "#ffffbf", "#d7191c"),
                             name = "Valor") +
        scale_y_continuous(expand = expansion(mult = c(0, .22)),
                           labels = label_comma(big.mark = ".")) +
        coord_flip() +
        labs(x = NULL, y = METRICA_LABEL[input$metrica],
             caption = "Font: Registre de la Propietat · Ajuntament de Barcelona") +
        tema_hab(base = 11) +
        theme(
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size = 9),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.y     = element_text(size = 9.5)
        )
    }, res = 115, bg = "white")

    # ── Evolució bretxa ──
    output$plot_evol <- renderPlot({
      req(input$metrica, input$tipus, input$nivell)
      df_ev <- df_hab %>%
        filter(nivell == input$nivell, metrica == input$metrica,
               tipus_habitatge == input$tipus, !is.na(valor), valor > 0) %>%
        add_t_num()

      df_bcn <- df_hab %>%
        filter(nivell == "barcelona", metrica == input$metrica,
               tipus_habitatge == input$tipus, !is.na(valor)) %>%
        add_t_num() %>%
        mutate(grup = "Barcelona total")

      df_top <- df_ev %>% group_by(periode, t_num) %>%
        slice_max(valor, n = 1, with_ties = FALSE) %>%
        ungroup() %>% mutate(grup = "Més car")

      df_bot <- df_ev %>% group_by(periode, t_num) %>%
        filter(valor > 0) %>% slice_min(valor, n = 1, with_ties = FALSE) %>%
        ungroup() %>% mutate(grup = "Més econòmic")

      df_all <- bind_rows(df_bcn, df_top, df_bot)

      anys_rang  <- floor(df_all$t_num)
      any_breaks <- seq(min(anys_rang, na.rm = TRUE), max(anys_rang, na.rm = TRUE), by = 2)

      col_ev <- c(
        "Barcelona total" = COLOR_BCN,
        "Més car"         = COLOR_MES_CAR,
        "Més econòmic"    = COLOR_MES_ECO
      )

      ribbon_df <- df_top %>% select(t_num, car = valor) %>%
        inner_join(df_bot %>% select(t_num, eco = valor), by = "t_num")

      ggplot(df_all, aes(x = t_num, y = valor, color = grup, group = grup)) +
        geom_ribbon(data = ribbon_df,
                    aes(x = t_num, ymin = eco, ymax = car, group = 1),
                    fill = "#f1f5f9", alpha = 0.6, inherit.aes = FALSE, na.rm = TRUE) +
        geom_line(linewidth = .8, na.rm = TRUE) +
        geom_point(size = 1.6, na.rm = TRUE) +
        scale_color_manual(values = col_ev, name = NULL) +
        scale_x_continuous(breaks = any_breaks, labels = as.character(any_breaks)) +
        scale_y_continuous(labels = label_comma(big.mark = ".")) +
        labs(
          x = NULL, y = METRICA_LABEL[input$metrica],
          caption = "Franja = rang entre el barri/districte més car i el més econòmic per trimestre"
        ) +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          legend.text     = element_text(size = 9, face = "bold"),
          axis.text.x     = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")
  })
}
