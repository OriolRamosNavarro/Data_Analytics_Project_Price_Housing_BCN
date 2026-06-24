# =============================================================================
# mod_evolucio.R — Evolució de preus, compravendes i superfície
# Millores: màx. 5 àrees, opció facetes (small multiples), llegenda fixa
# =============================================================================
library(shiny); library(ggplot2); library(dplyr); library(tidyr)
library(scales); library(ggrepel); library(shinyWidgets)

METRICA_OPTS <- c(
  "Preu per m² (EUR/m²)"             = "preu_m2",
  "Preu total (milers EUR/op.)"      = "preu_total",
  "Compravendes (nre. op.)"          = "compravendes",
  "Superfície mitjana (m²)"          = "superficie",
  "Creixement acumulat des del 2014" = "acumulat",
  "Variació anual (%)"               = "yoy"
)

mod_evolucio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Banner
    tags$div(
      class = "plot-container",
      style = "padding:14px 18px; margin-bottom:14px; background:linear-gradient(135deg,#eff6ff,#f0fdf4);",
      tags$div(style = "display:flex; gap:14px; align-items:center;",
        tags$i(class = "fa-solid fa-chart-line", style = "font-size:26px; color:#3b82f6;"),
        tags$div(
          tags$div(style = "font-size:13px; font-weight:700; color:#1e40af; margin-bottom:2px;",
                   "Com evolucionen els preus?"),
          tags$div(style = "font-size:12px; color:#3b82f6; line-height:1.5;",
            "Selecciona fins a ", tags$strong("5 àrees"), " per comparar. ",
            "Si les línies es solapen, activa ", tags$strong("Facetes"), " per veure-les separades, una per panell."
          )
        )
      )
    ),

    fluidRow(
      # ── Controls ──
      column(3,
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Mètrica"),
          selectInput(ns("metrica"), NULL, choices = METRICA_OPTS, selected = "preu_m2"),
          uiOutput(ns("ui_tipus")),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Àmbit geogràfic"),
          radioGroupButtons(ns("nivell"), NULL,
            choices = c("Tota la ciutat" = "barcelona", "Districte" = "districte", "Barri" = "barri"),
            selected = "districte", justified = TRUE, size = "sm"
          ),
          tags$div(style = "margin-top:10px;"),
          uiOutput(ns("ui_arees"))
        ),
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Rang temporal"),
          uiOutput(ns("ui_periode")),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Opcions de visualització"),
          materialSwitch(ns("mostrar_bcn"),  "Línia de referència BCN",   status = "primary", value = TRUE),
          materialSwitch(ns("facetes"),      "Facetes (panells separats)", status = "primary", value = FALSE),
          materialSwitch(ns("suavitzar"),    "Suavitzar (LOESS)",          status = "primary", value = FALSE),
          tags$div(class = "info-tip",
            tags$i(class = "fa-solid fa-lightbulb", style = "color:#f59e0b; margin-right:5px;"),
            "Consell: activa ", tags$strong("Facetes"), " quan selecciones 3 o més àrees ",
            "per evitar que les línies es solapen."
          )
        )
      ),

      # ── Contingut principal ──
      column(9,
        uiOutput(ns("kpi_row")),

        tags$div(class = "plot-container",
          tags$div(class = "plot-header",
            tags$div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
              tags$div(
                tags$div(class = "plot-title", uiOutput(ns("plot_titol"))),
                tags$div(class = "plot-subtitle", uiOutput(ns("plot_subtitol")))
              ),
              uiOutput(ns("badge_metrica"))
            )
          ),
          uiOutput(ns("plot_height_ui")),
          tags$div(class = "plot-footer",
            "Font: Registre de la Propietat · Ajuntament de Barcelona · Elaboració pròpia"
          )
        ),

        tags$details(
          tags$summary(
            style = "cursor:pointer; font-size:13px; font-weight:600; color:#374151; padding:14px 18px; background:white; border-radius:14px; box-shadow:0 1px 4px rgba(0,0,0,.07); margin-bottom:4px; list-style:none; display:flex; align-items:center; gap:8px;",
            tags$i(class = "fa-solid fa-table", style = "color:#94a3b8;"),
            "Dades trimestrals detallades",
            tags$span(style = "font-size:11px; color:#94a3b8; font-weight:400; margin-left:auto;", "Clica per expandir")
          ),
          tags$div(class = "plot-container", style = "margin-top:4px; border-radius:0 0 14px 14px;",
            tags$div(class = "plot-header",
              tags$div(class = "plot-subtitle", "Format ample — una columna per àrea seleccionada")
            ),
            DT::dataTableOutput(ns("taula"))
          )
        )
      )
    )
  )
}

mod_evolucio_server <- function(id, df_hab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    periodes_all <- get_periodes(df_hab)

    es_derivada <- reactive({ input$metrica %in% c("acumulat", "yoy") })

    output$ui_tipus <- renderUI({
      if (es_derivada()) return(NULL)
      opts <- TIPUS_PER_METRICA[[req(input$metrica)]]
      selectInput(ns("tipus"), "Tipus d'habitatge",
                  choices = setNames(opts, TIPUS_LABEL[opts]), selected = opts[1])
    })

    output$ui_arees <- renderUI({
      req(input$nivell)
      if (input$nivell == "barcelona") return(
        tags$div(class = "info-tip",
          tags$i(class = "fa-solid fa-city", style = "color:#3b82f6; margin-right:5px;"),
          "Mostrant les dades agregades de tota Barcelona."
        )
      )
      noms <- get_noms(df_hab, input$nivell)
      def  <- if (input$nivell == "districte") head(noms, 5)
              else head(intersect(c("la Dreta de l'Eixample", "el Raval", "Sarrià",
                                    "la Guineueta", "el Poblenou"), noms), 5)
      tagList(
        pickerInput(ns("noms_sel"), "Àrees (màx. 5)", choices = noms, selected = head(def, 5),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE, `live-search` = TRUE,
            `selected-text-format` = "count > 3",
            `count-selected-text` = "{0} àrees",
            `max-options` = 5,
            `max-options-text` = "Màxim 5 àrees"
          )
        ),
        uiOutput(ns("ui_arees_aviso"))
      )
    })

    output$ui_arees_aviso <- renderUI({
      req(input$noms_sel)
      n <- length(input$noms_sel)
      if (n >= 3 && !isTRUE(input$facetes)) {
        tags$div(class = "info-tip", style = "margin-top:8px;",
          tags$i(class = "fa-solid fa-triangle-exclamation", style = "color:#f59e0b; margin-right:5px;"),
          paste0("Tens ", n, " àrees seleccionades. Considera activar les Facetes per millorar la llegibilitat.")
        )
      }
    })

    output$ui_periode <- renderUI({
      sliderTextInput(ns("rang"), NULL, choices = periodes_all,
                      selected = c(periodes_all[1], tail(periodes_all, 1)), grid = FALSE)
    })

    noms_sel <- reactive({
      if (input$nivell == "barcelona") return("Barcelona")
      req(input$noms_sel)
      input$noms_sel
    })

    periodes_sel <- reactive({
      req(input$rang)
      periodes_all[periodes_all >= input$rang[1] & periodes_all <= input$rang[2]]
    })

    dades_base <- reactive({
      req(input$metrica, input$nivell)
      met_real <- if (es_derivada()) "preu_m2" else input$metrica
      tip      <- if (es_derivada() || is.null(input$tipus)) "total" else input$tipus
      noms     <- noms_sel()
      niv      <- input$nivell

      df <- filter_serie(df_hab, niv, noms, met_real, tip) %>%
        filter(periode %in% periodes_sel())

      if (input$metrica == "acumulat") {
        df <- df %>% group_by(nom) %>%
          mutate(valor = valor / first(na.omit(valor)) * 100) %>% ungroup()
      } else if (input$metrica == "yoy") {
        df <- df %>% arrange(nom, any, trimestre) %>%
          group_by(nom, trimestre) %>%
          mutate(valor = (valor / lag(valor) - 1) * 100) %>% ungroup() %>%
          filter(!is.na(valor))
      }
      df
    })

    dades_bcn <- reactive({
      if (!isTRUE(input$mostrar_bcn) || input$nivell == "barcelona") return(NULL)
      met_real <- if (es_derivada()) "preu_m2" else input$metrica
      tip      <- if (es_derivada() || is.null(input$tipus)) "total" else input$tipus
      df <- filter_serie(df_hab, "barcelona", "Barcelona", met_real, tip) %>%
        filter(periode %in% periodes_sel()) %>%
        mutate(nom = "BCN total")
      if (input$metrica == "acumulat")
        df <- df %>% mutate(valor = valor / first(na.omit(valor)) * 100)
      if (input$metrica == "yoy")
        df <- df %>% arrange(any, trimestre) %>%
          mutate(valor = (valor / lag(valor) - 1) * 100) %>% filter(!is.na(valor))
      df
    })

    # ── KPIs ──
    output$kpi_row <- renderUI({
      df   <- dades_base()
      req(nrow(df) > 0)
      noms <- unique(df$nom)
      n    <- min(length(noms), 5)

      cards <- lapply(seq_len(n), function(i) {
        nom_i <- noms[i]
        s <- df %>% filter(nom == nom_i) %>% arrange(periode) %>% pull(valor) %>% na.omit()
        if (length(s) < 1) return(NULL)
        v_fi  <- last(s); v_ini <- first(s)
        delta_num <- if (!is.na(v_ini) && v_ini != 0) (v_fi - v_ini) / abs(v_ini) * 100 else NA
        delta_str <- if (!is.na(delta_num)) sprintf("%+.1f%%", delta_num) else "—"
        delta_col <- if (!is.na(delta_num) && delta_num >= 0) "#ef4444" else "#10b981"

        val_principal <- switch(input$metrica,
          preu_m2      = format(round(v_fi), big.mark = "."),
          preu_total   = format(round(v_fi, 1), big.mark = "."),
          compravendes = format(round(v_fi), big.mark = "."),
          superficie   = as.character(round(v_fi, 1)),
          acumulat     = as.character(round(v_fi, 1)),
          yoy          = sprintf("%+.1f", v_fi),
          as.character(round(v_fi, 1))
        )
        val_unitat <- switch(input$metrica,
          preu_m2 = "EUR/m²", preu_total = "kEUR",
          superficie = "m²", acumulat = "índex", yoy = "%", ""
        )

        col_accent <- PALETTE_MAIN[(i - 1) %% length(PALETTE_MAIN) + 1]
        period_lbl <- paste0(head(periodes_sel(), 1), " → ", tail(periodes_sel(), 1))

        tags$div(
          style = paste0(
            "background:white; border-radius:12px; padding:14px 16px;",
            "border-top:3px solid ", col_accent, ";",
            "box-shadow:0 1px 3px rgba(0,0,0,.07); min-width:0; overflow:hidden;"
          ),
          tags$div(style = "font-size:10px; font-weight:700; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8; margin-bottom:6px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                   nom_i),
          tags$div(style = "display:flex; align-items:baseline; gap:5px; flex-wrap:wrap;",
            tags$span(style = "font-size:22px; font-weight:800; color:#0f172a; letter-spacing:-.8px; line-height:1;", val_principal),
            tags$span(style = "font-size:11px; font-weight:600; color:#94a3b8;", val_unitat)
          ),
          tags$div(style = paste0("font-size:12px; font-weight:700; color:", delta_col, "; margin-top:4px;"), delta_str),
          tags$div(style = "font-size:10px; color:#cbd5e1; margin-top:3px;", period_lbl)
        )
      })

      n_cards <- length(Filter(Negate(is.null), cards))
      tags$div(
        style = paste0("display:grid; grid-template-columns:repeat(", n_cards, ",minmax(0,1fr)); gap:12px; margin-bottom:16px;"),
        tagList(Filter(Negate(is.null), cards))
      )
    })

    output$plot_titol <- renderUI({
      req(input$metrica)
      names(METRICA_OPTS)[METRICA_OPTS == input$metrica]
    })
    output$plot_subtitol <- renderUI({
      niv <- c(barcelona = "Tota la ciutat", districte = "Districtes", barri = "Barris")[input$nivell]
      tip <- if (es_derivada() || is.null(input$tipus)) "Total" else TIPUS_LABEL[input$tipus]
      mode <- if (isTRUE(input$facetes)) " · Facetes actives" else ""
      paste0(niv, " · ", tip, " · ", head(periodes_sel(), 1), " – ", tail(periodes_sel(), 1), mode)
    })
    output$badge_metrica <- renderUI({
      if (!es_derivada()) return(NULL)
      txt <- if (input$metrica == "acumulat") "Base preu/m² · índex 100 = inici"
             else "Comparant vs. mateix trimestre l'any anterior"
      tags$div(style = "background:#fef3c7; color:#92400e; border-radius:20px; padding:4px 12px; font-size:11px; font-weight:600; white-space:nowrap;", txt)
    })

    # Alçada dinàmica del plot: facetes → més alta
    plot_h <- reactive({
      n <- length(noms_sel())
      if (isTRUE(input$facetes) && n > 1) pmax(280 * ceiling(n / 2), 400) else 390
    })

    output$plot_height_ui <- renderUI({
      plotOutput(ns("plot_main"), height = paste0(plot_h(), "px"))
    })

    # ── Gràfic principal ──
    output$plot_main <- renderPlot({
      df   <- dades_base() %>% add_t_num()
      df_b <- if (!is.null(dades_bcn())) dades_bcn() %>% add_t_num() else NULL
      req(nrow(df) > 0)

      noms_u <- unique(df$nom)
      n      <- length(noms_u)

      # ── Paleta coherent ──
      # Districtes: sempre COLORS_DIST. Barris: PALETTE_MAIN (exclou negre).
      # BCN total: sempre COLOR_BCN (negre).
      if (input$nivell == "districte") {
        col_map <- sapply(noms_u, color_districte, USE.NAMES = TRUE)
        names(col_map) <- noms_u
      } else if (input$nivell == "barcelona") {
        col_map <- setNames(COLOR_BCN, "Barcelona")
      } else {
        pal_barri <- PALETTE_MAIN[-1]   # treu el negre (reservat per BCN)
        col_map   <- setNames(rep_len(pal_barri, n), noms_u)
      }

      y_fmt <- switch(input$metrica,
        preu_m2      = label_comma(big.mark = ".", suffix = " EUR/m²"),
        preu_total   = label_comma(big.mark = ".", suffix = " kEUR"),
        compravendes = label_comma(big.mark = "."),
        superficie   = label_comma(big.mark = ".", suffix = " m²"),
        acumulat     = label_number(suffix = " ix", accuracy = .1),
        yoy          = label_number(suffix = "%", accuracy = .1),
        label_comma()
      )

      # Breaks un per any, labels "2014", "2015"...
      anys_rang  <- floor(periode_a_num(periodes_sel()))
      any_breaks <- seq(min(anys_rang), max(anys_rang), by = 1)

      usar_facetes <- isTRUE(input$facetes) && n > 1

      if (usar_facetes) {
        p <- ggplot(df, aes(x = t_num, y = valor, color = nom, group = nom)) +
          { if (input$metrica == "yoy") geom_hline(yintercept = 0, color = "#e2e8f0", linewidth = .5, linetype = "dashed") } +
          geom_line(linewidth = .7, na.rm = TRUE) +
          geom_point(size = 1.5, na.rm = TRUE) +
          { if (isTRUE(input$suavitzar))
              geom_smooth(method = "loess", se = FALSE, linewidth = .5, linetype = "dotted", na.rm = TRUE) } +
          scale_color_manual(values = col_map, name = "Àrea") +
          scale_x_continuous(breaks = any_breaks, labels = as.character(any_breaks)) +
          scale_y_continuous(labels = y_fmt, expand = expansion(mult = c(.05, .12))) +
          facet_wrap(~ nom, ncol = 2, scales = "free_y") +
          labs(x = NULL, y = NULL,
               caption = "Font: Registre de la Propietat · Ajuntament de Barcelona") +
          tema_hab() +
          theme(
            legend.position  = "none",
            strip.text       = element_text(size = 11, face = "bold", color = "#0f172a"),
            strip.background = element_rect(fill = "#f8fafc", color = NA),
            panel.spacing    = unit(14, "pt"),
            axis.text.x      = element_text(angle = 45, hjust = 1)
          )

        if (!is.null(df_b) && nrow(df_b) > 0) {
          p <- p + geom_line(
            data = df_b %>% slice(rep(seq_len(nrow(df_b)), n)),
            aes(x = t_num, y = valor, group = 1),
            color = COLOR_BCN, linewidth = .6, linetype = "dashed", inherit.aes = FALSE
          )
        }

      } else {
        df_lab <- df %>% group_by(nom) %>%
          slice_max(order_by = t_num, n = 1, with_ties = FALSE) %>%
          ungroup() %>% filter(!is.na(valor))

        p <- ggplot(df, aes(x = t_num, y = valor, color = nom, group = nom)) +
          { if (input$metrica == "yoy") geom_hline(yintercept = 0, color = "#e2e8f0", linewidth = .6, linetype = "dashed") } +
          geom_line(linewidth = .7, na.rm = TRUE) +
          geom_point(size = 1.6, na.rm = TRUE) +
          { if (isTRUE(input$suavitzar))
              geom_smooth(method = "loess", se = FALSE, linewidth = .4, linetype = "dotted", na.rm = TRUE) } +
          scale_color_manual(values = col_map, name = "Àrea") +
          scale_x_continuous(breaks = any_breaks, labels = as.character(any_breaks)) +
          scale_y_continuous(labels = y_fmt, expand = expansion(mult = c(.05, .15))) +
          geom_text_repel(
            data = df_lab, aes(label = nom), size = 3, fontface = "bold",
            direction = "y", nudge_x = .3, segment.size = .25,
            segment.color = "#e2e8f0", max.overlaps = 10
          ) +
          labs(x = NULL, y = NULL,
               caption = "Font: Registre de la Propietat · Ajuntament de Barcelona") +
          tema_hab() +
          theme(
            legend.position = "bottom",
            legend.title    = element_text(size = 10, face = "bold"),
            legend.text     = element_text(size = 9),
            plot.margin     = margin(8, 50, 8, 8),
            axis.text.x     = element_text(angle = 45, hjust = 1)
          ) +
          guides(color = guide_legend(nrow = 2, override.aes = list(size = 2.5, linewidth = 1.2)))

        if (!is.null(df_b) && nrow(df_b) > 0) {
          df_b_lab <- df_b %>% slice_max(order_by = t_num, n = 1, with_ties = FALSE)
          p <- p +
            geom_line(data = df_b,
                      aes(x = t_num, y = valor, group = nom),
                      color = COLOR_BCN, linewidth = 1, linetype = "dashed",
                      inherit.aes = FALSE) +
            geom_text_repel(
              data = df_b_lab,
              aes(x = t_num, y = valor, label = "BCN total"),
              color = COLOR_BCN, size = 3, fontface = "bold.italic",
              inherit.aes = FALSE, nudge_x = .3, direction = "y",
              segment.color = "#cbd5e1"
            )
        }
      }
      p
    }, res = 115, bg = "white", height = function() plot_h())

    # ── Taula ──
    output$taula <- DT::renderDataTable({
      df <- dades_base()
      req(nrow(df) > 0)
      df_w <- df %>% select(nom, periode, valor) %>%
        mutate(valor = round(valor, 2)) %>%
        pivot_wider(names_from = nom, values_from = valor) %>%
        arrange(periode)
      DT::datatable(df_w, rownames = FALSE,
        options = list(
          pageLength = 12, scrollX = TRUE, dom = "ftp",
          language = list(
            search = "Cerca:", info = "Mostrant _START_-_END_ de _TOTAL_",
            paginate = list(previous = "Ant.", `next` = "Seg.")
          )
        )
      ) %>% DT::formatRound(2:ncol(df_w), digits = 1)
    })
  })
}
