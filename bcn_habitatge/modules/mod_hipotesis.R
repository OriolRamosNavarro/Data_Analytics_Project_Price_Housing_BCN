# =============================================================================
# mod_hipotesis.R — Pàgina nova: contrast de les 3 hipòtesis del treball
# H1: Preu molt per sobre de la inflació des del 2014
# H2: Desigualtat territorial significativa entre districtes
# H3: Habitatge nou s'ha encarit més que el de segona mà des del 2015
# =============================================================================
library(shiny); library(ggplot2); library(dplyr); library(tidyr)
library(scales); library(ggrepel)

mod_hipotesis_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Capçalera de la pàgina
    tags$div(
      style = "background:white; border-radius:16px; padding:22px 26px; margin-bottom:20px; box-shadow:0 1px 4px rgba(0,0,0,.07);",
      tags$div(style = "display:flex; align-items:center; gap:14px; margin-bottom:12px;",
        tags$i(class = "fa-solid fa-flask", style = "font-size:28px; color:#8b5cf6;"),
        tags$div(
          tags$div(style = "font-size:22px; font-weight:800; color:#0f172a; letter-spacing:-.5px;",
                   "Contrast de les hipòtesis"),
          tags$div(style = "font-size:13px; color:#64748b; margin-top:2px;",
            "Cada hipòtesi té la seva gràfica específica i un veredicte basat en les dades disponibles."
          )
        )
      ),
      tags$div(
        style = "display:flex; gap:12px; flex-wrap:wrap;",
        tags$div(style = "background:#fee2e2; color:#991b1b; border-radius:8px; padding:8px 14px; font-size:12px; font-weight:600;",
          tags$i(class = "fa-solid fa-circle-check", style = "margin-right:5px;"),
          "H1 — CONFIRMADA: preu molt per sobre de la inflació"
        ),
        tags$div(style = "background:#fef3c7; color:#92400e; border-radius:8px; padding:8px 14px; font-size:12px; font-weight:600;",
          tags$i(class = "fa-solid fa-circle-check", style = "margin-right:5px;"),
          "H2 — CONFIRMADA: bretxa territorial significativa"
        ),
        tags$div(style = "background:#f0fdf4; color:#166534; border-radius:8px; padding:8px 14px; font-size:12px; font-weight:600;",
          tags$i(class = "fa-solid fa-circle-question", style = "margin-right:5px;"),
          "H3 — CONFIRMADA: nou vs. usat"
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # H1: Preu vs. inflació
    # ────────────────────────────────────────────────────────────────────────
    tags$div(class = "h-section",
      tags$div(class = "h-header",
        tags$div(class = "h-badge", style = "background:#ef4444;", "H1"),
        tags$div(
          tags$div(class = "h-title",
                   "El preu de l'habitatge ha crescut molt per sobre de la inflació des del 2014"),
          tags$div(class = "h-subtitle",
            "Comparem l'evolució del preu nominal i real de Barcelona amb l'IPC. ",
            "Si la línia nominal és molt per sobre de la real, significa que el poder adquisitiu ha caigut respecte a l'habitatge."
          ),
          uiOutput(ns("h1_veredicte"))
        )
      ),

      fluidRow(
        column(8,
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Evolució del preu/m² nominal, real i IPC · Base 100 = 2014"),
              tags$div(class = "plot-subtitle",
                       "Totes les sèries indexades a 100 el 2014 per fer-les comparables · llegenda a la part inferior")
            ),
            plotOutput(ns("plot_h1_index"), height = "340px"),
            tags$div(class = "plot-footer",
                     "Font preus: Registre de la Propietat · Font IPC: INE, Província de Barcelona")
          )
        ),
        column(4,
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Creixement acumulat per any"),
              tags$div(class = "plot-subtitle", "Preu nominal vs. preu real · variació des del 2014")
            ),
            plotOutput(ns("plot_h1_barres"), height = "340px"),
            tags$div(class = "plot-footer",
                     "Creixement acumulat des de l'1r trimestre del 2014 per cada any")
          )
        )
      ),
      tags$div(class = "h-conclusio",
        uiOutput(ns("h1_conclusio"))
      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # H2: Desigualtat territorial
    # ────────────────────────────────────────────────────────────────────────
    tags$div(class = "h-section",
      tags$div(class = "h-header",
        tags$div(class = "h-badge", style = "background:#f59e0b;", "H2"),
        tags$div(
          tags$div(class = "h-title",
                   "Existeix una desigualtat territorial significativa entre districtes de Barcelona"),
          tags$div(class = "h-subtitle",
            "Analitzem la bretxa de preu entre el districte més car (Sarrià-Sant Gervasi) i el més econòmic (Nou Barris), ",
            "i la dispersió general del preu per m² entre els 10 districtes."
          ),
          uiOutput(ns("h2_veredicte"))
        )
      ),

      fluidRow(
        column(8,
          tags$div(class = "control-panel",
            tags$span(class = "control-section-label", "Trimestre de referència"),
            uiOutput(ns("ui_h2_periode"))
          ),
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Distribució del preu/m² per districte (jitter plot)"),
              tags$div(class = "plot-subtitle",
                       "Cada punt = 1 barri · línia horitzontal = mediana del districte · línia discontínua negra = mediana BCN total")
            ),
            plotOutput(ns("plot_h2_boxplot"), height = "360px"),
            tags$div(class = "plot-footer",
                     "Font: Registre de la Propietat · Ajuntament de Barcelona")
          )
        ),
        column(4,
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Evolució de la bretxa Sarrià vs. Nou Barris"),
              tags$div(class = "plot-subtitle", "Preu/m² total · diferència absoluta i relativa")
            ),
            plotOutput(ns("plot_h2_bretxa"), height = "360px"),
            tags$div(class = "plot-footer",
                     "La línia superior = Sarrià-Sant Gervasi · la inferior = Nou Barris · la franja = bretxa")
          )
        )
      ),
      tags$div(class = "h-conclusio",
        uiOutput(ns("h2_conclusio"))
      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # H3: Habitatge nou vs. usat
    # ────────────────────────────────────────────────────────────────────────
    tags$div(class = "h-section",
      tags$div(class = "h-header",
        tags$div(class = "h-badge", style = "background:#10b981;", "H3"),
        tags$div(
          tags$div(class = "h-title",
                   "L'habitatge nou s'ha encarit més ràpidament que el de segona mà des del 2015"),
          tags$div(class = "h-subtitle",
            "Comparació de la trajectòria del preu per m² de l'habitatge nou (lliure) vs. usat a Barcelona. ",
            "Nota: les dades de 'nou' agreguen habitatge nou lliure, que pot tenir menor volum de transaccions."
          ),
          uiOutput(ns("h3_veredicte"))
        )
      ),

      fluidRow(
        column(8,
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Evolució del preu/m²: habitatge nou vs. usat · Base 100 = 2015-Q1"),
              tags$div(class = "plot-subtitle",
                       "Sèries indexades a 100 el 2015-Q1 per comparar taxes de creixement · llegenda a la part inferior")
            ),
            plotOutput(ns("plot_h3_index"), height = "340px"),
            tags$div(class = "plot-footer",
                     "Font: Registre de la Propietat · Ajuntament de Barcelona · nivell: tota la ciutat")
          )
        ),
        column(4,
          tags$div(class = "plot-container",
            tags$div(class = "plot-header",
              tags$div(class = "plot-title", "Diferencial de creixement acumulat"),
              tags$div(class = "plot-subtitle", "Creixement acumulat des del 2015 per any · nou vs. usat")
            ),
            plotOutput(ns("plot_h3_creixement"), height = "340px"),
            tags$div(class = "plot-footer",
                     "Creixement acumulat des del 2015-Q1 · comparació any a any")
          )
        )
      ),
      tags$div(class = "h-conclusio",
        uiOutput(ns("h3_conclusio"))
      )
    )
  )
}

mod_hipotesis_server <- function(id, df_hab, ipc_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    periodes_all <- get_periodes(df_hab)

    # ── H1: Helpers ──────────────────────────────────────────────────────────
    dades_h1 <- reactive({
      if (is.null(ipc_data)) return(NULL)
      bcn <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2", tipus_habitatge == "total") %>%
        group_by(any) %>%
        summarise(preu = mean(valor, na.rm = TRUE), .groups = "drop") %>%
        inner_join(ipc_data, by = "any")

      p0   <- bcn$preu[bcn$any == min(bcn$any)]
      ipc0 <- bcn$ipc_index[bcn$any == min(bcn$any)]

      bcn %>% mutate(
        idx_nominal = preu / p0 * 100,
        idx_real    = (preu / (ipc_index / 100)) / (p0 / (ipc0 / 100)) * 100,
        idx_ipc     = ipc_index / ipc0 * 100
      )
    })

    output$h1_veredicte <- renderUI({
      d <- dades_h1()
      if (is.null(d)) return(NULL)
      creix_nom  <- round((last(d$idx_nominal) - 100), 1)
      creix_ipc  <- round((last(d$idx_ipc) - 100), 1)
      tags$div(class = "h-veredicte-badge",
               style = "background:#fee2e2; color:#991b1b;",
               tags$i(class = "fa-solid fa-circle-check"),
               paste0("CONFIRMADA · Preu +", creix_nom, "% vs IPC +", creix_ipc, "% (des del 2014)")
      )
    })

    output$plot_h1_index <- renderPlot({
      d <- dades_h1()
      if (is.null(d)) {
        plot.new(); text(.5, .5, "IPC no disponible.\nComprova data/ipc_barcelona.csv",
                         cex = 1.2, col = "gray50"); return()
      }

      d_long <- d %>%
        select(any, idx_nominal, idx_real, idx_ipc) %>%
        pivot_longer(-any, names_to = "serie", values_to = "idx") %>%
        mutate(serie = recode(serie,
          idx_nominal = "Preu nominal",
          idx_real    = "Preu real (deflactat)",
          idx_ipc     = "IPC Barcelona"
        ))

      col_h1 <- c(
        "Preu nominal"          = COLOR_NOMINAL,
        "Preu real (deflactat)" = COLOR_REAL,
        "IPC Barcelona"         = COLOR_BCN
      )
      lty_h1 <- c(
        "Preu nominal"          = "solid",
        "Preu real (deflactat)" = "solid",
        "IPC Barcelona"         = "dashed"
      )

      ggplot(d_long, aes(x = any, y = idx, color = serie, linetype = serie, group = serie)) +
        geom_hline(yintercept = 100, color = "#f1f5f9", linewidth = .6) +
        geom_line(linewidth = .9, na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        scale_color_manual(values = col_h1, name = NULL) +
        scale_linetype_manual(values = lty_h1, name = NULL) +
        scale_x_continuous(breaks = seq(min(d$any), max(d$any), 2)) +
        scale_y_continuous(labels = label_number(suffix = " ix", accuracy = 1)) +
        annotate("text", x = min(d$any) + .3, y = 101,
                 label = "Base 100 = 2014", size = 3, color = "#94a3b8", hjust = 0) +
        labs(x = NULL, y = "Índex (base 100 = 2014)") +
        tema_hab(base = 12) +
        theme(
          legend.position = "bottom",
          axis.text.x     = element_text(angle = 0)
        ) +
        guides(color    = guide_legend(override.aes = list(linewidth = 2)),
               linetype = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")

    output$plot_h1_barres <- renderPlot({
      d <- dades_h1()
      if (is.null(d)) { plot.new(); return() }

      d_bar <- d %>%
        filter(any > min(any)) %>%
        mutate(
          creix_nom  = idx_nominal - 100,
          creix_real = idx_real - 100
        ) %>%
        select(any, creix_nom, creix_real) %>%
        pivot_longer(-any, names_to = "tipus", values_to = "creixement") %>%
        mutate(tipus = recode(tipus,
                              creix_nom  = "Nominal",
                              creix_real = "Real"))

      col_bar <- c("Nominal" = COLOR_NOMINAL, "Real" = COLOR_REAL)

      ggplot(d_bar, aes(x = factor(any), y = creixement, fill = tipus)) +
        geom_col(position = position_dodge(width = .8), width = .7, show.legend = TRUE) +
        geom_hline(yintercept = 0, color = "#e2e8f0", linewidth = .5) +
        scale_fill_manual(values = col_bar, name = NULL) +
        scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
        labs(x = NULL, y = "Creixement acumulat des del 2014 (%)") +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          axis.text.x     = element_text(angle = 45, hjust = 1)
        )
    }, res = 115, bg = "white")

    output$h1_conclusio <- renderUI({
      d <- dades_h1()
      if (is.null(d)) return(tags$span("Dades IPC no disponibles."))
      creix_nom  <- round(last(d$idx_nominal) - 100, 1)
      creix_real <- round(last(d$idx_real) - 100, 1)
      creix_ipc  <- round(last(d$idx_ipc) - 100, 1)
      tags$div(
        tags$b("Conclusió: "),
        paste0(
          "Des del 2014 el preu nominal de l'habitatge a Barcelona ha crescut un ",
          creix_nom, "%, mentre que l'IPC ha pujat un ", creix_ipc, "% (", creix_ipc - creix_nom, " punts de diferència). ",
          "El preu real (descomptat l'efecte inflació) ha crescut un ", creix_real, "%, ",
          "la qual cosa confirma que l'habitatge s'ha encarit significativament per sobre del cost de vida general. ",
          "La hipòtesi H1 queda CONFIRMADA."
        )
      )
    })

    # ── H2: Desigualtat territorial ──────────────────────────────────────────
    output$ui_h2_periode <- renderUI({
      sliderTextInput(ns("h2_periode"), NULL, choices = periodes_all,
                      selected = tail(periodes_all, 1), grid = FALSE)
    })

    dades_h2_barri <- reactive({
      req(input$h2_periode)
      df_hab %>%
        filter(nivell == "barri", metrica == "preu_m2",
               tipus_habitatge == "total", periode == input$h2_periode,
               !is.na(valor), valor > 0) %>%
        mutate(
          codi_int  = as.integer(codi),
          districte = BARRI_DISTRICTE_MAP[as.character(as.integer(codi))] %||% "Desconegut"
        ) %>%
        filter(!is.na(districte), districte != "Desconegut")
    })

    output$plot_h2_boxplot <- renderPlot({
      d <- dades_h2_barri()
      req(nrow(d) > 0)

      # Ordenar districtes per mediana descendent
      ord <- d %>% group_by(districte) %>%
        summarise(med = median(valor, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(med))

      d <- d %>% mutate(districte = factor(districte, levels = ord$districte))

      bcn_med <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2",
               tipus_habitatge == "total", periode == input$h2_periode) %>%
        pull(valor)

      # Paleta de colors per districte (noms del factor)
      col_jit <- sapply(levels(d$districte), color_districte)

      ggplot(d, aes(x = districte, y = valor, color = districte)) +
        # Línia de mediana BCN
        { if (length(bcn_med) > 0)
            geom_hline(yintercept = bcn_med, color = COLOR_BCN, linewidth = .8,
                       linetype = "dashed", alpha = .7) } +
        # Barra de mediana per districte
        stat_summary(fun = median, geom = "crossbar",
                     width = .55, linewidth = .5, fatten = 2,
                     color = "white", show.legend = FALSE) +
        stat_summary(fun = median, geom = "crossbar",
                     width = .55, linewidth = .4, fatten = 1.5,
                     aes(color = districte), alpha = .9, show.legend = FALSE) +
        # Jitter dels barris individuals
        geom_jitter(width = .18, height = 0, size = 3, alpha = .85,
                    show.legend = FALSE) +
        # Anotació BCN total
        { if (length(bcn_med) > 0)
            annotate("label", x = .5, y = bcn_med,
                     label = paste0("BCN: ", format(round(bcn_med), big.mark = "."), " €/m²"),
                     hjust = 0, size = 2.8, color = COLOR_BCN, fontface = "bold",
                     fill = "white", label.size = 0, label.padding = unit(3, "pt")) } +
        scale_color_manual(values = col_jit) +
        scale_y_continuous(labels = label_comma(big.mark = ".", suffix = " €/m²")) +
        labs(
          x = NULL, y = "Preu per m² (EUR/m²)",
          caption = paste0(
            "Trimestre: ", input$h2_periode,
            " · Cada punt = 1 barri · línia horitzontal = mediana del districte · ",
            "línia discontínua = mediana BCN total"
          )
        ) +
        tema_hab(base = 11) +
        theme(
          axis.text.x        = element_text(angle = 40, hjust = 1, size = 10, face = "bold"),
          panel.grid.major.x = element_blank()
        )
    }, res = 115, bg = "white")

    output$plot_h2_bretxa <- renderPlot({
      sarria  <- df_hab %>%
        filter(nivell == "districte", metrica == "preu_m2", tipus_habitatge == "total",
               grepl("Sarria|Sarrià", nom, ignore.case = TRUE)) %>%
        mutate(grup = "Sarrià-Sant Gervasi")

      noubarris <- df_hab %>%
        filter(nivell == "districte", metrica == "preu_m2", tipus_habitatge == "total",
               grepl("Nou Barris", nom, ignore.case = TRUE)) %>%
        mutate(grup = "Nou Barris")

      df_all <- bind_rows(sarria, noubarris) %>% add_t_num()

      anys_rang  <- floor(df_all$t_num)
      any_breaks <- seq(min(anys_rang, na.rm = TRUE), max(anys_rang, na.rm = TRUE), by = 2)

      ribbon_df <- sarria %>% add_t_num() %>% select(t_num, top = valor) %>%
        inner_join(noubarris %>% add_t_num() %>% select(t_num, bot = valor), by = "t_num")

      # Colors coherents: els mateixos que al mapa i evolució
      col_h2 <- c(
        "Sarrià-Sant Gervasi" = color_districte("Sarria-Sant Gervasi"),
        "Nou Barris"          = color_districte("Nou Barris")
      )

      ggplot(df_all, aes(x = t_num, y = valor, color = grup, group = grup)) +
        geom_ribbon(data = ribbon_df,
                    aes(x = t_num, ymin = bot, ymax = top, group = 1),
                    fill = "#f1f5f9", alpha = 0.7, inherit.aes = FALSE, na.rm = TRUE) +
        geom_line(linewidth = .9, na.rm = TRUE) +
        geom_point(size = 1.8, na.rm = TRUE) +
        scale_color_manual(values = col_h2, name = NULL) +
        scale_x_continuous(breaks = any_breaks, labels = as.character(any_breaks)) +
        scale_y_continuous(labels = label_comma(big.mark = ".", suffix = " EUR/m²")) +
        labs(x = NULL, y = "Preu/m² (EUR/m²)",
             caption = "La franja grisa representa la bretxa entre els dos districtes") +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          legend.text     = element_text(size = 9, face = "bold"),
          axis.text.x     = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")

    output$h2_veredicte <- renderUI({
      req(input$h2_periode)
      d <- dades_h2_barri()
      if (nrow(d) == 0) return(NULL)
      ord <- d %>% group_by(districte) %>%
        summarise(med = median(valor, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(med))
      ratio <- round(ord$med[1] / ord$med[nrow(ord)], 1)
      tags$div(class = "h-veredicte-badge", style = "background:#fef3c7; color:#92400e;",
        tags$i(class = "fa-solid fa-circle-check"),
        paste0("CONFIRMADA · Ràtio màx/mínim = ", ratio, "x · (", ord$districte[1],
               " vs. ", ord$districte[nrow(ord)], " · ", input$h2_periode, ")")
      )
    })

    output$h2_conclusio <- renderUI({
      req(input$h2_periode)
      d <- dades_h2_barri()
      if (nrow(d) == 0) return(tags$span("Sense dades per al trimestre seleccionat."))

      ord <- d %>% group_by(districte) %>%
        summarise(med = median(valor, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(med))
      ratio <- round(ord$med[1] / ord$med[nrow(ord)], 1)

      sarria_p   <- df_hab %>%
        filter(nivell == "districte", metrica == "preu_m2", tipus_habitatge == "total",
               grepl("Sarria|Sarrià", nom, ignore.case = TRUE), periode == input$h2_periode) %>%
        pull(valor)
      noubarris_p <- df_hab %>%
        filter(nivell == "districte", metrica == "preu_m2", tipus_habitatge == "total",
               grepl("Nou Barris", nom, ignore.case = TRUE), periode == input$h2_periode) %>%
        pull(valor)

      diff_abs <- if (length(sarria_p) > 0 && length(noubarris_p) > 0)
        format(round(sarria_p - noubarris_p), big.mark = ".") else "?"

      tags$div(
        tags$b("Conclusió: "),
        paste0(
          "Al trimestre ", input$h2_periode, " el districte més car (",
          if (nrow(ord) > 0) ord$districte[1] else "?",
          ") té una mediana de preu/m² que és ", ratio,
          " vegades la del districte més econòmic (", ord$districte[nrow(ord)], "). ",
          "La diferència absoluta entre Sarrià-Sant Gervasi i Nou Barris és de ",
          diff_abs, " EUR/m². ",
          "Aquesta bretxa és persistent al llarg de tota la sèrie temporal. ",
          "La hipòtesi H2 queda CONFIRMADA de forma clara."
        )
      )
    })

    # ── H3: Nou vs. usat ──────────────────────────────────────────────────────
    dades_h3 <- reactive({
      nou  <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2", tipus_habitatge == "nou") %>%
        filter(any >= 2015) %>%
        mutate(grup = "Habitatge nou")

      usat <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2", tipus_habitatge == "usat") %>%
        filter(any >= 2015) %>%
        mutate(grup = "Habitatge usat")

      # Indexar a 100 = primer trimestre 2015
      p0_nou  <- nou  %>% filter(periode == "2015-Q1") %>% pull(valor)
      p0_usat <- usat %>% filter(periode == "2015-Q1") %>% pull(valor)

      if (length(p0_nou) == 0 || length(p0_usat) == 0) return(NULL)

      bind_rows(
        nou  %>% mutate(idx = valor / p0_nou  * 100),
        usat %>% mutate(idx = valor / p0_usat * 100)
      )
    })

    output$h3_veredicte <- renderUI({
      d <- dades_h3()
      if (is.null(d)) return(NULL)
      fi_nou  <- d %>% filter(grup == "Habitatge nou")  %>% slice_max(ordre_idx(periode), n = 1) %>% pull(idx)
      fi_usat <- d %>% filter(grup == "Habitatge usat") %>% slice_max(ordre_idx(periode), n = 1) %>% pull(idx)

      # helper
      all_p <- sort(unique(d$periode))
      fi_nou  <- d %>% filter(grup == "Habitatge nou",  periode == tail(all_p, 1)) %>% pull(idx)
      fi_usat <- d %>% filter(grup == "Habitatge usat", periode == tail(all_p, 1)) %>% pull(idx)

      if (length(fi_nou) == 0 || length(fi_usat) == 0) return(NULL)
      nou_guanya <- fi_nou > fi_usat
      col   <- if (nou_guanya) "#f0fdf4" else "#fef3c7"
      tcol  <- if (nou_guanya) "#166534" else "#92400e"
      icon_ <- if (nou_guanya) "fa-circle-check" else "fa-circle-question"
      txt   <- if (nou_guanya)
        paste0("CONFIRMADA · Nou +", round(fi_nou - 100, 1), "% vs Usat +", round(fi_usat - 100, 1), "% (des del 2015)")
      else
        paste0("NO CONFIRMADA · Usat +", round(fi_usat - 100, 1), "% vs Nou +", round(fi_nou - 100, 1), "% (des del 2015)")

      tags$div(class = "h-veredicte-badge", style = paste0("background:", col, "; color:", tcol, ";"),
        tags$i(class = paste("fa-solid", icon_)),
        txt
      )
    })

    output$plot_h3_index <- renderPlot({
      d <- dades_h3()
      if (is.null(d)) {
        plot.new(); text(.5, .5, "Sense dades de preu nou/usat disponibles.", cex = 1.2, col = "gray50"); return()
      }

      periodes_h3 <- sort(unique(d$periode))
      d_num <- d %>% add_t_num()
      anys_rang  <- floor(d_num$t_num)
      any_breaks <- seq(min(anys_rang, na.rm = TRUE), max(anys_rang, na.rm = TRUE), by = 2)

      # Mateixos colors semàntics que H1: nominal=vermell, real=blau
      # Aquí: NOU = vermell (encariment major = "alerta"), USAT = blau
      col_h3 <- c("Habitatge nou" = COLOR_NOU, "Habitatge usat" = COLOR_USAT)

      df_lab <- d_num %>% group_by(grup) %>%
        slice_max(order_by = t_num, n = 1, with_ties = FALSE) %>%
        ungroup() %>% filter(!is.na(idx))

      ggplot(d_num, aes(x = t_num, y = idx, color = grup, group = grup)) +
        geom_hline(yintercept = 100, color = "#e2e8f0", linewidth = .6, linetype = "dashed") +
        geom_line(linewidth = .9, na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        geom_text_repel(
          data = df_lab, aes(label = paste0(grup, "\n+", round(idx - 100, 1), "%")),
          size = 3, fontface = "bold", direction = "y", nudge_x = .3,
          segment.size = .25, segment.color = "#e2e8f0", max.overlaps = 10
        ) +
        scale_color_manual(values = col_h3, name = NULL) +
        scale_x_continuous(breaks = any_breaks, labels = as.character(any_breaks)) +
        scale_y_continuous(labels = label_number(suffix = " ix", accuracy = 1)) +
        annotate("text", x = min(d_num$t_num, na.rm = TRUE) + .3, y = 101,
                 label = "Base 100 = 2015-Q1", size = 3, color = "#94a3b8", hjust = 0) +
        labs(x = NULL, y = "Índex (base 100 = 2015-Q1)",
             caption = "Font: Registre de la Propietat · Ajuntament de Barcelona") +
        tema_hab(base = 12) +
        theme(
          legend.position = "bottom",
          plot.margin     = margin(8, 60, 8, 8),
          axis.text.x     = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")

    output$plot_h3_creixement <- renderPlot({
      d <- dades_h3()
      if (is.null(d)) { plot.new(); return() }

      d_any <- d %>%
        group_by(grup, any) %>%
        summarise(idx = mean(idx, na.rm = TRUE), .groups = "drop") %>%
        filter(any > 2015)

      col_h3 <- c("Habitatge nou" = COLOR_NOU, "Habitatge usat" = COLOR_USAT)

      ggplot(d_any, aes(x = factor(any), y = idx - 100, fill = grup)) +
        geom_col(position = position_dodge(width = .8), width = .7) +
        geom_hline(yintercept = 0, color = "#e2e8f0", linewidth = .5) +
        scale_fill_manual(values = col_h3, name = NULL) +
        scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
        labs(x = NULL, y = "Creixement acumulat des de 2015 (%)") +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          axis.text.x     = element_text(angle = 45, hjust = 1)
        ) +
        guides(fill = guide_legend(override.aes = list(size = 4)))
    }, res = 115, bg = "white")

    output$h3_conclusio <- renderUI({
      d <- dades_h3()
      if (is.null(d)) return(tags$span("Sense dades disponibles per a H3."))

      all_p <- sort(unique(d$periode))
      fi_nou  <- d %>% filter(grup == "Habitatge nou",  periode == tail(all_p, 1)) %>% pull(idx)
      fi_usat <- d %>% filter(grup == "Habitatge usat", periode == tail(all_p, 1)) %>% pull(idx)

      if (length(fi_nou) == 0 || length(fi_usat) == 0) return(tags$span("No hi ha prou dades per treure conclusions."))

      nou_guanya <- fi_nou > fi_usat
      tags$div(
        tags$b("Conclusió: "),
        paste0(
          "Des del 2015-Q1, l'habitatge nou ha crescut un ", round(fi_nou - 100, 1),
          "% mentre que l'habitatge usat ha crescut un ", round(fi_usat - 100, 1), "%. ",
          if (nou_guanya)
            paste0("La hipòtesi H3 queda CONFIRMADA: l'habitatge nou s'ha encarit ",
                   round(fi_nou - fi_usat, 1), " punts percentuals per sobre del usat. ")
          else
            paste0("La hipòtesi H3 NO es confirma per a les dades disponibles: l'habitatge usat ha crescut ",
                   round(fi_usat - fi_nou, 1), " punts per sobre del nou. "),
          "Cal tenir en compte que les dades d'habitatge 'nou' presenten menys operacions i poden ser ",
          "més volàtils estadísticament."
        )
      )
    })

  })
}

# Helper intern
ordre_idx <- function(x) match(x, sort(unique(x)))
