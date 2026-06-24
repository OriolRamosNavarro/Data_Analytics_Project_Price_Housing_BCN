# =============================================================================
# mod_renda.R — Accessibilitat: preu de l'habitatge vs. renda disponible
# Colors: sempre COLORS_DIST (de utils.R) — no re-definir aquí
# =============================================================================
library(shiny); library(ggplot2); library(ggrepel); library(dplyr)
library(tidyr); library(scales); library(readr); library(shinyWidgets)

mod_renda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Banner
    tags$div(
      style = "background:linear-gradient(135deg,#faf5ff,#fef3c7); border-radius:14px; padding:18px 22px; margin-bottom:18px;",
      tags$div(style = "display:flex; gap:16px; align-items:flex-start;",
        tags$i(class = "fa-solid fa-scale-balanced", style = "font-size:28px; color:#8b5cf6;"),
        tags$div(
          tags$div(style = "font-size:14px; font-weight:700; color:#6d28d9; margin-bottom:4px;",
                   "Accessibilitat a l'habitatge: preu vs. renda disponible"),
          tags$div(style = "font-size:12px; color:#7c3aed; line-height:1.6;",
            "Anàlisi basada en la ", tags$strong("renda disponible real de les llars per persona"),
            " per secció censal (Ajuntament de Barcelona, 2015-2022). ",
            tags$br(),
            "L'índex d'esforç mesura quants ", tags$strong("mesos de renda bruta"),
            " es necessiten per comprar 1 m² — com més alt, menys accessible."
          )
        )
      )
    ),

    fluidRow(
      # ── Controls ──
      column(3,
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Configuració"),
          selectInput(ns("any_renda"), "Any de la renda",
                      choices = 2022:2015, selected = 2022),
          uiOutput(ns("ui_any_hab")),
          uiOutput(ns("ui_trim_hab")),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Mètrica de preu"),
          radioGroupButtons(ns("metrica_preu"), NULL,
            choices = c("EUR/m²" = "preu_m2", "milers EUR" = "preu_total"),
            selected = "preu_m2", justified = TRUE, size = "sm"
          ),
          tags$hr(class = "control-divider"),
          tags$span(class = "control-section-label", "Visualització"),
          materialSwitch(ns("deflactar"), "Preu deflactat (real)", status = "warning", value = FALSE),
          uiOutput(ns("nota_deflacio"))
        ),
        # Llegenda districtes sempre visible
        tags$div(class = "control-panel",
          tags$span(class = "control-section-label", "Llegenda districtes"),
          tags$div(style = "display:flex; flex-direction:column; gap:5px;",
            lapply(names(COLORS_DIST_ALT), function(d) {
              tags$div(style = "display:flex; align-items:center; gap:8px;",
                tags$div(style = paste0("width:12px; height:12px; border-radius:3px; flex-shrink:0; background:", COLORS_DIST_ALT[d], ";")),
                tags$div(style = "font-size:11px; color:#374151;", d)
              )
            })
          )
        )
      ),

      # ── Contingut ──
      column(9,
        # KPIs de context
        uiOutput(ns("kpis_context")),

        # 1. Context: IPC i preu real vs nominal
        fluidRow(
          column(12,
            tags$div(class = "plot-container",
              tags$div(class = "plot-header",
                tags$div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
                  tags$div(
                    tags$div(class = "plot-title", "1 · Context: preu nominal vs. preu real (deflactat IPC)"),
                    tags$div(class = "plot-subtitle",
                             "Barcelona total · IPC Província de Barcelona (INE) · Base 2014 = 100 · Llegenda: blau = nominal, verd = real")
                  ),
                  tags$div(
                    style = "background:#fef3c7; color:#92400e; border-radius:20px; padding:4px 12px; font-size:11px; font-weight:600; white-space:nowrap;",
                    tags$i(class = "fa-solid fa-link", style = "margin-right:4px;"),
                    "Relacionat amb H1"
                  )
                )
              ),
              plotOutput(ns("plot_real_nominal"), height = "260px"),
              tags$div(class = "plot-footer",
                "Font IPC: INE, Província de Barcelona. Preu real = preu nominal / (IPC/100). ",
                "Permet comparar el poder adquisitiu real eliminant l'efecte inflació."
              )
            )
          )
        ),

        # 2. Scatter principal — ample complet
        fluidRow(
          column(12,
            tags$div(class = "plot-container",
              tags$div(class = "plot-header",
                tags$div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
                  tags$div(
                    tags$div(class = "plot-title", "2 · Preu de l'habitatge vs. Renda disponible per barri"),
                    tags$div(class = "plot-subtitle",
                      "Cada punt = 1 barri · color = districte · mida = índex d'esforç (mesos de renda per comprar 1 m²) · ",
                      "etiquetes només als barris extrems"
                    )
                  )
                )
              ),
              plotOutput(ns("plot_scatter"), height = "460px"),
              tags$div(class = "plot-footer",
                "Font renda: Dep. d'Estadística, Ajuntament de Barcelona (mitjana de seccions censals). ",
                "Font preus: Registre de la Propietat. · Línia = regressió lineal amb interval de confiança."
              )
            )
          )
        ),

        # 3. Taula (desplegable)
        tags$details(
          tags$summary(
            style = "cursor:pointer; font-size:13px; font-weight:600; color:#374151; padding:14px 18px; background:white; border-radius:14px; box-shadow:0 1px 4px rgba(0,0,0,.07); margin-bottom:4px; list-style:none; display:flex; align-items:center; gap:8px;",
            tags$i(class = "fa-solid fa-table", style = "color:#94a3b8;"),
            "3 · Taula d'accessibilitat per barri",
            tags$span(style = "font-size:11px; color:#94a3b8; font-weight:400; margin-left:auto;", "Clica per expandir")
          ),
          tags$div(class = "plot-container", style = "margin-top:4px;",
            tags$div(class = "plot-header",
              tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                tags$div(class = "plot-subtitle",
                         "Ordenada per índex d'esforç descendent · vermell = menys accessible"),
                tags$div(style = "font-size:11px; color:#94a3b8;",
                         uiOutput(ns("taula_periode")))
              )
            ),
            DT::dataTableOutput(ns("taula"))
          )
        )
      )
    )
  )
}

mod_renda_server <- function(id, df_hab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    anys_hab   <- sort(unique(df_hab$any))
    ipc_data   <- load_ipc()
    renda_data <- reactive({ load_renda() })

    output$ui_any_hab <- renderUI({
      anys_renda <- if (!is.null(renda_data())) unique(renda_data()$any) else anys_hab
      anys_disp  <- sort(intersect(anys_hab, anys_renda), decreasing = TRUE)
      selectInput(ns("any_hab"), "Any del preu (ha de coincidir amb renda)",
                  choices = anys_disp, selected = max(anys_disp))
    })

    output$ui_trim_hab <- renderUI({
      req(input$any_hab)
      trims <- df_hab %>% filter(any == as.integer(input$any_hab)) %>%
        pull(trimestre) %>% unique() %>% sort(decreasing = TRUE)
      selectInput(ns("trim_hab"), "Trimestre del preu",
                  choices = trims, selected = trims[1])
    })

    output$nota_deflacio <- renderUI({
      req(input$deflactar)
      if (!isTRUE(input$deflactar)) return(NULL)
      ipc_val <- if (!is.null(ipc_data))
        ipc_data %>% filter(any == as.integer(input$any_hab %||% 2022)) %>% pull(ipc_index)
      else numeric(0)
      if (length(ipc_val) == 0) return(NULL)
      tags$div(
        style = "background:#fef3c7; border:1px solid #fcd34d; border-radius:8px; padding:10px 12px; font-size:11px; color:#92400e; margin-top:8px;",
        tags$b("IPC aplicat: "), sprintf("%.2f", ipc_val[1]), " (base 2014=100)",
        tags$br(), "Font: INE, Província de Barcelona"
      )
    })

    # ── Dades fusionades ──
    dades <- reactive({
      req(input$any_hab, input$trim_hab, input$metrica_preu)
      renda <- renda_data()
      if (is.null(renda)) return(NULL)

      any_r <- as.integer(input$any_renda %||% 2022)
      any_h <- as.integer(input$any_hab)
      tri_h <- as.integer(input$trim_hab)

      df_p <- df_hab %>%
        filter(nivell == "barri", metrica == input$metrica_preu,
               tipus_habitatge == "total",
               any == any_h, trimestre == tri_h,
               !is.na(codi), !is.na(valor), valor > 0) %>%
        mutate(codi_int = as.integer(codi))

      renda_sel <- renda %>%
        filter(any == any_r) %>%
        select(codi_barri, nom_barri, renda_mitjana_eur)

      df_m <- df_p %>%
        inner_join(renda_sel, by = c("codi_int" = "codi_barri")) %>%
        filter(!is.na(renda_mitjana_eur))

      if (nrow(df_m) == 0) return(NULL)

      if (isTRUE(input$deflactar) && !is.null(ipc_data)) {
        ipc_val <- ipc_data %>% filter(any == any_h) %>% pull(ipc_index)
        if (length(ipc_val) > 0)
          df_m <- df_m %>% mutate(valor = valor / (ipc_val[1] / 100))
      }

      df_m %>% mutate(
        districte    = BARRI_DISTRICTE_MAP[as.character(codi_int)] %||% "Desconegut",
        esforc_mesos = valor / (renda_mitjana_eur / 12),
        esforc_anys  = valor / renda_mitjana_eur,
        ratio        = valor / renda_mitjana_eur
      )
    })

    # ── KPIs context ──
    output$kpis_context <- renderUI({
      d <- dades()
      if (is.null(d) || nrow(d) < 3) {
        return(tags$div(
          style = "background:#fef3c7; border:1px solid #fcd34d; border-radius:10px; padding:12px 16px; margin-bottom:14px; font-size:12px; color:#92400e;",
          tags$b("Dades no disponibles. "),
          "Comprova que el fitxer ", tags$code("data/renda_barris_real.csv"), " existeix."
        ))
      }

      r2         <- cor(d$renda_mitjana_eur, d$valor, use = "complete.obs")^2
      med_esforc <- median(d$esforc_mesos, na.rm = TRUE)
      top        <- d %>% slice_max(esforc_mesos, n = 1, with_ties = FALSE)
      bot        <- d %>% slice_min(esforc_mesos, n = 1, with_ties = FALSE)
      renda_med  <- median(d$renda_mitjana_eur, na.rm = TRUE)

      kpi_item <- function(val, lab, sub, col = "#3b82f6") {
        tags$div(
          style = paste0("background:white; border-radius:12px; padding:14px 16px;",
                         "border-top:3px solid ", col, "; box-shadow:0 1px 3px rgba(0,0,0,.06);"),
          tags$div(style = "font-size:10px; font-weight:700; text-transform:uppercase; letter-spacing:.08em; color:#94a3b8; margin-bottom:5px;", lab),
          tags$div(style = "font-size:20px; font-weight:800; color:#0f172a; letter-spacing:-.5px; line-height:1.2;", val),
          tags$div(style = "font-size:11px; color:#64748b; margin-top:3px;", sub)
        )
      }

      # Càlcul pis 70m²: preu medià/m² × 70 / renda_anual_mediana
      preu_m2_bcn <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2",
               tipus_habitatge == "total",
               any == as.integer(input$any_hab %||% max(unique(df_hab$any))),
               trimestre == as.integer(input$trim_hab %||% 4)) %>%
        pull(valor)
      preu_m2_bcn <- if (length(preu_m2_bcn) > 0) preu_m2_bcn[1] else median(d$valor, na.rm = TRUE)

      cost_pis_70   <- preu_m2_bcn * 70                          # EUR total pis 70m²
      mesos_pis_70  <- cost_pis_70 / (renda_med / 12)            # mesos de renda bruta
      anys_pis_70   <- mesos_pis_70 / 12

      val_pis <- if (anys_pis_70 >= 2)
        sprintf("%.1f anys", anys_pis_70)
      else
        sprintf("%.0f mesos", mesos_pis_70)

      sub_pis <- paste0(
        format(round(cost_pis_70), big.mark = "."), " EUR · ",
        format(round(preu_m2_bcn), big.mark = "."), " EUR/m² × 70 m²"
      )

      tags$div(
        style = "display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:12px; margin-bottom:16px;",
        kpi_item(val_pis, "Temps per comprar un pis de 70 m²",
                 sub_pis, "#8b5cf6"),
        kpi_item(sprintf("%.2f", r2), "R² (correlació renda-preu)",
                 if (r2 > .7) "Correlació molt alta" else if (r2 > .5) "Alta" else "Moderada",
                 if (r2 > .7) "#10b981" else "#f59e0b"),
        kpi_item(paste0(round(top$esforc_mesos, 1), " m/m²"), "Barri menys accessible",
                 substr(top$nom, 1, 26), "#ef4444"),
        kpi_item(paste0(round(bot$esforc_mesos, 1), " m/m²"), "Barri més accessible",
                 substr(bot$nom, 1, 26), "#10b981"),
        kpi_item(paste0(format(round(renda_med), big.mark = "."), " EUR"),
                 "Renda mediana/persona/any",
                 paste0("Any ", input$any_renda, " · mitjana seccions censals"), "#3b82f6")
      )
    })

    # ── 1. Preu nominal vs real ──
    output$plot_real_nominal <- renderPlot({
      if (is.null(ipc_data)) {
        plot.new(); text(.5, .5, "IPC no disponible.\nComprova data/ipc_barcelona.csv",
                         cex = 1.1, col = "gray50"); return()
      }

      bcn_preu <- df_hab %>%
        filter(nivell == "barcelona", metrica == "preu_m2", tipus_habitatge == "total") %>%
        group_by(any) %>%
        summarise(preu_nominal = mean(valor, na.rm = TRUE), .groups = "drop")

      df_ev <- bcn_preu %>%
        inner_join(ipc_data, by = "any") %>%
        mutate(preu_real = preu_nominal / (ipc_index / 100)) %>%
        pivot_longer(cols = c(preu_nominal, preu_real),
                     names_to = "tipus", values_to = "preu") %>%
        mutate(tipus = recode(tipus,
                              preu_nominal = "Preu nominal",
                              preu_real    = "Preu real (deflactat IPC)"))

      vals <- bcn_preu %>% inner_join(ipc_data, by = "any") %>%
        mutate(preu_real = preu_nominal / (ipc_index / 100))
      creix_nom  <- (last(vals$preu_nominal) - first(vals$preu_nominal)) / first(vals$preu_nominal) * 100
      creix_real <- (last(vals$preu_real)    - first(vals$preu_real))    / first(vals$preu_real) * 100

      # Colors semàntics globals
      col_ev <- c(
        "Preu nominal"              = COLOR_NOMINAL,
        "Preu real (deflactat IPC)" = COLOR_REAL
      )

      ggplot(df_ev, aes(x = any, y = preu, color = tipus, group = tipus)) +
        geom_line(linewidth = .9, na.rm = TRUE) +
        geom_point(size = 2, na.rm = TRUE) +
        scale_color_manual(values = col_ev, name = NULL) +
        scale_x_continuous(breaks = seq(2014, 2025, 2)) +
        scale_y_continuous(labels = label_comma(big.mark = ".", suffix = " EUR/m²")) +
        annotate("text", x = max(vals$any) - .3, y = max(vals$preu_nominal, na.rm = TRUE),
                 label = sprintf("+%.0f%% nominal", creix_nom),
                 hjust = 1, color = COLOR_NOMINAL, fontface = "bold", size = 3.5) +
        annotate("text", x = max(vals$any) - .3, y = min(vals$preu_real, na.rm = TRUE) + 80,
                 label = sprintf("+%.0f%% real", creix_real),
                 hjust = 1, color = COLOR_REAL, fontface = "bold", size = 3.5) +
        labs(x = NULL, y = "EUR/m²") +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          axis.text.x     = element_text(angle = 0)
        ) +
        guides(color = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")

    # ── 2. Scatter (versió llegible) ──
    output$plot_scatter <- renderPlot({
      d <- dades()
      if (is.null(d)) {
        plot.new(); text(.5, .5, "Sense dades.\nComprova data/renda_barris_real.csv",
                         cex = 1.2, col = "gray50"); return()
      }

      med_x <- median(d$renda_mitjana_eur, na.rm = TRUE)
      med_y <- median(d$valor, na.rm = TRUE)
      x_min <- min(d$renda_mitjana_eur, na.rm = TRUE)
      x_max <- max(d$renda_mitjana_eur, na.rm = TRUE)
      y_min <- min(d$valor, na.rm = TRUE)
      y_max <- max(d$valor, na.rm = TRUE)

      # Mida del punt proporcional a l'esforç (mesos per m²)
      d <- d %>% mutate(mida = rescale(esforc_mesos, to = c(2.5, 8)))

      # Etiquetes: top 5 i bottom 5 per esforç + destacar extrems de preu
      dest_top  <- d %>% slice_max(esforc_mesos, n = 5, with_ties = FALSE)
      dest_bot  <- d %>% slice_min(esforc_mesos, n = 5, with_ties = FALSE)
      dest      <- bind_rows(dest_top, dest_bot) %>% distinct(nom, .keep_all = TRUE)

      fit <- lm(valor ~ renda_mitjana_eur, data = d)
      r2  <- summary(fit)$r.squared

      # Etiquetes dels eixos mediana
      fmt_eur <- function(x) paste0(format(round(x / 1000, 1), nsmall = 1), "k EUR")

      p <- ggplot(d, aes(x = renda_mitjana_eur, y = valor)) +

        # Fons quadrants molt subtil
        annotate("rect", xmin = x_min * .98, xmax = med_x, ymin = med_y, ymax = y_max * 1.06,
                 fill = "#fef2f2", alpha = .5) +
        annotate("rect", xmin = med_x, xmax = x_max * 1.02, ymin = y_min * .94, ymax = med_y,
                 fill = "#f0fdf4", alpha = .5) +

        # Línies de mediana
        geom_vline(xintercept = med_x, color = "#cbd5e1", linetype = "dashed", linewidth = .6) +
        geom_hline(yintercept = med_y, color = "#cbd5e1", linetype = "dashed", linewidth = .6) +

        # Anotacions de mediana als eixos
        annotate("text", x = med_x, y = y_min * .94,
                 label = paste0("← Renda mediana\n", fmt_eur(med_x)),
                 hjust = .5, vjust = 0, size = 2.8, color = "#94a3b8") +
        annotate("text", x = x_min * .98, y = med_y,
                 label = paste0("Preu\nmedià\n", format(round(med_y), big.mark = "."), "€"),
                 hjust = 0, vjust = .5, size = 2.8, color = "#94a3b8") +

        # Etiquetes de quadrant
        annotate("label", x = x_min * .98, y = y_max * 1.05,
                 label = "Preu alt · renda baixa\n(menys accessible)",
                 hjust = 0, vjust = 1, size = 3, color = "#b91c1c",
                 fill = "#fef2f2", label.size = 0, label.padding = unit(4, "pt")) +
        annotate("label", x = x_max * 1.02, y = y_min * .94,
                 label = "Preu baix · renda alta\n(més accessible)",
                 hjust = 1, vjust = 0, size = 3, color = "#15803d",
                 fill = "#f0fdf4", label.size = 0, label.padding = unit(4, "pt")) +

        # Regressió
        geom_smooth(aes(x = renda_mitjana_eur, y = valor), method = "lm", se = TRUE,
                    color = "#94a3b8", fill = "#f1f5f9", linewidth = .9,
                    inherit.aes = FALSE, na.rm = TRUE) +

        # Tots els punts: mida = esforç, fill = districte, vora blanca per separar-los
        geom_point(aes(size = mida, fill = districte),
                   alpha = .82, stroke = .5, shape = 21, color = "white") +

        # Etiquetes només dels extrems
        geom_text_repel(
          data = dest,
          aes(label = nom, color = districte),
          size = 3, fontface = "bold",
          segment.size = .35, segment.color = "#cbd5e1",
          box.padding = .55, point.padding = .3,
          max.overlaps = 20, show.legend = FALSE
        ) +

        # R² a la cantonada
        annotate("label", x = x_max * 1.02, y = y_max * 1.05,
                 label = paste0("R² = ", round(r2, 2), "  (",
                                if (r2 > .7) "correlació molt alta" else if (r2 > .5) "alta" else "moderada", ")"),
                 hjust = 1, vjust = 1, size = 3, fontface = "bold", color = "#374151",
                 fill = "white", label.size = .3, label.padding = unit(5, "pt")) +

        scale_color_manual(values = COLORS_DIST_ALT, na.value = "#94a3b8", name = "Districte") +
        scale_fill_manual(values  = COLORS_DIST_ALT, na.value = "#94a3b8", guide = "none") +
        scale_size_identity() +
        scale_x_continuous(labels = label_comma(big.mark = ".", suffix = " EUR"),
                           expand = expansion(mult = .06)) +
        scale_y_continuous(labels = label_comma(big.mark = "."),
                           expand = expansion(mult = .08)) +
        labs(
          x = paste0("← Renda disponible per persona (EUR/any) · any ", input$any_renda, "   |   com més a la dreta, més renda →"),
          y = if (isTRUE(input$deflactar))
                paste0(METRICA_LABEL[input$metrica_preu], " (real 2014)")
              else METRICA_LABEL[input$metrica_preu],
          size = NULL
        ) +
        tema_hab(base = 12) +
        theme(
          legend.position  = "right",
          legend.title     = element_text(size = 10, face = "bold"),
          legend.text      = element_text(size = 9),
          legend.key.size  = unit(12, "pt")
        ) +
        guides(
          color = guide_legend(override.aes = list(size = 4, shape = 16, stroke = 0),
                               ncol = 1)
        )
      p
    }, res = 115, bg = "white")

    # ── 3. Evolució renda per categories ──
    output$plot_evol_renda <- renderPlot({
      renda <- renda_data()
      if (is.null(renda)) { plot.new(); return() }

      renda_2022 <- renda %>% filter(any == 2022)
      q_2022 <- quantile(renda_2022$renda_mitjana_eur, probs = c(.33, .67), na.rm = TRUE)

      renda_cat <- renda_2022 %>%
        mutate(cat = cut(renda_mitjana_eur,
                         breaks = c(0, q_2022[1], q_2022[2], Inf),
                         labels = c("Renda baixa (T1)", "Renda mitjana (T2)", "Renda alta (T3)"),
                         right = TRUE, include.lowest = TRUE)) %>%
        select(codi_barri, cat)

      df_ev <- renda %>%
        inner_join(renda_cat, by = "codi_barri") %>%
        group_by(any, cat) %>%
        summarise(renda = mean(renda_mitjana_eur, na.rm = TRUE), .groups = "drop")

      col_cat <- c(
        "Renda alta (T3)"    = COLOR_RENDA_ALT,
        "Renda mitjana (T2)" = COLOR_RENDA_MIG,
        "Renda baixa (T1)"   = COLOR_RENDA_BAX
      )

      ggplot(df_ev, aes(x = any, y = renda, color = cat, group = cat)) +
        geom_line(linewidth = 1.3, na.rm = TRUE) +
        geom_point(size = 2.5, na.rm = TRUE) +
        scale_color_manual(values = col_cat, name = "Categoria") +
        scale_x_continuous(breaks = unique(df_ev$any)) +
        scale_y_continuous(labels = label_comma(big.mark = ".", suffix = " EUR")) +
        labs(x = NULL, y = "Renda disponible per persona (EUR/any)") +
        tema_hab(base = 11) +
        theme(
          legend.position = "top",
          axis.text.x     = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(override.aes = list(linewidth = 2)))
    }, res = 115, bg = "white")

    # ── 5. Taula ──
    output$taula_periode <- renderUI({
      req(input$any_hab, input$trim_hab)
      tags$span(paste0("Preu: any ", input$any_hab, " T", input$trim_hab,
                       " · Renda: any ", input$any_renda))
    })

    output$taula <- DT::renderDataTable({
      d <- dades()
      if (is.null(d)) return(DT::datatable(data.frame(Info = "Sense dades")))

      deflat_sfx <- if (isTRUE(input$deflactar)) " (real)" else " (nominal)"

      d_t <- d %>%
        arrange(desc(esforc_mesos)) %>%
        mutate(
          Rang          = row_number(),
          Preu          = paste0(format(round(valor), big.mark = "."), deflat_sfx),
          Renda         = paste0(format(round(renda_mitjana_eur), big.mark = "."), " EUR/any"),
          Esforc_mesos  = round(esforc_mesos, 1),
          Esforc_anys   = round(esforc_anys, 2),
          Accessibilitat = case_when(
            esforc_mesos > quantile(esforc_mesos, .67, na.rm = TRUE) ~ "Baixa",
            esforc_mesos > quantile(esforc_mesos, .33, na.rm = TRUE) ~ "Mitjana",
            TRUE ~ "Alta"
          )
        ) %>%
        select(Rang, Barri = nom, Districte = districte,
               `Preu` = Preu,
               `Renda disponible` = Renda,
               `Esforç (mesos/m²)` = Esforc_mesos,
               `Esforç (anys/m²)` = Esforc_anys,
               Accessibilitat)

      DT::datatable(d_t, rownames = FALSE,
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "ftp",
          language = list(search = "Cerca:",
                          paginate = list(previous = "Ant.", `next` = "Seg."),
                          info = "Mostrant _START_-_END_ de _TOTAL_ barris")
        )
      ) %>%
        DT::formatStyle("Accessibilitat",
          backgroundColor = DT::styleEqual(c("Baixa", "Mitjana", "Alta"),
                                           c("#fee2e2", "#fef3c7", "#dcfce7")),
          color = DT::styleEqual(c("Baixa", "Mitjana", "Alta"),
                                 c("#991b1b", "#92400e", "#166534")),
          fontWeight = "bold"
        ) %>%
        DT::formatStyle("Esforç (mesos/m²)",
          background = DT::styleColorBar(d_t$`Esforç (mesos/m²)`, "#fee2e2"),
          backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
  })
}
