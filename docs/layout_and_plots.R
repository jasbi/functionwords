site_title <- "The Function Word Dashboard 🐮"
site_lab_name <- "L3 Lab"
site_lab_url <- "https://l3lab.ucdavis.edu/"
site_creator_name <- "Debbie Odufuwa"
site_creator_github <- "https://github.com/dodufuwa"
site_year <- format(Sys.Date(), "%Y")

plot_axis_x <- "Age (Months)"
plot_axis_title_size <- 16
plot_axis_text_size <- 14
plot_legend_size <- 14
plot_point_size <- 1.6

plot_theme_common <- function() {
  theme_bw(base_family = "Lato") +
    theme(
      plot.title = element_blank(),
      axis.title = element_text(size = plot_axis_title_size, color = aggie_blue),
      axis.text = element_text(size = plot_axis_text_size, color = aggie_blue),
      legend.title = element_text(size = plot_legend_size, color = "black"),
      legend.text = element_text(size = plot_legend_size, color = aggie_blue),
      legend.position = "right"
    )
}

to_plotly <- function(p, show_legend = TRUE) {
  if (is.null(p)) {
    return(NULL)
  }
  gp <- ggplotly(p, tooltip = "x") %>%
    layout(
      hovermode = "x unified",
      font = list(family = "Lato, sans-serif", size = plot_axis_text_size),
      xaxis = list(
        title = list(font = list(size = plot_axis_title_size)),
        tickfont = list(size = plot_axis_text_size)
      ),
      yaxis = list(
        title = list(font = list(size = plot_axis_title_size)),
        tickfont = list(size = plot_axis_text_size)
      ),
      margin = list(r = if (show_legend) 120 else 40)
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
  if (show_legend) {
    gp <- gp %>%
      layout(
        legend = list(
          font = list(size = plot_legend_size),
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 1,
          yanchor = "top"
        )
      )
  }
  gp
}

plot_cumulative_ppt <- function(plotdat, show_points = TRUE) {
  if (nrow(plotdat) == 0) {
    return(NULL)
  }
  num_kids <- length(unique(plotdat$child_label))
  pal <- scale_color_brewer(palette = ifelse(num_kids <= 8, "Dark2", "Paired"))
  p <- ggplot(plotdat, aes(x = target_child_age, y = cumulative_ppt, color = child_label, group = child_label)) +
    geom_line(linewidth = 1.2)
  if (show_points) {
    p <- p + geom_point(size = plot_point_size)
  }
  p +
    pal +
    labs(
      x = plot_axis_x,
      y = "Cumulative PPT",
      color = NULL
    ) +
    guides(color = guide_legend(
      title = "Child (Collection | Corpus)",
      title.theme = element_text(color = "black", size = plot_legend_size)
    )) +
    plot_theme_common()
}

plot_growth_curve <- function(w, show_onset_line = TRUE) {
  obs <- word_growth_obs_by_word[[w]]
  fit <- word_growth_fit_by_word[[w]]
  if (is.null(obs) && is.null(fit)) {
    return(NULL)
  }
  p <- ggplot()
  if (!is.null(obs) && nrow(obs) > 0) {
    p <- p +
      geom_point(
        data = obs,
        aes(x = age, y = cumulative_ppt, color = "Actual Data"),
        size = plot_point_size
      )
  }
  if (!is.null(fit) && nrow(fit) > 0) {
    p <- p +
      geom_line(
        data = fit,
        aes(x = age, y = fitted_ppt, color = "Fitted Values"),
        linewidth = 1.6
      )
  }
  onset_row <- word_growth_aop %>% filter(.data$word == w)
  if (nrow(onset_row) > 0 && is.finite(onset_row$aop_estimate[1]) && isTRUE(show_onset_line)) {
    est <- onset_row$aop_estimate[1]
    ci_low <- onset_row$aop_q2_5[1]
    ci_high <- onset_row$aop_q97_5[1]
    # computes finite y-limits from observed/fitted data so the CI rectangle appears in ggplotly
    y_vals <- c()
    if (!is.null(obs) && nrow(obs) > 0) y_vals <- c(y_vals, obs$cumulative_ppt)
    if (!is.null(fit) && nrow(fit) > 0) y_vals <- c(y_vals, fit$fitted_ppt)
    if (length(y_vals) > 0 && any(is.finite(y_vals))) {
      y_min <- min(y_vals, na.rm = TRUE)
      y_max <- max(y_vals, na.rm = TRUE)
      rng <- y_max - y_min
      pad <- ifelse(rng == 0, 1, rng * 0.05)
      ymin <- y_min - pad
      ymax <- y_max + pad
      if (is.finite(ci_low) && is.finite(ci_high)) {
        rect_df <- data.frame(xmin = ci_low, xmax = ci_high, ymin = ymin, ymax = ymax)
        p <- p +
          geom_rect(data = rect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    inherit.aes = FALSE, fill = "red", alpha = 0.15, colour = NA)
      }
      p <- p +
        geom_vline(xintercept = est, color = "red", linetype = "dotted", linewidth = 0.8)
    } else {
      # fallback: draw only the vertical line
      p <- p +
        geom_vline(xintercept = est, color = "red", linetype = "dotted", linewidth = 0.8)
    }
  }
  p +
    scale_color_manual(
      name = "Legend",
      values = c("Actual Data" = aggie_gold, "Fitted Values" = aggie_blue)
    ) +
    labs(
      x = plot_axis_x,
      y = "Cumulative Parts Per Thousand"
    ) +
    plot_theme_common()
}

growth_plotly_key <- function(word) {
  as.character(word)
}

growth_plot_cache <- local({
  cache <- list()
  for (w in function_words) {
    cache[[growth_plotly_key(w)]] <- plot_growth_curve(w)
  }
  cache
})

growth_plotly_cache <- local({
  cache <- list()
  for (w in function_words) {
    p <- growth_plot_cache[[growth_plotly_key(w)]]
    cache[[growth_plotly_key(w)]] <- if (is.null(p)) NULL else to_plotly(p, show_legend = TRUE)
  }
  cache
})

safe_plot_filename <- function(prefix, word, ext = "png") {
  slug <- gsub("[^a-zA-Z0-9]+", "_", tolower(word))
  paste0(prefix, "_", slug, ".", ext)
}

save_plot_download <- function(file, plot_obj, width = 10, height = 6) {
  if (is.null(plot_obj)) {
    stop("No data available for this plot.")
  }
  ggplot2::ggsave(
    filename = file,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

ui <- fluidPage(
  title = "The Function Word Dashboard 🐮",
  theme = aggie_theme,
  tags$head(
    tags$style(HTML(sprintf(
      "
      html, body, .container-fluid {
        font-family: 'Lato', sans-serif;
        font-size: 16px;
        line-height: 1.5;
        color: #222;
      }
      h1, h2, h3, h4, .control-label, label {
        color: %s !important;
      }
      .selectize-input, .selectize-dropdown, .form-control, .btn {
        font-size: 16px !important;
      }
      .well, .card {
        background-color: %s !important;
        border-color: %s !important;
      }
      .app-layout {
        margin-top: 12px;
      }
      .app-sidebar {
        position: sticky;
        top: 12px;
        align-self: flex-start;
        padding-right: 16px;
      }
      .app-title {
        text-align: left;
        margin: 0 0 16px 0;
        font-family: 'Lato', sans-serif;
        font-weight: 700;
        font-size: 34px;
        line-height: 1.2;
        color: %s;
      }
      .function-word-search {
        margin-bottom: 20px;
      }
      .function-word-search .control-label {
        display: none;
      }
      .function-word-search .selectize-input {
        font-size: 16px;
        padding: 10px 12px;
        border: 2px solid %s;
        border-radius: 8px;
      }
      .section-outline {
        border: 2px solid %s;
        border-radius: 8px;
        padding: 12px 14px;
        background-color: #fff;
      }
      .section-outline-title {
        font-size: 14px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: %s !important;
        margin: 0 0 10px 0;
      }
      .section-outline-list {
        list-style: none;
        margin: 0;
        padding: 0;
      }
      .section-outline-list li {
        margin: 0;
        padding: 0;
      }
      .section-outline-list a {
        display: block;
        padding: 8px 10px;
        margin-bottom: 4px;
        color: %s !important;
        text-decoration: none;
        font-weight: 600;
        font-size: 16px;
        border: 1px solid %s;
        border-radius: 6px;
        transition: background-color 0.15s ease;
      }
      .section-outline-list a:hover,
      .section-outline-list a:focus {
        background-color: %s;
        color: %s !important;
        text-decoration: none;
      }
      .app-main {
        padding-left: 8px;
      }
      .section-heading {
        color: %s !important;
        font-weight: 700;
        font-size: 26px;
        margin-top: 28px;
        margin-bottom: 12px;
        border-bottom: 2px solid #002855;
        padding-bottom: 6px;
        scroll-margin-top: 12px;
      }
      .section-heading:first-child {
        margin-top: 0;
      }
      .section-block {
        margin-bottom: 42px;
        padding-bottom: 12px;
      }
      .trajectory-controls {
        margin-bottom: 12px;
      }
      .section-info-box {
        background-color: #FFB700;
        border: 1px solid #002855;
        border-radius: 8px;
        padding: 16px 20px;
        margin-bottom: 18px;
      }
      .section-info-box .plot-controls {
        margin: 0;
      }
      .literature-section {
        margin-top: 32px;
        padding: 16px 20px;
        background-color: %s;
        border: 1px solid %s;
        border-radius: 8px;
      }
      .literature-help {
        font-size: 16px;
        color: %s !important;
        margin-bottom: 14px;
      }
      .plot-toolbar {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-between;
        align-items: center;
        gap: 16px;
        margin: 12px 0 20px 0;
      }
      .plot-controls {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 16px;
        font-size: 16px;
        flex: 1 1 420px;
        min-width: 260px;
      }
      .plot-controls .shiny-input-container {
        flex: 1 1 320px;
        min-width: 240px;
      }
      .plot-controls .checkbox {
        margin-top: 0;
        margin-bottom: 0;
      }
      .plot-download {
        margin: 0;
        flex: 0 0 auto;
      }
      .growth-estimate {
        font-size: 16px;
        line-height: 1.55;
        color: #222 !important;
        margin: 12px 0 20px 0;
        padding: 12px 14px;
        background-color: #fff;
        border-left: 4px solid %s;
        border-radius: 4px;
      }
      .apa-references {
        margin: 0;
        padding: 0 0 0 1.5em;
        list-style: decimal;
      }
      .apa-reference {
        margin-bottom: 16px;
        padding-left: 0.5em;
      }
      .apa-citation {
        margin: 0;
        text-indent: -0.5em;
        padding-left: 0.5em;
        line-height: 1.5;
        color: #222 !important;
      }
      .apa-note {
        margin: 6px 0 0 0;
        font-size: 15px;
        font-style: italic;
        color: %s !important;
      }
      .plot-download {
        margin: 8px 0 24px 0;
      }
      .section-note {
        font-size: 15px;
        color: %s !important;
        margin: 0 0 14px 0;
        font-style: italic;
      }
      .site-attribution {
        margin-top: 40px;
        padding: 16px 0 24px 0;
        border-top: 1px solid %s;
        font-size: 14px;
        color: #002855 !important;
        text-align: center;
      }
      .plot-download {
        margin: 8px 0 24px 0;
      }
      .section-note {
        font-size: 15px;
        color: %s !important;
        margin: 0 0 14px 0;
        font-style: italic;
      }
      .site-attribution {
        margin-top: 40px;
        padding: 16px 0 24px 0;
        border-top: 1px solid %s;
        font-size: 14px;
        color: #002855 !important;
        text-align: center;
      }
      ",
      aggie_blue,
      aggie_gold, aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_gold,
      aggie_gold, aggie_blue,
      aggie_blue, aggie_gold,
      aggie_gold, aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_blue,
      aggie_gold,
      aggie_blue
    )))
  ),
  fluidRow(
    class = "app-layout",
    column(
      width = 3,
      class = "app-sidebar",
      div(
        class = "app-title",
        HTML(site_title)
      ),
      div(
        class = "function-word-search",
        tags$p(
          "Search for a function word (N=131) down below to view its growth curve, individual child trajectory plots, and relevant literature.",
          style = "margin-bottom: 8px; font-size: 15px; color: #002855;"
        ),
        selectizeInput(
          "selected_function_word",
          label = NULL,
          choices = function_words,
          selected = if ("no" %in% function_words) "no" else function_words[[1]],
          options = list(
            placeholder = "Search for a function word...",
            onInitialize = I("function() { this.setValue(this.getValue()); }")
          )
        )
      ),
      tags$nav(
        class = "section-outline",
        `aria-label` = "Page sections",
        tags$p(class = "section-outline-title", "Table of Contents"),
        tags$ul(
          class = "section-outline-list",
          tags$li(tags$a(href = "#section-growth", "Population Curves")),
          tags$li(tags$a(href = "#section-trajectories", "Trajectories for Individual Children")),
          tags$li(tags$a(href = "#section-literature", "Relevant Literature"))
        )
      )
    ),
    column(
      width = 9,
      class = "app-main",
      tags$div(
        class = "section-block",
        tags$h3(id = "section-growth", class = "section-heading", "Population Curves"),
        plotlyOutput("functionWordGrowthCurvePlot", height = "420px"),
        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-controls",
            checkboxInput("show_onset_line", "Show onset age estimate and CI", value = TRUE)
          )
        ),
        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-download",
            downloadButton("download_growth", "Download plot", class = "btn-primary")
          )
        ),
        uiOutput("growthCurveEstimates")
      ),

      tags$div(
        class = "section-block",
        tags$h3(id = "section-trajectories", class = "section-heading", "Trajectories for Individual Children"),
        div(
          class = "trajectory-controls well",
          selectInput(
            "plot_mode",
            "Overlay Mode",
            choices = c(
              "Overlay Selected Children" = "multi_select",
              "Overlay Top Ten Children" = "all_top_ten"
            ),
            selected = "all_top_ten"
          ),
          conditionalPanel(
            condition = "input.plot_mode == 'multi_select'",
            selectInput(
              "selected_children_multi",
              "Select Target Children (any corpus)",
              choices = child_list_all,
              selected = child_list_all[seq_len(min(3, length(child_list_all)))],
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.plot_mode == 'all_top_ten'",
            tags$p(
              style = "margin-bottom: 0;",
              "Overlays the top ten children (by total token count, 12–48 mo) across all collections and corpora."
            )
          )
        ),
        plotlyOutput("cumulativePptPlot", height = "460px"),

        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-controls",
            checkboxInput("show_points_trajectory", "Show data points", value = TRUE)
          )
        ),

        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-download",
            downloadButton("download_trajectory", "Download plot", class = "btn-primary")
          )
        ),
        tags$p(
          class = "growth-estimate",
          "Important note: Sparse production data can be observed in some function words (especially in early development), which poses challenges in producing trajectories for a large number of children."
        )
      ),

      tags$h3(id = "section-literature", class = "section-heading", "Relevant Literature"),
      div(
        class = "literature-section",
        uiOutput("functionWordLiterature")
      ),
      tags$footer(
        class = "site-attribution",
        {
          name_html <- if (nzchar(site_creator_github)) {
            sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>', site_creator_github, site_creator_name)
          } else {
            site_creator_name
          }
          lab_html <- if (nzchar(site_lab_url)) {
            sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>', site_lab_url, site_lab_name)
          } else {
            site_lab_name
          }
          HTML(sprintf('Website created by %s &middot; %s &middot; %s', name_html, lab_html, site_year))
        }
      )
    )
  )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    req(input$selected_function_word, input$plot_mode)
    dat <- child_relfreq %>%
      filter(word == input$selected_function_word)
    if (input$plot_mode == "multi_select") {
      req(input$selected_children_multi)
      wanted <- input$selected_children_multi
      selected_rows <- child_name_map %>% filter(display_name %in% wanted)
      dat <- dat %>%
        inner_join(selected_rows, by = c("collection_name", "corpus_name", "target_child_name"))
    } else {
      kids <- top_ten_children %>%
        select(collection_name, corpus_name, target_child_name)
      dat <- dat %>%
        inner_join(kids, by = c("collection_name", "corpus_name", "target_child_name"))
    }
    dat <- dat %>% arrange(collection_name, corpus_name, target_child_name, target_child_age)
    dat$child_label <- paste0(dat$target_child_name, " (", dat$collection_name, " | ", dat$corpus_name, ")")
    dat
  })

  output$functionWordGrowthCurvePlot <- renderPlotly({
    req(input$selected_function_word)
    p <- plot_growth_curve(input$selected_function_word, show_onset_line = isTRUE(input$show_onset_line))
    validate(need(!is.null(p), "No growth curve data for this function word."))
    to_plotly(p, show_legend = TRUE)
  })

  output$growthCurveEstimates <- renderUI({
    req(input$selected_function_word)
    text <- growth_estimate_text[[input$selected_function_word]]
    if (is.null(text) || text == "") {
      return(tags$p(
        class = "growth-estimate",
        paste0("Growth curve onset estimates are not available for \"", input$selected_function_word, "\".")
      ))
    }
    tags$p(class = "growth-estimate", text)
  })

  output$download_growth <- downloadHandler(
    filename = function() {
      safe_plot_filename("growth_curve", input$selected_function_word)
    },
    content = function(file) {
      req(input$selected_function_word)
      plot_obj <- plot_growth_curve(input$selected_function_word, show_onset_line = isTRUE(input$show_onset_line))
      if (is.null(plot_obj)) {
        stop("No growth curve data available for this function word.")
      }
      save_plot_download(file, plot_obj)
    }
  )

  output$cumulativePptPlot <- renderPlotly({
    req(input$selected_function_word, input$plot_mode)
    plotdat <- filteredData()
    p <- plot_cumulative_ppt(plotdat, show_points = isTRUE(input$show_points_trajectory))
    validate(need(!is.null(p), "No data for selection."))
    to_plotly(p, show_legend = TRUE)
  }) %>% bindCache(
    input$selected_function_word,
    input$plot_mode,
    input$selected_children_multi,
    input$show_points_trajectory
  )

  output$download_trajectory <- downloadHandler(
    filename = function() {
      safe_plot_filename("individual_child_trajectory", input$selected_function_word)
    },
    content = function(file) {
      req(input$selected_function_word, input$plot_mode)
      plotdat <- filteredData()
      p <- plot_cumulative_ppt(plotdat, show_points = isTRUE(input$show_points_trajectory))
      save_plot_download(file, p, width = 11, height = 7)
    }
  )

  output$functionWordLiterature <- renderUI({
    req(input$selected_function_word)
    word <- input$selected_function_word
    summary <- function_word_lit[[word]]
    if (is.null(summary) || summary == "") {
      return(tags$p(
        class = "literature-help",
        paste0("No references available for \"", word, "\".")
      ))
    }
    HTML(summary)
  })
}
