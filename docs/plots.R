plot_axis_x <- "Age (Months)"
plot_axis_title_size <- 16
plot_axis_text_size <- 14
plot_legend_size <- 14
plot_point_size <- 1.6

site_creator_name <- "Debbie Odufuwa"
site_creator_github <- "https://github.com/dodufuwa"
site_lab_name <- "L3 Lab"
site_lab_url <- "https://l3lab.ucdavis.edu/"
site_year <- format(Sys.Date(), "%Y")
site_title <- "Function Word Acquisition in CHILDES 🐮"

# Load precomputed data for the site (produced by `docs/data_prep.R`)
library(arrow)
data_dir <- "data"
function_word_pop_traj_data <- tryCatch(
  read_feather(file.path(data_dir, "function_word_pop_traj_data.feather")),
  error = function(e) tibble::tibble(word = character(), target_child_age = numeric(), cumulative_relfreq = numeric(), cumulative_ppt = numeric())
)
gompertz_fitted_relfreq <- tryCatch(
  read_feather(file.path(data_dir, "gompertz_fitted_relfreq.feather")),
  error = function(e) tibble::tibble(word = character(), target_child_age = numeric(), fitted = numeric())
)
gompertz_onset_estimates <- tryCatch(
  read_feather(file.path(data_dir, "gompertz_onset_estimates.feather")),
  error = function(e) tibble::tibble(word = character(), estimate = numeric(), q2_5 = numeric(), q97_5 = numeric(), model = character())
)

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

filter_by_age <- function(dat, age_min, age_max) {
  dat %>%
    dplyr::filter(
      .data$target_child_age >= age_min,
      .data$target_child_age <= age_max
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

format_growth_estimate_text <- function(word, onset_tbl) {
  row <- onset_tbl %>% dplyr::filter(.data$word == !!word)
  if (nrow(row) == 0 || !is.finite(row$estimate[1])) {
    return(NULL)
  }
  # Standardized note: always start with the same phrase and report exact age in months
  estimate <- row$estimate[1]
  q2.5 <- row$q2_5[1]
  q97.5 <- row$q97_5[1]
  ci_text <- if (is.finite(q2.5) && is.finite(q97.5)) {
    sprintf("CI = [%.2f, %.2f]", q2.5, q97.5)
  } else {
    "CI unavailable"
  }
  sprintf(
    "A bayesian growth curve model estimated the onset age of production for \"%s\" as %.1f months (%s).",
    word,
    estimate,
    ci_text
  )
}

plot_population_trajectory <- function(w, show_points = TRUE, age_min, age_max) {
  # Use full data (no age-based filtering) but limit display with coord_cartesian
  dat <- function_word_pop_traj_data %>%
    dplyr::filter(word == w) %>%
    dplyr::arrange(target_child_age)
  if (nrow(dat) == 0) {
    return(NULL)
  }
  p <- ggplot(dat, aes(x = target_child_age, y = cumulative_ppt)) +
    geom_line(color = aggie_blue, linewidth = 1.2)
  if (show_points) {
    p <- p + geom_point(color = aggie_gold, size = plot_point_size)
  }
  p +
    labs(
      x = plot_axis_x,
      y = "Cumulative PPT"
    ) +
    plot_theme_common() +
    coord_cartesian(xlim = c(age_min, age_max))
}

plot_cumulative_ppt <- function(plotdat, show_points = TRUE, age_min, age_max) {
  plotdat <- filter_by_age(plotdat, age_min, age_max)
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
    plot_theme_common() +
    coord_cartesian(xlim = c(age_min, age_max))
}

plot_growth_curve <- function(w, show_points = TRUE, age_min, age_max) {
  # Use full pooled data for fitting/points but respect viewer x-limits
  dat <- function_word_pop_traj_data %>%
    dplyr::filter(word == w) %>%
    dplyr::arrange(target_child_age)
  # convert stored fitted relative-frequency (proportion) back to parts-per-thousand
  curve <- gompertz_fitted_relfreq %>%
    dplyr::filter(word == w, is.finite(fitted)) %>%
    dplyr::mutate(fitted = .data$fitted * 1000) %>%
    dplyr::arrange(target_child_age)
  if (nrow(dat) == 0 && nrow(curve) == 0) {
    return(NULL)
  }
  p <- ggplot()
  if (show_points && nrow(dat) > 0) {
    p <- p +
      geom_point(
        data = dat,
        aes(x = target_child_age, y = cumulative_ppt, color = "Actual Data"),
        size = plot_point_size
      )
  }
  if (nrow(curve) > 0) {
    p <- p + geom_line(
      data = curve,
      aes(x = target_child_age, y = fitted, color = "Fitted Values"),
      linewidth = 1.6
    )
  }
  if (!show_points && nrow(curve) == 0) {
    return(NULL)
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
    plot_theme_common() +
    coord_cartesian(xlim = c(age_min, age_max))
}

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
  title = "Function Word Acquisition in CHILDES",
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
        selectizeInput(
          "selected_function_word",
          label = NULL,
          choices = function_words,
          selected = function_words[[1]],
          options = list(
            placeholder = "Search for a function word...",
            onInitialize = I("function() { this.setValue(this.getValue()); }")
          )
        )
      ),
      tags$nav(
        class = "section-outline",
        `aria-label` = "Page sections",
        tags$p(class = "section-outline-title", "On this page"),
        tags$ul(
          class = "section-outline-list",
          tags$li(tags$a(href = "#section-population", "Population Trajectories")),
          tags$li(tags$a(href = "#section-trajectories", "Individual Child Trajectories")),
          tags$li(tags$a(href = "#section-growth", "Growth Curves")),
          tags$li(tags$a(href = "#section-literature", "Relevant Literature"))
        )
      )
    ),
    column(
      width = 9,
      class = "app-main",
      tags$div(
        class = "section-block",
        tags$h3(id = "section-population", class = "section-heading", "Population Trajectories"),
        plotlyOutput("populationTrajPlot", height = "420px"),
        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-controls",
            sliderInput(
              "age_range_population",
              "Age range (months)",
              min = age_months_min,
              max = age_months_max,
              value = c(age_months_min, age_months_max),
              step = 1,
              sep = ""
            ),
            checkboxInput("show_points_population", "Show data points", value = TRUE)
          )
        ),
        div(
          class = "plot-download",
          downloadButton("download_population", "Download population trajectory plot", class = "btn-primary")
        )
      ),

      tags$div(
        class = "section-block",
        tags$h3(id = "section-trajectories", class = "section-heading", "Individual Child Trajectories"),
        div(
          class = "trajectory-controls well",
          selectInput(
            "plot_mode",
            "Overlay Mode",
            choices = c(
              "Single Child" = "single",
              "Overlay Selected Children" = "multi_select",
              "Overlay Top Ten Children" = "all_top_ten"
            ),
            selected = "all_top_ten"
          ),
          conditionalPanel(
            condition = "input.plot_mode == 'single'",
            selectInput(
              "selected_child_display",
              "Select Target Child",
              choices = child_list_all,
              selected = child_list_all[[1]]
            )
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
            sliderInput(
              "age_range_trajectory",
              "Age range (months)",
              min = age_months_min,
              max = age_months_max,
              value = c(age_months_min, age_months_max),
              step = 1,
              sep = ""
            ),
            checkboxInput("show_points_trajectory", "Show data points", value = TRUE)
          )
        ),
        div(
          class = "plot-download",
          downloadButton("download_trajectory", "Download individual child trajectory plot", class = "btn-primary")
        ),
        tags$p(
          class = "growth-estimate",
          "Trajectories can be sparse for some children, especially for less frequent function words or smaller corpora."
        )
      ),

      tags$div(
        class = "section-block",
        tags$h3(id = "section-growth", class = "section-heading", "Growth Curves"),
        plotlyOutput("functionWordGrowthCurvePlot", height = "420px"),
        tags$div(
          class = "plot-toolbar",
          div(
            class = "plot-controls",
            sliderInput(
              "age_range_growth",
              "Age range (months)",
              min = age_months_min,
              max = age_months_max,
              value = c(age_months_min, age_months_max),
              step = 1,
              sep = ""
            ),
            checkboxInput("show_points_growth", "Show data points", value = TRUE)
          )
        ),
        div(
          class = "plot-download",
          downloadButton("download_growth", "Download growth curve plot", class = "btn-primary")
        ),
        uiOutput("growthCurveEstimates")
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
  output$populationTrajPlot <- renderPlotly({
    req(input$selected_function_word, input$age_range_population)
    ages <- list(min = input$age_range_population[1], max = input$age_range_population[2])
    p <- plot_population_trajectory(
      input$selected_function_word,
      show_points = isTRUE(input$show_points_population),
      age_min = ages$min,
      age_max = ages$max
    )
    validate(need(!is.null(p), "No data for selected function word and age range."))
    to_plotly(p, show_legend = FALSE)
  })

  output$download_population <- downloadHandler(
    filename = function() {
      safe_plot_filename("population_trajectory", input$selected_function_word)
    },
    content = function(file) {
      req(input$selected_function_word, input$age_range_population)
      ages <- list(min = input$age_range_population[1], max = input$age_range_population[2])
      save_plot_download(
        file,
        plot_population_trajectory(
          input$selected_function_word,
          show_points = isTRUE(input$show_points_population),
          age_min = ages$min,
          age_max = ages$max
        )
      )
    }
  )

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
    } else if (input$plot_mode == "all_top_ten") {
      kids <- top_ten_children %>%
        select(collection_name, corpus_name, target_child_name)
      dat <- dat %>%
        inner_join(kids, by = c("collection_name", "corpus_name", "target_child_name"))
    } else {
      req(input$selected_child_display)
      sel <- child_name_map %>%
        filter(display_name == input$selected_child_display) %>%
        slice(1)
      dat <- dat %>%
        filter(
          collection_name == sel$collection_name,
          corpus_name == sel$corpus_name,
          target_child_name == sel$target_child_name
        )
    }
    dat <- dat %>% arrange(collection_name, corpus_name, target_child_name, target_child_age)
    dat$child_label <- paste0(dat$target_child_name, " (", dat$collection_name, " | ", dat$corpus_name, ")")
    dat
  })

  output$cumulativePptPlot <- renderPlotly({
    req(input$selected_function_word, input$age_range_trajectory)
    ages <- list(min = input$age_range_trajectory[1], max = input$age_range_trajectory[2])
    plotdat <- filteredData()
    p <- plot_cumulative_ppt(
      plotdat,
      show_points = isTRUE(input$show_points_trajectory),
      age_min = ages$min,
      age_max = ages$max
    )
    validate(need(!is.null(p), "No data for selection and age range."))
    to_plotly(p, show_legend = TRUE)
  })

  output$download_trajectory <- downloadHandler(
    filename = function() {
      safe_plot_filename("individual_child_trajectory", input$selected_function_word)
    },
    content = function(file) {
      req(input$selected_function_word, input$plot_mode, input$age_range_trajectory)
      ages <- list(min = input$age_range_trajectory[1], max = input$age_range_trajectory[2])
      plotdat <- filteredData()
      p <- plot_cumulative_ppt(
        plotdat,
        show_points = isTRUE(input$show_points_trajectory),
        age_min = ages$min,
        age_max = ages$max
      )
      save_plot_download(file, p, width = 11, height = 7)
    }
  )

  output$functionWordGrowthCurvePlot <- renderPlotly({
    req(input$selected_function_word, input$age_range_growth)
    ages <- list(min = input$age_range_growth[1], max = input$age_range_growth[2])
    p <- plot_growth_curve(
      input$selected_function_word,
      show_points = isTRUE(input$show_points_growth),
      age_min = ages$min,
      age_max = ages$max
    )
    validate(need(!is.null(p), "No data for selected function word and age range."))
    to_plotly(p, show_legend = TRUE)
  })

  output$growthCurveEstimates <- renderUI({
    req(input$selected_function_word)
    word <- input$selected_function_word
    text <- format_growth_estimate_text(word, gompertz_onset_estimates)
    if (is.null(text)) {
      return(tags$p(
        class = "growth-estimate",
        "Growth curve onset estimates are not available for this function word."
      ))
    }
    tags$p(class = "growth-estimate", text)
  })

  output$download_growth <- downloadHandler(
    filename = function() {
      safe_plot_filename("growth_curve", input$selected_function_word)
    },
    content = function(file) {
      req(input$selected_function_word, input$age_range_growth)
      ages <- list(min = input$age_range_growth[1], max = input$age_range_growth[2])
      save_plot_download(
        file,
        plot_growth_curve(
          input$selected_function_word,
          show_points = isTRUE(input$show_points_growth),
          age_min = ages$min,
          age_max = ages$max
        )
      )
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
