`%||%` <- function(x, y) if (is.null(x)) y else x

literature_candidates <- c("literature.yaml", file.path("docs", "literature.yaml"))
literature_file <- literature_candidates[file.exists(literature_candidates)][1]

normalize_literature_entry <- function(e) {
  if (is.null(e)) {
    return(NULL)
  }
  if (!is.null(e$focus_function_words)) {
    return(e)
  }
  if (length(e) == 1 && is.list(e[[1]])) {
    return(e[[1]])
  }
  NULL
}

extract_apa_citation <- function(entry) {
  if (!is.null(entry$apa) && nzchar(trimws(entry$apa))) {
    return(trimws(entry$apa))
  }
  if (!is.null(entry$authors) && !is.null(entry$year)) {
    authors <- paste(entry$authors, collapse = ", ")
    title <- entry$description %||% ""
    source <- entry$publication %||% ""
    return(trimws(sprintf("%s (%s). %s %s", authors, entry$year, title, source)))
  }
  ""
}

format_literature_html <- function(entries) {
  if (length(entries) == 0) {
    return("")
  }
  blocks <- vapply(entries, function(entry) {
    apa <- extract_apa_citation(entry)
    if (!nzchar(apa)) {
      return("")
    }
    note <- entry$notes %||% ""
    note_html <- if (nzchar(trimws(note))) {
      sprintf(
        '<p class="apa-note">%s</p>',
        htmltools::htmlEscape(trimws(note))
      )
    } else {
      ""
    }
    sprintf(
      '<li class="apa-reference"><p class="apa-citation">%s</p>%s</li>',
      htmltools::htmlEscape(apa),
      note_html
    )
  }, FUN.VALUE = character(1))
  blocks <- blocks[nzchar(blocks)]
  if (length(blocks) == 0) {
    return("")
  }
  paste0('<ol class="apa-references">', paste(blocks, collapse = ""), "</ol>")
}

if (!is.na(literature_file) && file.exists(literature_file)) {
  raw_lit <- yaml::yaml.load_file(literature_file)
  entries <- Filter(Negate(is.null), lapply(raw_lit, normalize_literature_entry))

  function_word_lit <- setNames(lapply(function_words, function(w) {
    matches <- Filter(function(e) w %in% e$focus_function_words, entries)
    format_literature_html(matches)
  }), function_words)
} else {
  warning("Literature YAML file not found. Using empty summaries.")
  function_word_lit <- setNames(as.list(rep("", length(function_words))), function_words)
}
