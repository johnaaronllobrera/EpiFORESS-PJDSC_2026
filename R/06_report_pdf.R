# EpiFORESS | 06_report_pdf.R
# PDF report drawing helpers and full report renderer.
# Split from app_full_fixed.R; code logic preserved during file separation.

# ============================================================
# 6A. PDF REPORT GENERATION
#
# Academic, publication-style report generated with base
# grDevices and graphics. No CSS, LaTeX, rmarkdown, or pandoc.
#
# Standard report structure:
#   Page 1: Assessment summary and interpretation
#   Page 2: Diagnostic assessment
#   Page 3: STL decomposition
#   Page 4: Recommended action, evidence, and methods
#
# Body paragraphs use width-aware full justification. Tables,
# labels, captions, and bullets remain left-aligned for clarity.
# ============================================================
# ------------------------------------------------------------
# 6A.1 PAGE GEOMETRY, TYPOGRAPHY, AND COLOR
# ------------------------------------------------------------
PDF_L <- 0.095
PDF_R <- 0.905
PDF_TOP <- 0.900
PDF_BOTTOM <- 0.090
PDF_BODY_BOTTOM <- 0.125
PDF_FONT <- "Times"
PDF_COL_TEXT <- "#202A35"
PDF_COL_MUTED <- "#5F6B76"
PDF_COL_RULE <- "#C9D1D9"
PDF_COL_LIGHT <- "#EEF1F4"
PDF_COL_ZEBRA <- "#F7F9FB"
PDF_COL_NAVY <- "#17365D"
PDF_COL_GREEN <- "#1F6B4F"
PDF_COL_AMBER <- "#9A6700"
PDF_COL_RED <- "#A40E26"
pdf_band_color <- function(band_key) {
  switch(
    band_key,
    "very-low" = PDF_COL_RED,
    "low" = "#B54708",
    "moderate" = PDF_COL_AMBER,
    "high" = "#2457A6",
    "very-high" = PDF_COL_GREEN,
    PDF_COL_MUTED
  )
}
pdf_status_color <- function(status_key) {
  switch(
    status_key,
    "good" = PDF_COL_GREEN,
    "warn" = PDF_COL_AMBER,
    "danger" = PDF_COL_RED,
    PDF_COL_MUTED
  )
}
# ------------------------------------------------------------
# 6A.2 TEXT AND LAYOUT UTILITIES
# ------------------------------------------------------------
pdf_is_blank <- function(x) {
  length(x) == 0L || all(is.na(x)) || !nzchar(stringr::str_squish(as.character(x)[1]))
}
pdf_clean_text <- function(x, strip_html = FALSE) {
  x <- as.character(x %||% "")
  if (strip_html) {
    x <- gsub("<[^>]+>", "", x)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    x <- gsub("&nbsp;", " ", x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
  }
  stringr::str_squish(x)
}
pdf_clean_items <- function(x) {
  x <- pdf_clean_text(x)
  x <- x[!is.na(x) & nzchar(x)]
  x[!duplicated(tolower(x))]
}
pdf_trim_sentence <- function(x) {
  x <- pdf_clean_text(x)
  sub("[[:space:]]*[.!?]+$", "", x)
}
pdf_remove_leading_label <- function(x) {
  x <- pdf_clean_text(x)
  sub("^[^:]{1,60}:[[:space:]]*", "", x)
}
pdf_lower_first <- function(x) {
  x <- pdf_clean_text(x)
  if (!nzchar(x)) return(x)
  paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
pdf_human_list <- function(items, max_items = 3L) {
  items <- pdf_clean_items(items)
  if (length(items) == 0L) return("")
  items <- head(items, max_items)
  items <- vapply(
    items,
    function(item) {
      item <- pdf_remove_leading_label(item)
      item <- pdf_trim_sentence(item)
      pdf_lower_first(item)
    },
    character(1)
  )
  if (length(items) == 1L) return(items)
  if (length(items) == 2L) return(paste(items, collapse = " and "))
  paste0(paste(items[-length(items)], collapse = ", "), ", and ", items[length(items)])
}
pdf_assessment_narrative <- function(score, band_label, concerns) {
  score_text <- if (is.finite(score)) {
    formatC(score, format = "f", digits = 1)
  } else {
    "not available"
  }
  opening <- paste0(
    "The series received an EpiFORESS score of ",
    score_text,
    " out of 100, placing it in the ",
    band_label,
    " band. The score is interpreted continuously, with higher values indicating stronger predicted forecast-readiness."
  )
  direction <- switch(
    score_band_key(score),
    "very-low" = "The integrated feature profile provides very limited support for routine forecast development.",
    "low" = "The integrated feature profile provides limited support and warrants data improvement before substantial forecast development.",
    "moderate" = "The integrated feature profile provides moderate support for cautious candidate-model comparison.",
    "high" = "The integrated feature profile is favorable for candidate-model development and rolling-origin validation.",
    "very-high" = "The integrated feature profile is strongly favorable for candidate-model development and comprehensive rolling-origin validation.",
    "The assessment could not be completed reliably."
  )
  concern_text <- pdf_human_list(concerns, max_items = 3L)
  if (nzchar(concern_text)) {
    direction <- paste0(
      direction,
      " Model development should give particular attention to ",
      concern_text,
      "."
    )
  }
  paste(opening, direction)
}
pdf_clean_action_steps <- function(steps) {
  steps <- pdf_clean_items(steps)
  vague_patterns <- c(
    "^review the affected feature",
    "^review the identified feature",
    "^review the flagged feature",
    "^address the identified concern",
    "^address the flagged concern",
    "^inspect the affected feature"
  )
  keep <- rep(TRUE, length(steps))
  for (pattern in vague_patterns) {
    keep <- keep & !grepl(pattern, steps, ignore.case = TRUE)
  }
  steps[keep]
}
pdf_line_height <- function(cex, leading = 1.15) {
  cex * 0.0187 * leading
}
pdf_text_width <- function(text, cex, font = 1) {
  graphics::strwidth(text, cex = cex, font = font, units = "user")
}
pdf_wrap_to_width <- function(text, width, cex = 0.79, font = 1) {
  text <- pdf_clean_text(text)
  
  if (!nzchar(text)) {
    return(character(0))
  }
  
  words <- strsplit(text, "\\s+")[[1]]
  lines <- character(0)
  current <- words[1]
  
  if (length(words) == 1L) {
    return(current)
  }
  
  for (word in words[-1]) {
    candidate <- paste(current, word)
    
    if (pdf_text_width(candidate, cex, font) <= width) {
      current <- candidate
    } else {
      lines <- c(lines, current)
      current <- word
    }
  }
  
  c(lines, current)
}
pdf_draw_text_line <- function(
    line,
    x,
    y,
    width,
    cex,
    col,
    font = 1,
    justify = FALSE
) {
  words <- strsplit(line, "\\s+")[[1]]
  
  if (!justify || length(words) < 2L) {
    graphics::text(x, y, line, cex = cex, font = font, col = col, adj = c(0, 1))
    return(invisible(NULL))
  }
  
  word_widths <- vapply(words, pdf_text_width, numeric(1), cex = cex, font = font)
  gap <- (width - sum(word_widths)) / (length(words) - 1L)
  normal_gap <- pdf_text_width(" ", cex = cex, font = font)
  
  if (!is.finite(gap) || gap <= 0 || gap > 3.5 * normal_gap) {
    graphics::text(x, y, line, cex = cex, font = font, col = col, adj = c(0, 1))
    return(invisible(NULL))
  }
  
  cursor <- x
  
  for (i in seq_along(words)) {
    graphics::text(cursor, y, words[i], cex = cex, font = font, col = col, adj = c(0, 1))
    cursor <- cursor + word_widths[i] + if (i < length(words)) gap else 0
  }
  
  invisible(NULL)
}
pdf_draw_lines <- function(
    lines,
    x,
    y,
    width,
    cex,
    col = PDF_COL_TEXT,
    font = 1,
    justify = FALSE,
    leading = 1.15
) {
  if (length(lines) == 0L) {
    return(y)
  }
  
  line_height <- pdf_line_height(cex, leading)
  
  for (i in seq_along(lines)) {
    pdf_draw_text_line(
      line = lines[i],
      x = x,
      y = y,
      width = width,
      cex = cex,
      col = col,
      font = font,
      justify = justify && i < length(lines)
    )
    
    y <- y - line_height
  }
  
  y
}
pdf_overlay_canvas <- function() {
  graphics::par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), new = TRUE, family = PDF_FONT)
  graphics::plot(
    NA_real_,
    NA_real_,
    type = "n",
    xlim = c(0, 1),
    ylim = c(0, 1),
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i",
    yaxs = "i"
  )
}
# ------------------------------------------------------------
# 6A.3 PAGE FRAME AND TYPOGRAPHIC COMPONENTS
# ------------------------------------------------------------
pdf_new_page <- function(section_title, series_label, years_label) {
  graphics::par(mar = c(0, 0, 0, 0), family = PDF_FONT)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  
  graphics::text(
    PDF_L, 0.967,
    "EPIFORESS FORECAST-READINESS ASSESSMENT",
    cex = 0.66, font = 2, col = PDF_COL_NAVY, adj = c(0, 1)
  )
  
  graphics::text(
    PDF_R, 0.967,
    section_title,
    cex = 0.66, col = PDF_COL_MUTED, adj = c(1, 1)
  )
  
  graphics::text(
    PDF_L, 0.944,
    paste0("Series: ", series_label, "   |   Period: ", years_label),
    cex = 0.64, col = PDF_COL_MUTED, adj = c(0, 1)
  )
  
  graphics::segments(PDF_L, 0.925, PDF_R, 0.925, col = PDF_COL_RULE, lwd = 0.8)
  PDF_TOP
}
pdf_footer <- function(page_num) {
  graphics::segments(PDF_L, PDF_BOTTOM, PDF_R, PDF_BOTTOM, col = PDF_COL_RULE, lwd = 0.7)
  
  graphics::text(
    PDF_L,
    PDF_BOTTOM - 0.017,
    paste0("Generated ", format(Sys.time(), "%B %d, %Y at %H:%M")),
    cex = 0.55,
    col = PDF_COL_MUTED,
    adj = c(0, 1)
  )
  
  graphics::text(
    PDF_R,
    PDF_BOTTOM - 0.017,
    paste0("Page ", page_num),
    cex = 0.55,
    col = PDF_COL_MUTED,
    adj = c(1, 1)
  )
}
pdf_section_heading <- function(
    number,
    label,
    x,
    y,
    cex = 0.96,
    col = PDF_COL_NAVY
) {
  heading <- if (pdf_is_blank(number)) label else paste0(number, ". ", label)
  
  graphics::text(x, y, heading, cex = cex, font = 2, col = col, adj = c(0, 1))
  rule_y <- y - pdf_line_height(cex) - 0.006
  graphics::segments(x, rule_y, PDF_R, rule_y, col = PDF_COL_RULE, lwd = 0.65)
  
  rule_y - 0.018
}
pdf_subheading <- function(label, x, y, col = PDF_COL_TEXT, cex = 0.76) {
  graphics::text(x, y, label, cex = cex, font = 2, col = col, adj = c(0, 1))
  y - pdf_line_height(cex) - 0.006
}
pdf_paragraph <- function(
    label,
    x,
    y,
    width = PDF_R - x,
    cex = 0.79,
    col = PDF_COL_TEXT,
    font = 1,
    justify = TRUE,
    leading = 1.15,
    paragraph_gap = 0.35
) {
  label <- pdf_clean_text(label)
  
  if (!nzchar(label)) {
    return(y)
  }
  
  lines <- pdf_wrap_to_width(label, width = width, cex = cex, font = font)
  y <- pdf_draw_lines(
    lines = lines,
    x = x,
    y = y,
    width = width,
    cex = cex,
    col = col,
    font = font,
    justify = justify,
    leading = leading
  )
  
  y - pdf_line_height(cex, leading) * paragraph_gap
}
pdf_bullets <- function(
    items,
    x,
    y,
    width = PDF_R - x,
    cex = 0.76,
    bullet_col = PDF_COL_NAVY,
    text_col = PDF_COL_TEXT,
    justify = FALSE
) {
  items <- pdf_clean_items(items)
  
  if (length(items) == 0L) {
    return(y)
  }
  
  bullet_x <- x + 0.004
  text_x <- x + 0.020
  text_width <- width - 0.020
  line_height <- pdf_line_height(cex)
  
  for (item in items) {
    lines <- pdf_wrap_to_width(item, width = text_width, cex = cex)
    
    graphics::points(bullet_x, y - 0.007, pch = 16, cex = 0.48, col = bullet_col)
    
    y <- pdf_draw_lines(
      lines = lines,
      x = text_x,
      y = y,
      width = text_width,
      cex = cex,
      col = text_col,
      justify = justify
    )
    
    y <- y - line_height * 0.30
  }
  
  y - line_height * 0.15
}
pdf_caption <- function(label, text, x, y, width = PDF_R - x) {
  label_width <- pdf_text_width(paste0(label, " "), cex = 0.68, font = 2)
  text_x <- x + label_width
  text_width <- width - label_width
  
  graphics::text(x, y, label, cex = 0.68, font = 2, col = PDF_COL_TEXT, adj = c(0, 1))
  
  lines <- pdf_wrap_to_width(text, width = text_width, cex = 0.68)
  pdf_draw_lines(
    lines = lines,
    x = text_x,
    y = y,
    width = text_width,
    cex = 0.68,
    col = PDF_COL_TEXT,
    justify = FALSE
  )
}
pdf_key_value_rows <- function(
    rows,
    x,
    y,
    label_width = 0.20,
    width = PDF_R - x,
    cex = 0.75
) {
  if (is.null(rows) || length(rows) == 0L) {
    return(y)
  }
  
  value_x <- x + label_width
  value_width <- width - label_width
  line_height <- pdf_line_height(cex)
  
  for (name in names(rows)) {
    value_lines <- pdf_wrap_to_width(as.character(rows[[name]]), value_width, cex = cex)
    row_lines <- max(1L, length(value_lines))
    
    graphics::text(x, y, name, cex = cex, font = 2, col = PDF_COL_MUTED, adj = c(0, 1))
    pdf_draw_lines(value_lines, value_x, y, value_width, cex, col = PDF_COL_TEXT)
    
    y <- y - row_lines * line_height - 0.008
  }
  
  y
}
pdf_evidence_rows <- function(
    labels,
    details,
    x,
    y,
    width = PDF_R - x,
    label_width = 0.185,
    cex = 0.68,
    label_col = PDF_COL_TEXT
) {
  labels <- as.character(labels)
  details <- as.character(details)
  valid <- !is.na(labels) & nzchar(stringr::str_squish(labels)) &
    !is.na(details) & nzchar(stringr::str_squish(details))
  labels <- labels[valid]
  details <- details[valid]
  if (length(labels) == 0L) return(y)
  
  value_x <- x + label_width
  value_width <- width - label_width
  line_height <- pdf_line_height(cex, leading = 1.12)
  
  for (i in seq_along(labels)) {
    label <- pdf_clean_text(labels[i])
    detail <- pdf_clean_text(details[i])
    lines <- pdf_wrap_to_width(detail, width = value_width, cex = cex)
    row_height <- max(0.034, length(lines) * line_height + 0.012)
    
    graphics::text(x, y, label, cex = cex, font = 2, col = label_col, adj = c(0, 1))
    pdf_draw_lines(
      lines = lines,
      x = value_x,
      y = y,
      width = value_width,
      cex = cex,
      col = PDF_COL_TEXT,
      justify = FALSE,
      leading = 1.12
    )
    
    y <- y - row_height
    if (i < length(labels)) {
      graphics::segments(x, y + 0.005, x + width, y + 0.005, col = PDF_COL_LIGHT, lwd = 0.55)
    }
  }
  y - 0.008
}
pdf_format_feature_value <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) return("Not estimable")
  absolute_value <- abs(value)
  if (absolute_value >= 100000 || (absolute_value > 0 && absolute_value < 0.0001)) {
    return(formatC(value, format = "e", digits = 3))
  }
  formatC(value, format = "fg", digits = 5, flag = "#")
}
pdf_feature_table_columns <- function() {
  c(PDF_L, 0.335, 0.775, PDF_R)
}
pdf_feature_table_header <- function(y) {
  x <- pdf_feature_table_columns()
  header_height <- 0.034
  graphics::rect(x[1], y - header_height, x[4], y, col = PDF_COL_LIGHT, border = PDF_COL_RULE, lwd = 0.65)
  headers <- c("Feature", "What it captures", "Value")
  for (i in seq_along(headers)) {
    graphics::text(x[i] + 0.006, y - 0.010, headers[i], cex = 0.62, font = 2, col = PDF_COL_NAVY, adj = c(0, 1))
  }
  graphics::segments(x[2:3], y, x[2:3], y - header_height, col = PDF_COL_RULE, lwd = 0.50)
  y - header_height
}
pdf_feature_row_layout <- function(feature, meaning, value, cex = 0.60) {
  x <- pdf_feature_table_columns()
  padding <- 0.006
  widths <- diff(x) - 2 * padding
  feature_lines <- pdf_wrap_to_width(feature, width = widths[1], cex = cex, font = 2)
  meaning_lines <- pdf_wrap_to_width(meaning, width = widths[2], cex = cex, font = 1)
  value_lines <- pdf_wrap_to_width(value, width = widths[3], cex = cex, font = 1)
  line_height <- pdf_line_height(cex, leading = 1.08)
  row_height <- max(
    0.037,
    max(length(feature_lines), length(meaning_lines), length(value_lines), 1L) * line_height + 0.014
  )
  list(
    x = x,
    padding = padding,
    widths = widths,
    feature_lines = feature_lines,
    meaning_lines = meaning_lines,
    value_lines = value_lines,
    row_height = row_height,
    cex = cex
  )
}
pdf_feature_table_row <- function(feature, meaning, value, y, zebra = FALSE) {
  layout <- pdf_feature_row_layout(feature = feature, meaning = meaning, value = value)
  x <- layout$x
  padding <- layout$padding
  row_height <- layout$row_height
  cell_top <- y - 0.010
  
  row_fill <- if (isTRUE(zebra)) PDF_COL_ZEBRA else "white"
  graphics::rect(x[1], y - row_height, x[4], y, col = row_fill, border = PDF_COL_RULE, lwd = 0.45)
  graphics::segments(x[2:3], y, x[2:3], y - row_height, col = PDF_COL_RULE, lwd = 0.45)
  
  pdf_draw_lines(layout$feature_lines, x[1] + padding, cell_top, layout$widths[1], layout$cex,
                 col = PDF_COL_TEXT, font = 2, justify = FALSE, leading = 1.08)
  pdf_draw_lines(layout$meaning_lines, x[2] + padding, cell_top, layout$widths[2], layout$cex,
                 col = PDF_COL_TEXT, font = 1, justify = FALSE, leading = 1.08)
  pdf_draw_lines(layout$value_lines, x[3] + padding, cell_top, layout$widths[3], layout$cex,
                 col = PDF_COL_TEXT, font = 1, justify = FALSE, leading = 1.08)
  
  y - row_height
}
pdf_assessment_result <- function(score, band_label, y) {
  band_key <- if (is.finite(score)) score_band_key(score) else "unknown"
  score_col <- pdf_band_color(band_key)
  score_text <- if (is.finite(score)) {
    formatC(score, format = "f", digits = 1)
  } else {
    "NA"
  }
  graphics::rect(
    PDF_L, y - 0.150, PDF_R, y,
    col = "#F7F8FA", border = PDF_COL_RULE, lwd = 0.8
  )
  graphics::rect(
    PDF_L, y - 0.150, PDF_L + 0.008, y,
    col = score_col, border = NA
  )
  graphics::text(
    PDF_L + 0.022, y - 0.021,
    "EPIFORESS SCORE",
    cex = 0.64, font = 2, col = PDF_COL_MUTED, adj = c(0, 1)
  )
  graphics::text(
    PDF_L + 0.022, y - 0.050,
    score_text,
    cex = 2.55, font = 2, col = score_col, adj = c(0, 1)
  )
  graphics::text(
    PDF_L + 0.165, y - 0.066,
    "out of 100",
    cex = 0.74, col = PDF_COL_MUTED, adj = c(0, 1)
  )
  graphics::text(
    PDF_L + 0.300, y - 0.030,
    band_label,
    cex = 1.02, font = 2, col = score_col, adj = c(0, 1)
  )
  graphics::text(
    PDF_L + 0.300, y - 0.068,
    "Higher scores indicate stronger predicted forecast-readiness.",
    cex = 0.73, col = PDF_COL_MUTED, adj = c(0, 1)
  )
  y - 0.173
}
# ------------------------------------------------------------
# 6A.4 DIAGNOSTIC TABLE
# ------------------------------------------------------------
pdf_diagnostic_status_key <- function(decision) {
  decision <- tolower(pdf_clean_text(decision))
  
  if (decision %in% c("passed", "present", "extracted")) {
    return("good")
  }
  
  if (decision == "flagged") {
    return("danger")
  }
  
  if (decision == "weak") {
    return("warn")
  }
  
  "neutral"
}
pdf_diagnostic_table <- function(diagnostic_table, y) {
  if (is.null(diagnostic_table) || nrow(diagnostic_table) == 0L) {
    return(pdf_paragraph("Diagnostic results were not available for this series.", PDF_L, y))
  }
  
  x <- c(PDF_L, 0.245, 0.365, 0.505, PDF_R)
  padding <- 0.006
  header_height <- 0.036
  headers <- c("Component", "Finding", "Metric", "Interpretation and forecasting implication")
  
  graphics::rect(x[1], y - header_height, x[5], y, col = PDF_COL_LIGHT, border = PDF_COL_RULE, lwd = 0.7)
  
  for (j in seq_along(headers)) {
    graphics::text(
      x[j] + padding,
      y - 0.010,
      headers[j],
      cex = 0.66,
      font = 2,
      col = PDF_COL_NAVY,
      adj = c(0, 1)
    )
  }
  
  y <- y - header_height
  
  for (i in seq_len(nrow(diagnostic_table))) {
    row <- diagnostic_table[i, , drop = FALSE]
    status_key <- pdf_diagnostic_status_key(row$Decision)
    status_col <- pdf_status_color(status_key)
    
    values <- list(
      pdf_clean_text(row$Component),
      pdf_clean_text(row$Decision),
      paste0(pdf_clean_text(row$Metric_Label), ": ", pdf_clean_text(row$Metric_Value)),
      paste(
        pdf_clean_text(row$Interpretation),
        "Forecasting implication:",
        pdf_clean_text(row$Recommendation)
      )
    )
    
    cex <- c(0.66, 0.65, 0.64, 0.63)
    font <- c(2, 2, 1, 1)
    left_inset <- c(padding, 0.022, padding, padding)
    widths <- diff(x) - left_inset - padding
    
    lines <- Map(
      function(value, width, size, face) pdf_wrap_to_width(value, width, size, face),
      values, widths, cex, font
    )
    
    max_lines <- max(vapply(lines, length, integer(1)), 1L)
    row_height <- max(0.068, max_lines * 0.0164 + 0.020)
    
    row_fill <- if (i %% 2 == 0) PDF_COL_ZEBRA else "white"
    graphics::rect(x[1], y - row_height, x[5], y, col = row_fill, border = PDF_COL_RULE, lwd = 0.55)
    graphics::segments(x[2:4], y, x[2:4], y - row_height, col = PDF_COL_RULE, lwd = 0.5)
    
    cell_top <- y - 0.012
    
    pdf_draw_lines(lines[[1]], x[1] + padding, cell_top, widths[1], cex[1], font = font[1])
    
    graphics::points(x[2] + 0.010, cell_top - 0.006, pch = 16, cex = 0.62, col = status_col)
    pdf_draw_lines(lines[[2]], x[2] + 0.022, cell_top, widths[2], cex[2], col = status_col, font = font[2])
    
    pdf_draw_lines(lines[[3]], x[3] + padding, cell_top, widths[3], cex[3])
    pdf_draw_lines(lines[[4]], x[4] + padding, cell_top, widths[4], cex[4])
    
    y <- y - row_height
  }
  
  y - 0.018
}
# ------------------------------------------------------------
# 6A.5 STL AXIS AND DECOMPOSITION FIGURE
# ------------------------------------------------------------
pdf_prepare_time_axis <- function(res, n_points) {
  index <- seq_len(n_points)
  default_axis <- list(
    x = index,
    at = pretty(index, n = 6),
    labels = pretty(index, n = 6),
    xlab = "Time index"
  )
  
  selected_data <- res$selected_data
  
  if (is.null(selected_data) || nrow(selected_data) < n_points) {
    return(default_axis)
  }
  
  selected_data <- selected_data[seq_len(n_points), , drop = FALSE]
  has_year <- ".EpiFORESS_Year" %in% names(selected_data)
  has_week <- ".EpiFORESS_Week" %in% names(selected_data)
  
  year <- if (has_year) suppressWarnings(as.numeric(selected_data$.EpiFORESS_Year)) else rep(NA_real_, n_points)
  week <- if (has_week) suppressWarnings(as.numeric(selected_data$.EpiFORESS_Week)) else rep(NA_real_, n_points)
  
  if (sum(is.finite(year)) >= 2L && sum(is.finite(week)) >= 2L) {
    frequency_value <- suppressWarnings(as.integer(round(res$frequency %||% 52)))
    
    if (!is.finite(frequency_value) || frequency_value < 1L) {
      frequency_value <- 52L
    }
    
    x <- year + (week - 1) / frequency_value
    
    if (sum(is.finite(x)) >= 2L) {
      ticks <- seq(floor(min(year, na.rm = TRUE)), ceiling(max(year, na.rm = TRUE)), by = 1)
      return(list(x = x, at = ticks, labels = as.character(ticks), xlab = "Year"))
    }
  }
  
  if (sum(is.finite(year)) >= 1L) {
    first_positions <- tapply(index[is.finite(year)], year[is.finite(year)], min)
    
    if (length(first_positions) > 0L) {
      return(list(
        x = index,
        at = as.numeric(first_positions),
        labels = names(first_positions),
        xlab = "Year"
      ))
    }
  }
  
  default_axis
}
pdf_draw_stl_decomposition <- function(res, panel_top = 0.790, panel_bottom = 0.300) {
  stl_data <- res$stl_decomposition_data
  
  if (is.null(stl_data) || nrow(stl_data) == 0L) {
    pdf_overlay_canvas()
    pdf_paragraph(
      paste(
        "STL decomposition was unavailable.",
        "A seasonal frequency greater than 1 and at least two complete seasonal cycles are required."
      ),
      PDF_L,
      panel_top,
      cex = 0.80
    )
    return(FALSE)
  }
  
  components <- c("Original series", "STL trend", "STL seasonality", "STL residual")
  
  stl_data <- stl_data %>%
    dplyr::mutate(Component = as.character(Component)) %>%
    dplyr::filter(Component %in% components, is.finite(Time_Index), is.finite(Value))
  
  if (nrow(stl_data) == 0L) {
    pdf_overlay_canvas()
    pdf_paragraph("STL decomposition did not contain finite component values.", PDF_L, panel_top, cex = 0.80)
    return(FALSE)
  }
  
  n_points <- max(stl_data$Time_Index, na.rm = TRUE)
  axis_info <- pdf_prepare_time_axis(res, n_points)
  panel_gap <- 0.014
  panel_height <- ((panel_top - panel_bottom) - panel_gap * (length(components) - 1L)) / length(components)
  old_par <- graphics::par(no.readonly = TRUE)
  
  for (i in seq_along(components)) {
    component <- components[i]
    top <- panel_top - (i - 1L) * (panel_height + panel_gap)
    bottom <- top - panel_height
    
    panel_data <- stl_data %>%
      dplyr::filter(Component == component) %>%
      dplyr::arrange(Time_Index)
    
    panel_x <- axis_info$x[panel_data$Time_Index]
    panel_y <- panel_data$Value
    valid <- is.finite(panel_x) & is.finite(panel_y)
    panel_x <- panel_x[valid]
    panel_y <- panel_y[valid]
    
    graphics::par(
      fig = c(PDF_L, PDF_R, bottom, top),
      mar = c(if (i == length(components)) 2.5 else 0.7, 4.0, 1.2, 0.7),
      new = TRUE,
      family = PDF_FONT,
      xaxs = "i"
    )
    
    if (length(panel_x) < 2L || length(panel_y) < 2L) {
      next
    }
    
    graphics::plot(
      panel_x,
      panel_y,
      type = "l",
      axes = FALSE,
      xlab = "",
      ylab = "",
      lwd = if (component == "Original series") 1.15 else 1.00,
      col = if (component == "STL residual") "#5B6573" else PDF_COL_NAVY
    )
    
    graphics::axis(
      2,
      cex.axis = 0.58,
      las = 1,
      col.axis = PDF_COL_MUTED,
      col = PDF_COL_RULE,
      tck = -0.015
    )
    
    if (component %in% c("STL seasonality", "STL residual")) {
      graphics::abline(h = 0, col = "#AEB7C2", lwd = 0.6, lty = 2)
    }
    
    if (i == length(components)) {
      graphics::axis(
        1,
        at = axis_info$at,
        labels = axis_info$labels,
        cex.axis = 0.60,
        col.axis = PDF_COL_MUTED,
        col = PDF_COL_RULE,
        tck = -0.015
      )
      
      graphics::mtext(axis_info$xlab, side = 1, line = 1.55, cex = 0.68, col = PDF_COL_MUTED)
    }
    
    graphics::box(bty = "l", col = PDF_COL_RULE, lwd = 0.7)
    graphics::mtext(component, side = 3, line = 0.15, adj = 0, cex = 0.72, font = 2, col = PDF_COL_TEXT)
  }
  
  graphics::par(old_par)
  pdf_overlay_canvas()
  TRUE
}
# ------------------------------------------------------------
# 6A.6 REPORT RENDERER
# ------------------------------------------------------------
render_epiforess_pdf_report <- function(file, res, series_label, years_label) {
  score_data <- res$score
  score <- suppressWarnings(as.numeric(score_data$EpiFORESS))

  estimated_median_best_mase <-
    suppressWarnings(
      as.numeric(
        score_data$Estimated_Median_Best_MASE
      )
    )

  if (
    length(estimated_median_best_mase) == 0L ||
    !is.finite(estimated_median_best_mase)
  ) {
    estimated_median_best_mase <-
      estimate_median_best_mase(score)[1]
  }

  band_label <- score_label(score)
  band_key <- if (is.finite(score)) score_band_key(score) else "unknown"
  band_col <- pdf_band_color(band_key)
  
  concerns <- pdf_clean_items(res$potential_concerns_vec %||% character(0))
  profile <- make_diagnostic_profile(res$raw_features)
  action <- compute_recommended_action(score, concerns)
  diagnostic_table <- res$stationarity_stl_table
  n_observations <- suppressWarnings(as.integer(score_data$N_Observations))
  frequency_value <- suppressWarnings(as.integer(res$frequency %||% NA_integer_))
  
  grDevices::pdf(
    file,
    width = 8.27,
    height = 11.69,
    onefile = TRUE,
    family = PDF_FONT,
    paper = "special"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  
  # ==========================================================
  # PAGE 1: ASSESSMENT
  # ==========================================================
  page_num <- 1L
  y <- pdf_new_page("Assessment", series_label, years_label)
  
  graphics::text(
    PDF_L, y, "EpiFORESS Forecast-Readiness Report",
    cex = 1.48, font = 2, col = PDF_COL_NAVY, adj = c(0, 1)
  )
  y <- y - 0.040
  graphics::segments(PDF_L, y, PDF_L + 0.090, y, col = band_col, lwd = 2.6)
  y <- y - 0.014
  graphics::text(
    PDF_L, y,
    "Epidemiological Forecasting-Readiness Screening System for Disease Count Time Series",
    cex = 0.84, col = PDF_COL_MUTED, adj = c(0, 1)
  )
  y <- y - 0.048
  
  y <- pdf_key_value_rows(
    rows = c(
      "Series assessed" = series_label,
      "Observation period" = years_label,
      "Valid observations" = if (is.finite(n_observations)) as.character(n_observations) else "Not available",
      "Estimated MASE" =
        if (is.finite(estimated_median_best_mase)) {
          paste0(
            formatC(estimated_median_best_mase, format = "f", digits = 2),
            " (score-calibrated estimate)"
          )
        } else {
          "Not available"
        },
      "Seasonal frequency" = if (is.finite(frequency_value)) paste0(frequency_value, " observations per cycle") else "Not available"
    ),
    x = PDF_L,
    y = y,
    label_width = 0.205
  )
  
  y <- y - 0.006
  y <- pdf_assessment_result(score, band_label, y)
  y <- pdf_section_heading("1", "Assessment", PDF_L, y)
  y <- pdf_subheading("Interpretation", PDF_L, y, cex = 0.78)
  
  assessment_text <- pdf_assessment_narrative(
    score = score,
    band_label = band_label,
    concerns = concerns
  )
  
  y <- pdf_paragraph(assessment_text, PDF_L, y, cex = 0.79)
  y <- y - 0.004
  y <- pdf_paragraph(
    paste(
      "EpiFORESS integrates the complete 49-feature profile into a single",
      "readiness score without imposing a binary operational decision."
    ),
    PDF_L,
    y,
    cex = 0.72,
    col = PDF_COL_MUTED,
    font = 3
  )
  pdf_footer(page_num)
  
  # ==========================================================
  # PAGE 2: DIAGNOSTIC EVIDENCE
  # ==========================================================
  page_num <- page_num + 1L
  y <- pdf_new_page("Diagnostic Evidence", series_label, years_label)
  y <- pdf_section_heading("2", "Diagnostic Evidence", PDF_L, y)
  y <- pdf_paragraph(
    paste(
      "The diagnostics below clarify the statistical structure underlying the",
      "assessment by summarizing stationarity and STL-based trend, seasonality,",
      "and residual variation. Together with the remaining predictors, they",
      "strengthen the interpretability of the complete 49-feature profile."
    ),
    PDF_L,
    y,
    cex = 0.77,
    col = PDF_COL_MUTED
  )
  y <- y - 0.004
  graphics::text(
    PDF_L, y, "Table 1. Stationarity and decomposition diagnostics",
    cex = 0.70, font = 2, col = PDF_COL_TEXT, adj = c(0, 1)
  )
  y <- y - 0.026
  y <- pdf_diagnostic_table(diagnostic_table, y)
  
  pdf_paragraph(
    paste(
      "Note. The Augmented Dickey-Fuller decision uses alpha = 0.05.",
      "STL is fitted with a periodic seasonal window and robust estimation.",
      "Missing observations are interpolated only for decomposition and",
      "diagnostic calculation. At least two complete seasonal cycles are",
      "required. STL component strengths are distinct from the linear trend",
      "R-squared and week-of-year seasonality R-squared predictors used",
      "by the EpiFORESS classification models."
    ),
    PDF_L,
    y,
    cex = 0.65,
    col = PDF_COL_MUTED,
    font = 3,
    justify = TRUE
  )
  pdf_footer(page_num)
  
  # ==========================================================
  # PAGE 3: DIAGNOSTIC EVIDENCE, CONTINUED
  # ==========================================================
  page_num <- page_num + 1L
  y <- pdf_new_page("Diagnostic Evidence, continued", series_label, years_label)
  y <- pdf_subheading("Seasonal-trend decomposition using LOESS", PDF_L, y, col = PDF_COL_NAVY, cex = 0.94)
  y <- pdf_paragraph(
    paste(
      "The figure separates the interpolated series into its observed level,",
      "STL trend, STL seasonal component, and STL residual variation.",
      "Independent vertical scales preserve detail within each component."
    ),
    PDF_L,
    y,
    cex = 0.76,
    col = PDF_COL_MUTED
  )
  
  stl_available <- pdf_draw_stl_decomposition(
    res = res,
    panel_top = min(y - 0.010, 0.790),
    panel_bottom = 0.300
  )
  
  if (isTRUE(stl_available)) {
    caption_y <- pdf_caption(
      "Figure 1.",
      paste(
        "STL decomposition of the selected series.",
        "The seasonal component repeats across cycles because s.window = \"periodic\".",
        "The residual panel contains variation not explained by the estimated STL trend and seasonal components."
      ),
      PDF_L,
      0.255
    )
    pdf_paragraph(
      paste(
        "The decomposition indicates where temporal structure is concentrated",
        "and how much variation remains after the estimated STL trend and",
        "seasonal components have been removed."
      ),
      PDF_L,
      caption_y - 0.012,
      cex = 0.64,
      col = PDF_COL_MUTED,
      font = 3,
      justify = TRUE
    )
  }
  pdf_footer(page_num)
  
  # ==========================================================
  # PAGE 4: DECISION GUIDANCE
  # ==========================================================
  page_num <- page_num + 1L
  y <- pdf_new_page("Forecasting Guidance", series_label, years_label)
  y <- pdf_section_heading("3", "Forecasting Guidance", PDF_L, y, col = band_col)
  y <- pdf_subheading("Recommended action", PDF_L, y, cex = 0.78)
  graphics::text(PDF_L, y, action$heading, cex = 1.00, font = 2, col = band_col, adj = c(0, 1))
  y <- y - 0.034
  y <- pdf_paragraph(action$summary, PDF_L, y, cex = 0.78)
  
  next_steps <- pdf_clean_action_steps(action$next_steps)
  if (length(next_steps) > 0L) {
    y <- pdf_subheading("Priority next steps", PDF_L, y)
    y <- pdf_bullets(head(next_steps, 4L), PDF_L, y, cex = 0.74, bullet_col = PDF_COL_NAVY)
  }
  
  y <- y - 0.002
  y <- pdf_subheading("Evidence informing the recommendation", PDF_L, y, cex = 0.78)
  
  good_index <- profile$Status_Key == "good"
  good_labels <- profile$Area[good_index]
  good_details <- profile$Explanation[good_index]
  if (length(good_labels) > 3L) {
    good_labels <- good_labels[seq_len(3L)]
    good_details <- good_details[seq_len(3L)]
  }
  
  y <- pdf_subheading("Readiness-supporting evidence", PDF_L, y, col = PDF_COL_GREEN, cex = 0.72)
  if (length(good_labels) > 0L) {
    y <- pdf_evidence_rows(
      labels = good_labels,
      details = good_details,
      x = PDF_L,
      y = y,
      cex = 0.68,
      label_col = PDF_COL_GREEN
    )
  } else {
    y <- pdf_paragraph(
      "The classification reflects the combined 49-feature profile rather than dependence on one dominant favorable indicator.",
      PDF_L, y, cex = 0.68
    )
  }
  
  attention_index <- profile$Status_Key %in% c("warn", "danger")
  attention_labels <- profile$Area[attention_index]
  attention_details <- profile$Explanation[attention_index]
  if (length(attention_labels) > 3L) {
    attention_labels <- attention_labels[seq_len(3L)]
    attention_details <- attention_details[seq_len(3L)]
  }
  
  y <- pdf_subheading("Factors requiring attention", PDF_L, y, col = PDF_COL_AMBER, cex = 0.72)
  if (length(attention_labels) > 0L) {
    y <- pdf_evidence_rows(
      labels = attention_labels,
      details = attention_details,
      x = PDF_L,
      y = y,
      cex = 0.68,
      label_col = PDF_COL_AMBER
    )
  } else {
    y <- pdf_paragraph("No core feature group raised a material concern in this assessment.", PDF_L, y, cex = 0.68)
  }
  
  if (y < PDF_BODY_BOTTOM + 0.285) {
    pdf_footer(page_num)
    page_num <- page_num + 1L
    y <- pdf_new_page("Decision Guidance, continued", series_label, years_label)
  } else {
    y <- y - 0.002
  }
  
  y <- pdf_subheading("Technical basis and appropriate use", PDF_L, y, cex = 0.78)
  y <- pdf_paragraph(
    paste0(
      "The EpiFORESS score is calculated as 100 x [1 - P(Hybrid Not Ready)]. ",
      "The deployed hybrid combines the statistical and machine-learning components using weights of ",
      formatC(w_stat, format = "f", digits = 3), " and ",
      formatC(w_ml, format = "f", digits = 3),
      ", respectively. Weight source: ", weight_source,
      ". The simulation-derived reference P(Not Ready) = ",
      formatC(simulation_not_ready_threshold, format = "f", digits = 2),
      " corresponds to an EpiFORESS score of ",
      formatC(simulation_score_boundary, format = "f", digits = 0),
      ". The displayed readiness bands are rounded descriptive interpretations. ",
      "The assessment uses the complete 49-feature predictor profile with the model-aligned ",
      "preprocessing and imputation rules implemented in the application. ",
      "The secondary estimated median Best MASE is calculated as ",
      "exp[0.688744 - 0.145625 x (EpiFORESS / 10)]. ",
      "It translates the score into the validation-set forecast-error scale ",
      "but does not replace direct rolling-origin evaluation of candidate models."
    ),
    PDF_L,
    y,
    cex = 0.68
  )
  y <- pdf_paragraph(
    paste(
      "EpiFORESS positions a series along a readiness gradient for forecast",
      "development. Candidate forecasting models should subsequently be",
      "evaluated through benchmarked out-of-sample validation before",
      "operational deployment."
    ),
    PDF_L,
    y,
    cex = 0.68
  )
  y <- pdf_subheading("Forecast-development checks", PDF_L, y, cex = 0.72)
  pdf_bullets(
    c(
      "Compare candidate models with a seasonal-naive benchmark.",
      "Evaluate MASE, bias, and stability across rolling forecast origins.",
      "Inspect residual dependence and unresolved structural change.",
      "Document preprocessing, corrections, and operational assumptions."
    ),
    PDF_L,
    y,
    cex = 0.67,
    bullet_col = PDF_COL_NAVY
  )
  pdf_footer(page_num)
  
  # ==========================================================
  # PAGE 5 ONWARD: EXTRACTED FEATURES
  # ==========================================================
  feature_table <- res$feature_table
  
  if (!is.null(feature_table) && nrow(feature_table) > 0L) {
    feature_table <- feature_table %>%
      dplyr::mutate(
        Domain = as.character(Domain),
        Feature = as.character(Feature),
        `What it captures` = as.character(`What it captures`),
        Value = as.numeric(Value)
      )
    
    page_num <- page_num + 1L
    y <- pdf_new_page("Extracted Features", series_label, years_label)
    y <- pdf_section_heading("4", "Extracted Features", PDF_L, y)
    y <- pdf_paragraph(
      paste(
        "The following tables present the complete 49-feature profile used",
        "for EpiFORESS scoring. Features are grouped by analytical domain and",
        "retain the definitions implemented in the deployed extraction pipeline."
      ),
      PDF_L,
      y,
      cex = 0.73,
      col = PDF_COL_MUTED
    )
    y <- pdf_paragraph(
      paste(
        "Linear trend R-squared and week-of-year seasonality R-squared are",
        "model predictors. The STL trend and seasonal strengths shown in the",
        "diagnostic section are separate decomposition-based measures."
      ),
      PDF_L,
      y,
      cex = 0.64,
      col = PDF_COL_MUTED,
      font = 3
    )
    
    for (domain_name in feature_domain_order) {
      domain_rows <- feature_table %>% dplyr::filter(Domain == domain_name)
      if (nrow(domain_rows) == 0L) next
      
      first_value <- pdf_format_feature_value(domain_rows$Value[1])
      first_layout <- pdf_feature_row_layout(
        feature = domain_rows$Feature[1],
        meaning = domain_rows$`What it captures`[1],
        value = first_value
      )
      required_space <- 0.030 + 0.034 + first_layout$row_height
      
      if (y - required_space < PDF_BODY_BOTTOM) {
        pdf_footer(page_num)
        page_num <- page_num + 1L
        y <- pdf_new_page("Extracted Features, continued", series_label, years_label)
      }
      
      y <- pdf_subheading(domain_name, PDF_L, y, col = PDF_COL_NAVY, cex = 0.76)
      y <- pdf_feature_table_header(y)
      
      for (row_index in seq_len(nrow(domain_rows))) {
        feature_name <- as.character(domain_rows$Feature[row_index])
        feature_meaning <- as.character(domain_rows$`What it captures`[row_index])
        feature_value <- pdf_format_feature_value(domain_rows$Value[row_index])
        row_layout <- pdf_feature_row_layout(
          feature = feature_name,
          meaning = feature_meaning,
          value = feature_value
        )
        
        if (y - row_layout$row_height < PDF_BODY_BOTTOM) {
          pdf_footer(page_num)
          page_num <- page_num + 1L
          y <- pdf_new_page("Extracted Features, continued", series_label, years_label)
          y <- pdf_subheading(paste0(domain_name, " (continued)"), PDF_L, y, col = PDF_COL_NAVY, cex = 0.76)
          y <- pdf_feature_table_header(y)
        }
        
        y <- pdf_feature_table_row(
          feature = feature_name,
          meaning = feature_meaning,
          value = feature_value,
          y = y,
          zebra = (row_index %% 2 == 0)
        )
      }
      y <- y - 0.016
    }
    pdf_footer(page_num)
  }
  
  invisible(TRUE)
}
