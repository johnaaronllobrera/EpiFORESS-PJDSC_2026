# EpiFORESS | 01_helpers.R
# General utility functions and shared helpers.
# Split from app_full_fixed.R; code logic preserved during file separation.

# 2. GENERAL HELPERS
# ------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
normalize_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}
safe_numeric <- function(x) {
  x <- stringr::str_replace_all(as.character(x), ",|%", "")
  out <- suppressWarnings(as.numeric(x))
  out[is.infinite(out)] <- NA_real_
  out
}
safe_var <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::var(x)
}
safe_sd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}
safe_cv <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(m) || abs(m) < 1e-12 || !is.finite(s)) return(NA_real_)
  s / abs(m)
}
safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, p, names = FALSE))
}
is_valid_number <- function(x) {
  length(x) == 1 && !is.na(x) && is.finite(x)
}
clamp_probability <- function(p) {
  p <- as.numeric(p)
  p[p < 0] <- 0
  p[p > 1] <- 1
  p
}
format_audit_number <- function(x, digits = 4) {
  x <- as.numeric(x)
  if (length(x) != 1 || is.na(x) || !is.finite(x)) return("NA")
  formatC(x, format = "f", digits = digits)
}
format_p_value <- function(p) {
  p <- as.numeric(p)
  if (length(p) != 1 || is.na(p) || !is.finite(p)) return("NA")
  if (p < 0.0001) return("<0.0001")
  formatC(p, format = "f", digits = 4)
}
collapse_clean <- function(x) {
  x <- x[!is.na(x)]
  x <- stringr::str_squish(x)
  x <- x[x != ""]
  if (length(x) == 0) return("None detected.")
  paste(x, collapse = ", ")
}
lower_initial <- function(x) {
  x <- as.character(x)
  vapply(x, function(value) {
    value <- stringr::str_squish(value)
    if (is.na(value) || value == "") return(value)
    paste0(stringr::str_to_lower(stringr::str_sub(value, 1, 1)), stringr::str_sub(value, 2))
  }, character(1), USE.NAMES = FALSE)
}
bold_sentence_list <- function(x) {
  x <- x[!is.na(x)]
  x <- stringr::str_squish(x)
  x <- x[x != ""]
  if (length(x) == 0) return("")
  x <- paste0("<strong>", htmltools::htmlEscape(x), "</strong>")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}
professional_dt_options <- function(page_length = 12, scroll_x = FALSE, show_pagination = TRUE) {
  list(
    pageLength = page_length,
    scrollX = scroll_x,
    autoWidth = FALSE,
    dom = if (isTRUE(show_pagination)) "tp" else "t",
    info = FALSE,
    searching = FALSE,
    lengthChange = FALSE,
    ordering = TRUE,
    pagingType = "simple_numbers",
    language = list(
      paginate = list(previous = "\u2039", `next` = "\u203a"),
      emptyTable = "No results available."
    ),
    columnDefs = list(list(className = "dt-left", targets = "_all"))
  )
}
make_feature_value_table <- function(features, label_map, vars = predictor_vars) {
  features <- as.data.frame(features)
  values <- sapply(vars, function(v) {
    if (v %in% names(features)) as.numeric(features[[v]][1]) else NA_real_
  })
  labels <- unname(label_map[vars])
  labels[is.na(labels)] <- vars[is.na(labels)]
  domains <- unname(feature_domain_map[vars])
  domains[is.na(domains)] <- "Unclassified Feature Domain"
  meanings <- unname(feature_interpretation_map[vars])
  meanings[is.na(meanings)] <- "No accessible description is available."
  tibble::tibble(
    Domain = domains,
    Feature = labels,
    `What it captures` = meanings,
    Value = as.numeric(values)
  )
}
make_selected_data_display_table <- function(data) {
  d <- as.data.frame(data)
  names(d)[names(d) == ".EpiFORESS_Year"] <- "Selected Year"
  names(d)[names(d) == ".EpiFORESS_Week"] <- "Selected Week"
  names(d)[names(d) == "Count_Selected"] <- "Selected Count"
  d
}
make_score_detail_table <- function(score_table) {
  score_table <- as.data.frame(score_table)
  tibble::tibble(
    Field = names(score_table),
    Value = vapply(score_table, function(x) as.character(x[1]), character(1))
  )
}
unwrap_model_object <- function(model_obj) {
  supported <- c("glm", "train", "ranger", "xgb.Booster", "cv.glmnet", "glmnet")
  if (any(vapply(supported, function(cl) inherits(model_obj, cl), logical(1)))) {
    return(model_obj)
  }
  if (is.list(model_obj)) {
    for (nm in c(
      "model", "final_model", "best_model", "fitted_model", "caret_model",
      "ranger_model", "xgb_model", "object", "fit", "final_fit", "finalModel",
      "final_model_object", "best_model_object"
    )) {
      cand <- model_obj[[nm]]
      if (is.null(cand)) next
      if (any(vapply(supported, function(cl) inherits(cand, cl), logical(1)))) {
        return(cand)
      }
    }
  }
  model_obj
}
get_model_predictor_names <- function(model_obj, default_predictors = predictor_vars) {
  obj <- model_obj
  if (is.list(obj)) {
    for (nm in c(
      "predictors", "predictor_vars", "features", "x_names",
      "final_predictor_cols", "predictor_cols", "model_predictors", "predictor_columns"
    )) {
      if (!is.null(obj[[nm]]) && length(obj[[nm]]) > 0) {
        out <- intersect(as.character(obj[[nm]]), default_predictors)
        if (length(out) > 0) return(out)
      }
    }
  }
  model <- unwrap_model_object(obj)
  if (inherits(model, "ranger")) {
    ranger_names <- tryCatch(model$forest$independent.variable.names, error = function(e) NULL)
    if (is.null(ranger_names) && !is.null(model$variable.importance)) {
      ranger_names <- names(model$variable.importance)
    }
    out <- intersect(as.character(ranger_names), default_predictors)
    if (length(out) > 0) return(out)
  }
  if (inherits(model, "xgb.Booster")) {
    fn <- tryCatch(model$feature_names, error = function(e) NULL)
    out <- intersect(as.character(fn), default_predictors)
    if (length(out) > 0) return(out)
  }
  if (inherits(model, "train")) {
    if (!is.null(model$xNames)) {
      out <- intersect(model$xNames, default_predictors)
      if (length(out) > 0) return(out)
    }
    if (!is.null(model$trainingData)) {
      out <- setdiff(names(model$trainingData), c(".outcome", "y", "y_notready"))
      out <- intersect(out, default_predictors)
      if (length(out) > 0) return(out)
    }
    if (!is.null(model$finalModel) && !is.null(model$finalModel$xNames)) {
      out <- intersect(model$finalModel$xNames, default_predictors)
      if (length(out) > 0) return(out)
    }
  }
  if (inherits(model, "cv.glmnet")) {
    coef_names <- tryCatch(rownames(stats::coef(model, s = "lambda.min")), error = function(e) NULL)
    out <- intersect(setdiff(coef_names, "(Intercept)"), default_predictors)
    if (length(out) > 0) return(out)
  }
  if (inherits(model, "glmnet")) {
    coef_names <- tryCatch(rownames(stats::coef(model, s = model$lambda[1])), error = function(e) NULL)
    out <- intersect(setdiff(coef_names, "(Intercept)"), default_predictors)
    if (length(out) > 0) return(out)
  }
  if (inherits(model, "glm")) {
    terms_obj <- tryCatch(attr(stats::terms(model), "term.labels"), error = function(e) NULL)
    out <- intersect(terms_obj, default_predictors)
    if (length(out) > 0) return(out)
  }
  default_predictors
}
extract_imputation_medians <- function(final_results, predictor_set) {
  med <- rep(0, length(predictor_set))
  names(med) <- predictor_set
  if (is.null(final_results) || !is.list(final_results)) return(med)
  candidate_names <- c(
    "imputation_medians", "train_medians", "training_medians",
    "predictor_medians", "median_imputation_values",
    "final_imputation_medians", "preprocessing_medians"
  )
  for (nm in candidate_names) {
    obj <- final_results[[nm]]
    if (is.null(obj)) next
    if (is.data.frame(obj)) {
      nms <- normalize_name(names(obj))
      var_col <- names(obj)[nms %in% c("variable", "predictor", "feature", "name")][1]
      med_col <- names(obj)[nms %in% c("median_used", "median", "value", "imputation_value")][1]
      if (!is.na(var_col) && !is.na(med_col)) {
        vals <- safe_numeric(obj[[med_col]])
        names(vals) <- as.character(obj[[var_col]])
        hit <- intersect(names(vals), predictor_set)
        med[hit] <- vals[hit]
        return(med)
      }
    }
    if (is.numeric(obj) && !is.null(names(obj))) {
      hit <- intersect(names(obj), predictor_set)
      med[hit] <- obj[hit]
      return(med)
    }
  }
  med
}
prepare_model_features <- function(model_features, predictor_set, medians = NULL) {
  mf <- as.data.frame(model_features)
  for (v in predictor_set) {
    if (!v %in% names(mf)) mf[[v]] <- NA_real_
    mf[[v]] <- as.numeric(mf[[v]])
    mf[[v]][is.infinite(mf[[v]]) | is.nan(mf[[v]])] <- NA_real_
  }
  if (is.null(medians)) {
    medians <- rep(0, length(predictor_set))
    names(medians) <- predictor_set
  }
  if (is.null(names(medians))) {
    names(medians) <- predictor_set[seq_len(min(length(medians), length(predictor_set)))]
  }
  for (v in predictor_set) {
    med <- medians[[v]]
    if (is.null(med) || length(med) == 0 || is.na(med) || !is.finite(med)) med <- 0
    mf[[v]][is.na(mf[[v]])] <- med
  }
  mf <- mf %>% dplyr::select(dplyr::all_of(predictor_set))
  bad_after <- names(mf)[vapply(mf, function(z) any(is.na(z) | is.nan(z) | is.infinite(z)), logical(1))]
  if (length(bad_after) > 0) {
    stop("Invalid values remain after median imputation: ", paste(bad_after, collapse = ", "))
  }
  mf
}
# ------------------------------------------------------------
