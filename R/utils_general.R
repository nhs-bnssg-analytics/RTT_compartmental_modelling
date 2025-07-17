#' Calculates performance by period from a given data set and target stock
#'
#' @param incompletes_data tibble; requires column headers of "period",
#'   "months_waited_id" and "value", where value is the count of incomplete
#'   pathways by period and the number of months the patients have waited
#' @param target_bin integer; the number of months waited where patients that
#'   have waited for greater or equal to that number of months have breached
#'   performance
#' @return A two column tibble containing "period" and "prop" columns, where
#'   prop is the proportion of people that are on the waiting list in each
#'   period that have waited for the time less that specified by the target_bin
#' @importFrom rlang .data
#' @importFrom dplyr across all_of group_by ungroup group_vars case_when mutate
#'   filter
#' @noRd
calc_performance <- function(incompletes_data, target_bin) {
  # check one record per month waited per period
  check_counts <- incompletes_data |>
    dplyr::count(
      .data$period,
      .data$months_waited_id
    ) |>
    dplyr::filter(
      .data$n > 1
    )

  if (nrow(check_counts) > 0) {
    stop("duplicate counts per period and month waited")
  }

  current_groupings <- dplyr::group_vars(
    incompletes_data
  )

  incompletes_data <- dplyr::ungroup(
    incompletes_data
  )

  # check target_bin within the range of bins available
  if (!(target_bin %in% incompletes_data[["months_waited_id"]])) {
    stop("target_bin not a valid month waited in the incompletes_data")
  }

  # check names
  if (
    !all(c("period", "months_waited_id", "value") %in% names(incompletes_data))
  ) {
    stop(
      "'period', 'months_waited_id' and 'value' should be present in incompletes_data"
    )
  }

  performance <- incompletes_data |>
    mutate(
      perf = case_when(
        .data$months_waited_id < target_bin ~ "Below",
        .default = "Above"
      )
    ) |>
    summarise(
      value = sum(.data$value),
      .by = all_of(
        c("period", "perf", current_groupings)
      )
    ) |>
    mutate(
      prop = .data$value / sum(.data$value),
      .by = all_of(
        c("period", current_groupings)
      )
    ) |>
    filter(
      .data$perf == "Below"
    ) |>
    select(
      all_of(
        c(
          "period",
          "prop",
          current_groupings
        )
      )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          current_groupings
        )
      )
    )

  return(performance)
}

#' @importFrom dplyr as_tibble filter pull
#' @param lm_object the output from a lm() function
#' @param term string; the term name for the p.value of interest. This is used
#'   for a filter() operation
#' @noRd
extract_pval <- function(lm_object, input_term) {
  if (!inherits(lm_object, "lm")) {
    stop("lm_object not lm class")
  }

  p_val <- summary(lm_object)$coefficients |>
    dplyr::as_tibble(rownames = "term") |>
    filter(.data$term == input_term) |>
    pull(
      .data$`Pr(>|t|)`
    )

  return(p_val)
}

#' extracts the percentage value at the end of a string
#' @noRd
extract_percent <- function(text) {
  match <- regmatches(text, regexpr("\\d+(\\.\\d+)?%$", text))
  if (length(match) > 0 && match != "") {
    return(
      as.numeric(sub("%$", "", match))
    )
  } else {
    return(numeric(0)) # Return empty numeric vector if no match
  }
}

extract_first_number <- function(text) {
  start_month <- sub("[+\\-].*", "", text)
  return(as.integer(start_month))
}

convert_month_to_factor <- function(months_waited_id) {
  months_waited_character <- ifelse(
    months_waited_id < 12,
    paste0(months_waited_id, "-", months_waited_id + 1, " months"),
    "12+ months"
  )

  months_waited_factor <- factor(
    months_waited_character,
    levels = paste(
      c(
        paste0(0:11, "-", 1:12),
        "12+"
      ),
      "months"
    )
  )
  return(months_waited_factor)
}

#' Replaces values in a string vector with corresponding values from a named
#' vector
#' @param string_vector A character vector
#' @param replacement_vector A named vector where names correspond to values in
#'   string_vector, adn can be regular expressions, and values are the
#'   replacements
#' @return A character vector with replaced values
#' @noRd
replace_fun <- function(string_vector, replacement_vector) {
  # Check if replacement_vector has names
  if (is.null(names(replacement_vector))) {
    stop("replacement_vector must have names")
  }

  replaced_vector <- string_vector

  for (pattern in names(replacement_vector)) {
    replaced_vector <- ifelse(
      grepl(pattern, replaced_vector),
      replacement_vector[pattern],
      replaced_vector
    )
  }

  return(replaced_vector)
}

#' @param names character vector of names
#' @param type string; one of "NHS Region", "Provider Parent", "Provider Org",
#'   "Commissioner Parent", "Commissioner Org"
#' @importFrom dplyr all_of select rename distinct filter pull
#' @noRd
org_name_lkp <- function(names = NULL, type) {
  type <- match.arg(
    type,
    c(
      "NHS Region",
      "Provider Parent",
      "Provider Org",
      "Commissioner Parent",
      "Commissioner Org"
    )
  )

  if (length(names) == 0) {
    return(NULL)
  }

  if (type == "NHS Region") {
    code_col <- "NHS Region Code"
    name_col <- "NHS Region Name"
  } else if (type == "Provider Parent") {
    code_col <- "Provider Parent Org Code"
    name_col <- "Provider Parent Name"
  } else if (type == "Provider Org") {
    code_col <- "Provider Org Code"
    name_col <- "Provider Org Name"
  } else if (type == "Commissioner Parent") {
    code_col <- "Commissioner Parent Org Code"
    name_col <- "Commissioner Parent Name"
  } else if (type == "Commissioner Org") {
    code_col <- "Commissioner Org Code"
    name_col <- "Commissioner Org Name"
  }

  codes <- org_lkp |>
    dplyr::select(
      dplyr::all_of(
        c(code_col, name_col)
      )
    ) |>
    dplyr::rename(
      code = all_of(code_col),
      name = all_of(name_col)
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      .data$name %in% names
    ) |>
    dplyr::pull(.data$code)

  if (length(names) != length(codes)) {
    warning(
      "some names were not translated to codes as they were missing from the lookup"
    )
  }

  return(codes)
}


#' function to create standard names so the subsequent aggregation tasks don't
#' break. This function is focussed on performing analysis for a single
#' selection within the tool, rather than analysing trusts/specialties as a
#' batch process

#' @param trust_parents character; vector of full names for trust parents
#' @param trusts character; vector of full names for trusts
#' @param comm_parents character; vector of full names for commissioner parents
#' @param comms character; vector of full names for commissioners
#' @param spec character; vector of full names for specialties
#' @param nhs_only character; one of "nhs_only", "non_nhs_only" or "all"
#' @param nhs_regions character; vector of NHS regions
#' @returns a nested list with five nests. The five nests are named
#'   trust_parents, trusts, commissioner_parents, commissioners, and
#'   specialties. Within each of these there are 3 items; selected_name,
#'   selected_code and display.
#'
#'   selected_name can be a vector of length one or more, and will contain the
#'   names based on the selected items in the UI. If no items are selected, then
#'   this value will be NULL.
#'
#'   selected_code can be a vector of length one or more, and will contain the
#'   equivalent codes based on the selected items in the UI. If no items are
#'   selected, then this value will be NULL.
#'
#'   display is a vector of length one, and will contain the name that is
#'   subsequently displayed in the chart titles. If multiple or no selections
#'   are made, then this will take the value 'Aggregated'.
#' @noRd
filters_displays <- function(
  nhs_regions,
  nhs_only,
  trust_parents,
  trusts,
  comm_parents,
  comms,
  spec
) {
  nhs_only <- match.arg(
    nhs_only,
    c(
      "nhs_only",
      "non_nhs_only",
      "all"
    )
  )

  selected_trust_parents <- org_name_lkp(
    names = trust_parents,
    type = "Provider Parent"
  )

  if (
    all(
      is.null(trusts),
      is.null(trust_parents),
      is.null(comm_parents),
      is.null(comms)
    )
  ) {
    data_table <- org_lkp

    if (nhs_only == "nhs_only") {
      data_table <- data_table |>
        dplyr::filter(
          grepl("NHS", .data$`Provider Org Name`)
        )
    } else if (nhs_only == "non_nhs_only") {
      data_table <- data_table |>
        dplyr::filter(
          !grepl("NHS", .data$`Provider Org Name`)
        )
    }

    if (!is.null(nhs_regions)) {
      data_table <- data_table |>
        dplyr::filter(
          .data$`NHS Region Name` %in% nhs_regions
        )
    }

    trusts <- data_table |>
      dplyr::pull(.data$`Provider Org Name`) |>
      unique()
  }

  selected_trusts <- org_name_lkp(
    names = trusts,
    type = "Provider Org"
  )

  selected_commissioner_parents <- org_name_lkp(
    names = comm_parents,
    type = "Commissioner Parent"
  )

  selected_commissioners <- org_name_lkp(
    names = comms,
    type = "Commissioner Org"
  )
  selected_specialties <- specialty_lkp |>
    filter(.data$Treatment.Function.Name %in% spec) |>
    pull(.data$Treatment.Function.Code)

  spec <- replace_fun(
    spec,
    treatment_function_codes
  )

  if (
    length(selected_trust_parents) > 1 |
      is.null(selected_trust_parents)
  ) {
    display_trust_parents <- "Aggregated"
  } else {
    display_trust_parents <- trust_parents
  }

  if (
    length(selected_trusts) > 1 |
      is.null(selected_trusts)
  ) {
    display_trusts <- "Aggregated"
  } else {
    display_trusts <- trusts
  }

  if (
    length(selected_commissioner_parents) > 1 |
      is.null(selected_commissioner_parents)
  ) {
    display_commissioner_parents <- "Aggregated"
  } else {
    display_commissioner_parents <- comm_parents
  }

  if (
    length(selected_commissioners) > 1 |
      is.null(selected_commissioners)
  ) {
    display_commissioners <- "Aggregated"
  } else {
    display_commissioners <- comms
  }

  if (
    length(selected_specialties) > 1 |
      length(selected_specialties) == 0
  ) {
    display_specialties <- "Aggregated"
  } else {
    display_specialties <- spec
  }

  return(
    list(
      trust_parents = list(
        selected_name = trust_parents,
        selected_code = selected_trust_parents,
        display = display_trust_parents
      ),
      trusts = list(
        selected_name = trusts,
        selected_code = selected_trusts,
        display = display_trusts
      ),
      commissioner_parents = list(
        selected_name = comm_parents,
        selected_code = selected_commissioner_parents,
        display = display_commissioner_parents
      ),
      commissioners = list(
        selected_name = comms,
        selected_code = selected_commissioners,
        display = display_commissioners
      ),
      specialties = list(
        selected_name = spec,
        selected_code = selected_specialties,
        display = display_specialties
      )
    )
  )
}

local_enframe <- function(named_vector, name, value_name) {
  if (is.null(names(named_vector))) {
    stop("named_vector must have names")
  }

  df <- dplyr::tibble(
    name = names(named_vector),
    value_name = unname(named_vector)
  ) |>
    setNames(
      nm = c(name, value_name)
    )

  return(df)
}


value_box_text <- function(x_val, y_title, y_val, y_val_type, facet = NA) {
  x_val <- format(x_val, "%b %Y")

  y_val_type <- match.arg(
    y_val_type,
    c("number", "percent")
  )

  if (y_val_type == "number") {
    y_val <- format(
      round(
        y_val,
        1
      ),
      big.mark = ",",
      scientific = FALSE
    )
  } else if (y_val_type == "percent") {
    y_val <- paste0(
      formatC(
        100 * y_val,
        format = "f",
        digits = 1
      ),
      "%"
    )
  }

  if (is.na(facet)) {
    out <- p(
      HTML(
        paste0(
          "<strong>Month:</strong> ",
          x_val,
          "<br>",
          "<strong>",
          y_title,
          ":</strong> ",
          y_val
        )
      )
    )
  } else {
    months_waited <- facet

    out <- p(
      HTML(
        paste0(
          "<strong>Month:</strong> ",
          x_val,
          "<br>",
          "<strong>",
          y_title,
          ":</strong> ",
          y_val,
          "<br>",
          "<strong>Months waited: </strong>",
          months_waited
        )
      )
    )
  }

  return(out)
}
