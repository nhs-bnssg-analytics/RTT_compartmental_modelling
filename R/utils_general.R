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
    dplyr::mutate(
      perf = dplyr::case_when(
        .data$months_waited_id < target_bin ~ "Below",
        .default = "Above"
      )
    ) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = dplyr::all_of(c("period", "perf", current_groupings))
    ) |>
    dplyr::mutate(
      prop = .data$value / sum(.data$value),
      .by = dplyr::all_of(c("period", current_groupings))
    ) |>
    dplyr::filter(
      .data$perf == "Below"
    ) |>
    dplyr::select(
      dplyr::all_of(
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

#' Calculates performance by period from a given data set and target stock
#'
#' @param incompletes_data tibble; requires column headers of "period",
#'   "months_waited_id" and "value", where value is the count of incomplete
#'   pathways by period and the number of months the patients have waited
#' @param target_bin integer; the number of months waited where patients that
#'   have waited for greater or equal to that number of months have breached
#'   performance
#' @param target_performance numeric of length 1; must be between 0 and 1. The
#'   value is the number of patients on the waiting list that have waited for
#'   less than the `target_bin` time, as a proportion of the total waiting
#'   list
#' @return A two column tibble containing "period" and "shortfall" columns, where
#'   shortfall is the number of additional patients that are on the waiting list
#'   and have been waiting longer than the `target_bin` length that, if removed,
#'   will results in a performance equal to the `target_performance`
#' @noRd
calc_shortfall <- function(
  incompletes_data,
  target_bin = 4,
  target_performance = 0.92
) {
  # check target_performance between 0 and 1
  if (!dplyr::between(target_performance, 0, 1)) {
    stop("target_performance must be between 0 and 1")
  }

  # check target_bin within the range of bins available
  if (!(target_bin %in% incompletes_data[["months_waited_id"]])) {
    stop("target_bin not a valid month waited in the incompletes_data")
  }

  current_groupings <- dplyr::group_vars(
    incompletes_data
  )

  # ungroup so the following methods work
  incompletes_data <- dplyr::ungroup(
    incompletes_data
  )

  shortfall_tbl <- incompletes_data |>
    dplyr::summarise(
      wl_total = sum(.data$value),
      wl_above_target = sum(.data$value[.data$months_waited_id >= target_bin]),
      .by = dplyr::all_of(c("period", current_groupings))
    ) |>
    dplyr::mutate(
      shortfall = (.data$wl_above_target -
        ((1 - target_performance) * .data$wl_total)) /
        target_performance
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("period", current_groupings, "shortfall")
      )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          current_groupings
        )
      )
    )

  return(shortfall_tbl)
}

#' Calculate the percentile waiting at a given week
#' @param wl_shape a tibble with columns for the number of months waited (where 0 is 0-1 month)
#'  and volume waiting
#' @param week numeric; the week to calculate the percentile for
#' @param wlsize_col character length 1; the name of the column containing the volume waiting
#' @param time_col character length 1; the name of the column containing the number of months waiting
calc_percentile_at_week <- function(
  wl_shape,
  week,
  wlsize_col = "wlsize",
  time_col = "months_waited_id"
) {
  mnth <- convert_weeks_to_months(week)

  remainder <- mnth %% 1

  if (sum(wl_shape[[wlsize_col]]) != 0) {
    if (mnth != 0) {
      whole_months <- wl_shape |>
        filter(!!rlang::sym(time_col) %in% 0:(floor(mnth) - 1)) |>
        pull(!!rlang::sym(wlsize_col)) |>
        sum()

      remainder_months <- wl_shape |>
        filter(!!rlang::sym(time_col) %in% floor(mnth)) |>
        pull(!!rlang::sym(wlsize_col)) |>
        (\(x) x * remainder)()
      # whole_months <- sum(wl_shape[[wlsize_col]])[0:floor(mnth)]
      # remainder_months <- remainder * wl_shape[[wlsize_col]][floor(mnth) + 1]

      total_wl <- sum(wl_shape[[wlsize_col]])

      perc_calc <- (whole_months + remainder_months) / total_wl
    } else {
      perc_calc <- 0
    }
  } else {
    perc_calc <- NA
  }

  return(perc_calc)
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

#' extracts the first percentage value in a string
#' @noRd
extract_percent <- function(text) {
  # Use regular expression to find percentage values
  matches <- regmatches(text, gregexpr("\\d+(\\.\\d+)?%", text))[[1]]

  # Return the first match if any found
  if (length(matches) > 0) {
    return(as.numeric(gsub("%", "", matches[1])))
  } else {
    return(NA) # Return NA if no percentage found
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

convert_weeks_to_months <- function(wks) {
  dys_in_year <- 365.25
  mnths_in_year <- dys_in_year / 12

  mnths <- wks * 7 / mnths_in_year

  return(mnths)
}

#' Replaces values in a string vector with corresponding values from a named
#' vector
#' @param string_vector A character vector
#' @param replacement_vector A named vector where names correspond to values in
#'   string_vector, and can be regular expressions, and values are the
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
      grepl(paste0("^", pattern, "$"), replaced_vector),
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

#' Create a text string for the latest performance based on the r$all_data dataset
#' @param data the r$all_data dataset
#' @noRd
#' @importFrom dplyr filter pull mutate
latest_performance_text <- function(data) {
  text <- data |>
    filter(
      .data$type == "Incomplete",
      .data$period == max(.data$period)
    ) |>
    calc_performance(
      target_bin = 4
    ) |>
    mutate(
      text = paste0(
        # "The performance at ",
        format(.data$period, '%b %Y'),
        ": ",
        format(
          100 * .data$prop,
          format = "f",
          digits = 2,
          nsmall = 1
        ),
        "%"
      )
    ) |>
    pull(.data$text)

  return(text)
}

#' @param data_source can be "upload" or "download"
#' @noRd
nov24_performance_text <- function(
  data,
  trust_parent_codes = NULL,
  trust_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  specialty_codes = NULL,
  data_source
) {
  # november 2024 target ---------------------------------------------------

  if (as.Date("2024-11-01") %in% data$period) {
    nov_24_data <- data |>
      dplyr::filter(
        .data$period == as.Date("2024-11-01"),
        .data$type == "Incomplete"
      )
  } else {
    if (data_source == "download") {
      shiny::withProgress(
        message = "Downloading Nov 24 benchmarking data...",
        value = 0,
        {
          nov_24_data <- NHSRtt::get_rtt_data(
            date_start = as.Date("2024-11-01"),
            date_end = as.Date("2024-11-01"),
            trust_parent_codes = trust_parent_codes,
            trust_codes = trust_codes,
            commissioner_parent_codes = commissioner_parent_codes,
            commissioner_org_codes = commissioner_org_codes,
            specialty_codes = specialty_codes,
          ) |>
            filter(.data$type == "Incomplete") |>
            dplyr::summarise(
              value = sum(.data$value),
              .by = c("period", "months_waited")
            ) |>
            mutate(
              months_waited_id = NHSRtt::convert_months_waited_to_id(
                .data$months_waited,
                max_months_waited = 12
              )
            ) |>
            dplyr::summarise(
              value = sum(.data$value),
              .by = c("period", "months_waited_id")
            )
          shiny::incProgress(1, detail = "Complete")
        }
      )
    } else {
      nov_24_data <- tibble(data = numeric())
    }
  }

  if (nrow(nov_24_data) > 0) {
    nov_24_benchmark <- nov_24_data |>
      calc_performance(
        target_bin = 4
      ) |>
      dplyr::pull(.data$prop)

    nov_24_benchmark <- paste0(
      "The performance at Nov 2024 was ",
      format(
        100 * nov_24_benchmark,
        format = "f",
        digits = 2,
        nsmall = 1
      ),
      "%"
    )
  } else {
    nov_24_benchmark <- ""
  }

  return(nov_24_benchmark)
}

#' @param data_source can be "upload" or "download"
#' @noRd
performance_text_planner <- function(
  data,
  trust_parent_codes = NULL,
  trust_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  specialty_codes = NULL,
  data_source
) {
  nov_performance <- nov24_performance_text(
    data,
    trust_parent_codes = NULL,
    trust_codes = NULL,
    commissioner_parent_codes = NULL,
    commissioner_org_codes = NULL,
    specialty_codes = NULL,
    data_source = data_source
  )

  latest_performance <- latest_performance_text(data)

  performance_text <- paste0(
    "<b>",
    nov_performance,
    "</b><br><small><p>",
    latest_performance,
    "</p></small>"
  )

  return(HTML(performance_text))
}

name_with_tooltip <- function(name, definition) {
  wrap_hover_text <- function(text, width = 30) {
    # Split the string into words
    words <- unlist(strsplit(text, " "))
    wrapped <- ""
    line <- ""

    for (word in words) {
      # Check if adding the next word exceeds the width
      if (nchar(line) + nchar(word) + 1 > width) {
        # Add the current line to wrapped text
        wrapped <- paste0(wrapped, line, "\n")
        line <- word
      } else {
        # Add word to the current line
        if (nchar(line) == 0) {
          line <- word
        } else {
          line <- paste(line, word)
        }
      }
    }

    # Add the last line
    wrapped <- paste0(wrapped, line)

    return(wrapped)
  }

  wrapped_definition <- wrap_hover_text(
    definition,
    30
  )

  wrapped_definition <- gsub("^\\n", "", wrapped_definition)

  withTags(
    span(
      name,
      title = wrapped_definition,
      class = "table-headers"
    )
  )
}

cell_colour <- function(currentval, lowval, midval, highval) {
  # check each input length 1
  if (!all(length(lowval) == 1, length(midval) == 1, length(highval) == 1)) {
    stop("All inputs must be length 1")
  }

  # check each input is a named vector
  if (
    any(is.null(names(lowval)), is.null(names(midval)), is.null(names(highval)))
  ) {
    stop("All inputs must have a name")
  }

  # check all names are hex vals
  pat <- "^#(0x|0X)?[a-fA-F0-9]+$"
  if (
    any(
      !grepl(pat, names(lowval)),
      !grepl(pat, names(midval)),
      !grepl(pat, names(midval))
    )
  ) {
    stop("All names must be hex value")
  }

  # Extract numeric values and hex color names
  lv <- as.numeric(lowval)
  lc <- names(lowval)
  mv <- as.numeric(midval)
  mc <- names(midval)
  hv <- as.numeric(highval)
  hc <- names(highval)

  # Convert hex to RGB
  hex_to_rgb <- function(hex) {
    rgb <- grDevices::col2rgb(hex)
    return(as.numeric(rgb))
  }

  # Interpolate between two RGB colors
  interpolate_rgb <- function(val, val1, val2, col1, col2) {
    ratio <- (val - val1) / (val2 - val1)
    rgb1 <- hex_to_rgb(col1)
    rgb2 <- hex_to_rgb(col2)
    rgb_interp <- rgb1 + ratio * (rgb2 - rgb1)
    rgb_interp <- pmax(0, pmin(255, rgb_interp)) # Clamp values
    rgb_interp <- round(rgb_interp)

    rgb_interp <- grDevices::rgb(
      rgb_interp[1],
      rgb_interp[2],
      rgb_interp[3],
      maxColorValue = 255
    )

    return(rgb_interp)
  }

  if (
    any(
      is.na(currentval),
      is.na(lv),
      is.na(mv),
      is.infinite(currentval),
      is.infinite(lv),
      is.infinite(mv)
    )
  ) {
    return("#a3a3a3ff")
  } else {
    # Determine which range to interpolate
    if (currentval <= mv) {
      return(interpolate_rgb(currentval, lv, mv, lc, mc))
    } else {
      return(interpolate_rgb(currentval, mv, hv, mc, hc))
    }
  }
}


definitions <- function() {
  list(
    "Treatment capacity" = "A 'clock stop' as a result of leaving the RTT pathway due to treatment, or for other reasons as described by section 4 of the document linked to at the top of this page.",
    "Renege" = "A 'clock stop' as a result of leaving the RTT pathway for reasons other than treatment. This could be because of inter-provider transfers, or a clock stop that was not captured in the data submissions, for example.",
    "Referral" = "A 'clock start', when an RTT pathway begins.",
    "RTT" = "Referral to Treatment. Here an RTT pathway specifies a time from clock start to clock stop.",
    "Performance" = "The proportion of the RTT waiting list that have been waiting less than 18 weeks (four months).",
    "Waiting list" = "The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock stop').",
    "Skew" = "Adjust the capacity utilisation profile (see above for definition) to focus more on longer waiters than shorter waiters (a skew value of greater than 1), or vice versa (a skew value of less than 1). In all scenarios, it is assumed the people waiting 0-1 months that are treated are 'urgent', and so the capacity utilisation for this group remains unchanged.",
    "Capacity utilisation profile" = "The model calibration process calculates the average rate that people have been treated by the number of months they have been waiting. This is calculated for those waiting up to 1 month, all the way up to those waiting 12+ months. These rates are the 'capacity utilisation profile'.",
    "18 week performance" = "The public data are published monthly, therefore permitting monthly modelling only. 18 weeks is, on average, 5 days less than 4 months. For ease of translating the tool into NHS target terms, the tool presents the 4 month performance as '18 weeks'.",
    "Performance shortfall" = "The number of additional clock stops required to achieve a defined performance target. The clock stops would need to occur on pathways that are longer than the specified time that the target refers to.",
    "Treatment profile" = "The distribution of total treatments by how long patients have waited."
  )
}

#' Provides a tooltip for words that have a definition
#'
#' @param label_text the text in the user interface that requires a
#'   tooltip (it can be any case)
#' @param label_lkp if the text in the user interface isn't directly
#'   in the definitions, another string can be provided here to act
#'   as the lookup
#' @noRd
tooltip_label <- function(label_text, label_lkp = NULL) {
  defs <- definitions()
  names(defs) <- tolower(names(defs))

  if (is.null(label_lkp)) {
    lookup_string <- label_text
  } else {
    lookup_string <- label_lkp
  }

  tt <- sprintf(
    '<span title="%s" style="border-bottom: 1px dotted #999; cursor: help;">%s</span>',
    defs[[tolower(lookup_string)]],
    label_text
  )

  if (length(tt) == 1) {
    tt <- HTML(tt)
  } else {
    tt <- label_text
  }

  return(tt)
}
