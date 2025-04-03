#' Compare user data with package data
#'
#' This function allows users to compare their own AMR data with the data
#' included in the package. It standardizes the user data using the provided
#' mapping and performs comparisons based on specified parameters.
#'
#' @param user_data A data frame containing user AMR data
#' @param mapping A named list mapping user's column names to standard names
#' @param comparison_vars Variables to use for comparison (default: pathogen_name, antibiotic_name)
#' @param reference_data The reference dataset to compare with (default: human_amr)
#'
#' @return A list containing comparison results
#' @export
#'
#' @examples
#' \dontrun{
#' # Load your own data
#' my_data <- read.csv("my_amr_data.csv")
#'
#' # Create a mapping from your columns to standard names
#' my_mapping <- list(
#'   "Study ID" = "study_id",
#'   "Bacteria" = "pathogen_name",
#'   "Drug" = "antibiotic_name",
#'   "Total" = "sample_count",
#'   "Resistant" = "resistant_count"
#' )
#'
#' # Compare with package data
#' comparison <- compare_with_package_data(my_data, my_mapping)
#' }
compare_with_package_data <- function(user_data,
                                      mapping,
                                      comparison_vars = c("pathogen_name", "antibiotic_name"),
                                      reference_data = NULL) {
  # Load the reference data if not provided
  if (is.null(reference_data)) {
    data(human_amr, envir = environment())
    reference_data <- human_amr
  }

  # Standardize the user data
  std_user_data <- standardize_amr_data(user_data, mapping)

  # Check that comparison variables exist in both datasets
  missing_vars <- comparison_vars[!comparison_vars %in% names(std_user_data)]
  if (length(missing_vars) > 0) {
    stop("Missing comparison variables in user data: ",
         paste(missing_vars, collapse = ", "))
  }

  missing_vars <- comparison_vars[!comparison_vars %in% names(reference_data)]
  if (length(missing_vars) > 0) {
    stop("Missing comparison variables in reference data: ",
         paste(missing_vars, collapse = ", "))
  }

  # Group data by comparison variables and calculate summary statistics
  user_summary <- std_user_data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(comparison_vars))) %>%
    dplyr::summarise(
      user_studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      user_samples = sum(sample_count, na.rm = TRUE),
      user_resistant = sum(resistant_count, na.rm = TRUE),
      user_rate = user_resistant / user_samples,
      .groups = "drop"
    )

  ref_summary <- reference_data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(comparison_vars))) %>%
    dplyr::summarise(
      ref_studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      ref_samples = sum(sample_count, na.rm = TRUE),
      ref_resistant = sum(resistant_count, na.rm = TRUE),
      ref_rate = ref_resistant / ref_samples,
      .groups = "drop"
    )

  # Join summaries
  comparison <- dplyr::full_join(
    user_summary,
    ref_summary,
    by = comparison_vars
  )

  # Calculate differences
  comparison <- comparison %>%
    dplyr::mutate(
      rate_diff = user_rate - ref_rate,
      rel_diff = (user_rate - ref_rate) / ref_rate * 100
    )

  # Organize results
  result <- list(
    comparison_table = comparison,
    shared_categories = comparison %>%
      dplyr::filter(!is.na(user_rate) & !is.na(ref_rate)) %>%
      nrow(),
    user_only_categories = comparison %>%
      dplyr::filter(!is.na(user_rate) & is.na(ref_rate)) %>%
      nrow(),
    ref_only_categories = comparison %>%
      dplyr::filter(is.na(user_rate) & !is.na(ref_rate)) %>%
      nrow(),
    user_data_summary = dplyr::summarise(
      std_user_data,
      studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      total_samples = sum(sample_count, na.rm = TRUE),
      total_resistant = sum(resistant_count, na.rm = TRUE),
      overall_rate = total_resistant / total_samples
    ),
    ref_data_summary = dplyr::summarise(
      reference_data,
      studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      total_samples = sum(sample_count, na.rm = TRUE),
      total_resistant = sum(resistant_count, na.rm = TRUE),
      overall_rate = total_resistant / total_samples
    )
  )

  return(result)
}

#' Create comparison plot
#'
#' Creates a visual comparison between user data and reference data
#'
#' @param comparison_result Result from compare_with_package_data function
#' @param plot_type Type of plot to create: "scatter", "bar", or "forest"
#'
#' @return A ggplot2 object
#' @export
plot_amr_comparison <- function(comparison_result, plot_type = "scatter") {
  # Get comparison table
  comp_data <- comparison_result$comparison_table

  # Make sure ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is needed for this function")
  }

  if (plot_type == "scatter") {
    # Create scatter plot of resistance rates
    p <- ggplot2::ggplot(comp_data, ggplot2::aes(x = ref_rate, y = user_rate)) +
      ggplot2::geom_point(ggplot2::aes(size = user_samples), alpha = 0.7) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(
        x = "Package data resistance rate",
        y = "User data resistance rate",
        title = "Comparison of resistance rates"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1))

  } else if (plot_type == "bar") {
    # Prepare data for bar plot
    plot_data <- comp_data %>%
      tidyr::pivot_longer(
        cols = c("user_rate", "ref_rate"),
        names_to = "source",
        values_to = "rate"
      ) %>%
      dplyr::filter(!is.na(rate))

    # Get variable names for labels
    var_names <- names(comp_data)[1:2]

    # Create bar plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data[[var_names[1]]], y = rate, fill = source)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::facet_wrap(ggplot2::vars(.data[[var_names[2]]]), scales = "free_x") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(
        y = "Resistance rate",
        fill = "Data source",
        title = "Comparison of resistance rates by category"
      ) +
      ggplot2::scale_fill_manual(
        values = c("user_rate" = "steelblue", "ref_rate" = "darkred"),
        labels = c("user_rate" = "User data", "ref_rate" = "Package data")
      )

  } else if (plot_type == "forest") {
    # Prepare data for forest plot
    plot_data <- comp_data %>%
      dplyr::filter(!is.na(user_rate) & !is.na(ref_rate))

    # Get variable names for labels
    var_names <- names(comp_data)[1:2]

    # Create combined labels
    plot_data$label <- paste(plot_data[[var_names[1]]], "-",
                             plot_data[[var_names[2]]])

    # Create forest plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = rate_diff, y = label)
    ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::geom_point(ggplot2::aes(size = user_samples)) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = rate_diff - 0.1, xmax = rate_diff + 0.1),
        height = 0.2
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "Difference in resistance rate (User - Package)",
        y = "",
        title = "Forest plot of resistance rate differences"
      )
  } else {
    stop("Invalid plot type. Use 'scatter', 'bar', or 'forest'")
  }

  return(p)
}
