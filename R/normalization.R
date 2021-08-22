#' File preparation
#' 
#' Prepare a single microarray dataset for plotting.
#' Reading data and selecting columns of interest.
#' 
#' @param file_name Name of a delimited file containing
#'                  at least SPOT, CH1B, CH1I, CH2I and
#'                  CH2B columns.
#' @return Tibble containing SPOT, CH1B, CH1I, CH2I,
#'         CH2B and name columns.
#' @export
prepare_single_microarray <- function(file_name) {
  file_name %>% read.delim() %>% tibble::as_tibble() %>%
    dplyr::select(SPOT, CH1B, CH1I,
                  CH2I, CH2B) %>%
    dplyr::mutate(name = file_name) %>%
    return()
}


#' Remove noise
#' 
#' Subtract background noise from every channel.
#' 
#' @param microarray A tibble containing at least SPOT, CH1B,
#'                   CH1I, CH2I, CH2B and name columns.
#' @return Tibble containing input columns and two
#'         new columns CH1 and CH2. All values <= 0
#'         become 1.
#' @export
subtract_background <- function(microarray) {
  result <- dplyr::mutate(microarray,
                          CH1 = CH1I - CH1B,
                          CH2 = CH2I - CH2B
  )
  
  # Extinction < 0 does not make sense; such values must be removed.
  result$CH1[result$CH1 <= 0] <- 1
  result$CH2[result$CH2 <= 0] <- 1
  
  return(result)
}


#' Transform channels
#' 
#' Add (M)ean and (A)verage to a tibble containing CH1 and CH2 columns.
#' 
#' @param microarray A tibble containing at least CH1, CH2 and name columns.
#' @return Same table with two new columns: M(ean) and A(verage).
#' @export
mean_average <- function(microarray) {
  microarray %>% dplyr::group_by(name) %>%
    dplyr::mutate(M = log2(CH2) - log2(CH1),
                  A = (log2(CH2) + log2(CH1)) / 2) %>%
    dplyr::ungroup() %>% return()
}  


#' Normalization
#' 
#' Normalize mean and average.
#'
#' @param mocroarray A table containing at least M, A and name columns.
#' @return Same table normalized and with NA removed.
#' @export
normalize <- function(microarray) {
  microarray %>% dplyr::group_by(name) %>%
    dplyr::mutate(M = M - (coef(lm(M ~ A))[1] + coef(lm(M ~ A))[2] * A)) %>%
    dplyr::ungroup() %>% na.omit() %>% return()
  
  # lm1 <- lm(microarray$M ~ microarray$A)
  # microarray$M <- microarray$M - (coef(lm1)[1] + coef(lm1)[2] * microarray$A)
  # microarray %>% na.omit() %>% return()
}


#' Purge
#' 
#' Remove all columns except M, name and SPOT.
#' 
#' @param microarray A tibble containing M, name and SPOT columns.
#' @return A tibble containing only  M, name and SPOT columns.
#' @export
purge <- function(microarray) {
  microarray %>% dplyr::select(M, name, SPOT) %>% return()
}


#' Merge datasets
#' 
#' Merge *.tab datasets from a directory. Safe them as tibble with
#' M, name and SPOT columns. Secure switching back to the user-defined
#' file directory.
#' 
#' @param directory=getwd() Working directory with delimited files.
#' @param file_ext="tab" File extension of delimited files containing
#'                       M, name and SPOT columns.
#' @param step="between" Determine step of normalization: "between" means
#'                       between array step.
#' @return A tibble containing M, name and SPOT columns, if step = "between",
#'         or CH1, CH2, name and SPOT in other cases.
#' @export
merge_datasets <- function(directory = getwd(), file_ext = "tab", step = "between") {
  old_directory <- getwd()
  setwd(directory)
  
  # Join datasets.
  if(step == "between") {
    result <- tibble::tibble(M = NA,
                             name = NA,
                             SPOT = NA
    )
    for (dataset in dir(pattern = paste(".*.", file_ext, sep = ""))) {
      buffer <- dataset %>%
        prepare_single_microarray() %>%
        subtract_background() %>%
        mean_average() %>%
        normalize() %>%
        purge()
      result <- dplyr::add_row(result, buffer)
    }
  } else {
    result <- tibble::tibble(CH1 = NA,
                             CH2 = NA,
                             name = NA,
                             SPOT = NA
    )
    for (dataset in dir(pattern = paste(".*.", file_ext, sep = ""))) {
      buffer <- dataset %>%
        prepare_single_microarray() %>%
        subtract_background() %>%
        select(CH1, CH2, name, SPOT)
      result <- dplyr::add_row(result, buffer)
    }
  }  
  # Switch back to the user-defined file directory.
  setwd(old_directory)
  
  return(na.omit(result))
}


#' Centering
#' 
#' Centering step of between array normalization.
#' Subtract median from each value, so the medians become 0.
#' 
#' @param microarrays A tibble containing M and name columns.
#' @return Same tibble with M column centered.
#' @export
center_norm <- function(microarrays) {
  microarrays %>% dplyr::group_by(name) %>%
    dplyr::mutate(med = median(M)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(M = M - med) %>%
    dplyr::select(-med) %>% return()
}


#' Scaling
#' 
#' Scaling step of between array normalization.
#' Divide each value through MAD, so std. deviations become 1.
#' 
#' @param microarrays A tibble containing M and name columns.
#' @return Same tibble with  M column scaled.
#' @export
scale_norm <- function(microarrays) {
  microarrays %>% dplyr::group_by(name) %>%
    dplyr::mutate(mad = mad(M)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(M = M / mad) %>%
    dplyr::select(-mad) %>% return()
}


#' Distribution normalization
#' 
#' Sort all values in ascending order, build a mean value for each
#' position in the order, so distributions of all experiments become
#' the same.
#' 
#' @param microarrays A tibble containing M and name columns.
#' @return A new tibble with "number" and M columns, M column
#'         is normalized.
#' @export
distribution_norm <- function(microarrays) {
  microarrays %>% dplyr::arrange(M) %>% dplyr::group_by(name) %>%
    dplyr::mutate(number = row_number()) %>%
    dplyr::ungroup() %>% dplyr::group_by(number) %>%
    dplyr::summarise(M = mean(M)) %>% return()
}

#' Run App
#' 
#' Run Shiny App for more comfortable plotting.
#' @export
run_app <- function() {
  shiny::runApp()
}