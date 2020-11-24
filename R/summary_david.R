#' @importFrom stats quantile
ql_summarize <- function(x){
  ql <- table(x, useNA = "always")
  data.frame(
    stat = names(ql),
    value = as.integer(ql),
    stringsAsFactors = FALSE
  )
}

#' @importFrom tidyselect everything
#' @importFrom dplyr select_if mutate_all group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr `%>%`
get_ql_summary <- function(dat){
  select_if(dat, is.factor) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "value") %>%
    group_by(column) %>%
    summarise(ql_summarize(value), .groups = "drop")
}


qt_summarize <- function(x){
  qt <- quantile(x, probs = seq(0, 1, by = .25))
  avg <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  missing <- sum(is.na(x))
  data.frame(
    stat = c("min", "q1", "median", "q3", "max", "mean", "sd", "missings"),
    value = c(as.double(qt), avg, sd, missing),
    stringsAsFactors = FALSE
  )
}
globalVariables(names = c("column", "value"))

get_qt_summary <- function(dat){
  select_if(dat, is.double) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "value") %>%
    group_by(column) %>%
    summarise(qt_summarize(value), .groups = "drop")
}

#' @importFrom dplyr bind_rows
#' @export
#' @title ma version de summary
#' @description une version tellement plus mieux
#' que celle de la R Core Team.
#' @param dat le dataset a analyser
#' @examples
#' summary_david(iris)
#'
#' if(require("ggplot2")){
#'   summary_david(ggplot2::diamonds)
#' }
#'
summary_david <- function(dat){
  x1 <- get_ql_summary(dat)
  x2 <- get_qt_summary(dat)
  x <- bind_rows(x1, x2)
  x
}
