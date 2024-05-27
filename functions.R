# FONCTIONS


#' @title donne la décennie d'une année
#' @param ANNEE numeric
#'
#' @return an numeric
#'
#' @examples
#' decennie_a_partir_annee(2014)=2010
decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %% 10)
}

#' Calcule la moyenne, ou autre stat descriptive d'une var
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
calcule_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = T, ...)
  } else if (b == "ecart-type" | b == "sd") {
    x <- sd(a, na.rm = T, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = T, ...)
  }
  return(x)
}
