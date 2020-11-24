#' @export
#' @title creation de noms de colonnes valides
#' @description la fonction prend en parametre
#' un vecteur de chaine de caracteres, et la transforme
#' en supprimant l'ensemble des accents, ponctuation, etc.
#' @param z le vecteur de noms a traiter
#' @return le vecteur z assaini
#' @examples
#' m <- system.file(package = "nettoyage",
#'    "examples", "m-chanson.txt")
#' z <- readLines(m, encoding = "UTF-8")
#' as_valid_names(z)
#' @importFrom stringi stri_trans_tolower stri_trans_general
#'  stri_replace stri_replace_all
as_valid_names <- function(z){

  if(!is.character(z)){
    stop("ola, z est sence etre une chaine de caractere")
  }

  x <- stri_trans_tolower(z)
  x <- stri_trans_general(x, "latin-ascii")
  x <- stri_replace(x, regex = "[[:punct:]]+$", replacement = "")
  x <- stri_replace_all(x, regex = "[[:punct:]]+", replacement = "_")
  x <- stri_replace_all(x, regex = "[[:space:]]+", replacement = "_")
  x <- stri_replace_all(x, regex = "^[0-9]+", replacement = "_")
  x <- gsub(pattern = "[_]+", "_", x)
  x <- gsub(pattern = "^_", "", x)
  x <- gsub(pattern = "_$", "", x)
  x
}

