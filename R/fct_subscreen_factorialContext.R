#' #' Returns the factorial context of a specific subgroup
#' #'
#' #' @param data The "SubScreenResult" object generated via function 'subscreencalc'.
#' #' @param SGID Subgroup id(s) of the subgroup for which the factorial context is/are requested.
#' #'
#' #' @return List of factorial context information.
#' #'
#' #' @example factorialContext(results,18)
#' #' @example factorialContext(results,c(19,20))
#' #'
#'
#'
#' # Funtion factorialContext is outdated and should be removed (in mod_asmus_v1.R & app.R) by the context calculation in subcreencalc (17MAY2022)
#' #
#' factorialContext <- function(data, SGID) {
#'   SGID <- SGID[1]
#'   if (is.null(SGID) | is.integer0(SGID)){} else {
#'
#'     nfac <- data$sge[which(data$sge$SGID == SGID), ]$nfactors
#'     tmp <- colnames(data$sge[which(data$sge$SGID == SGID), data$factors])[which(data$sge[data$sge$SGID == SGID, data$factors] != "Not used")]
#'     tmp2 <- data$sge[apply(data$sge[ , c("SGID","nfactors", tmp)] != "Not used", 1, sum) == (2 + nfac), ]
#'     tmp3 <- tmp2[tmp2$nfactors == nfac,]
#'     ges <- 1
#'     if (length(tmp) > 0) {
#'       for(i in 1:length(tmp)) {
#'         ges <- sum(levels(data$sge[[tmp[i]]]) != "Not used") * ges
#'       }
#'     } else {
#'        ges <- 0
#'     }
#'
#'     status_ <- ifelse(ges == dim(tmp3)[1], "Complete", "Incomplete")
#'
#'     return(list(
#'       'Factorial' = tmp3,
#'       'Number Factors' = nfac,
#'       'Variables' = tmp,
#'       'Status' = status_
#'       )
#'     )
#'   }
#' }
#'
