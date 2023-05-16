# pseudo <- function(
#   incomplete_context,
#   #response_variable,
#   factor_names = names(FFF)
# ) {
#   # A 1-factorial context can not be transfered to pseudo factorial context
#   if(all(incomplete_context$nfactors == 1, na.rm = TRUE)) {
#     NULL
#   }
#   # multi-factorial context
#   if (all(incomplete_context$nfactors > 1, na.rm = TRUE)) {
#     #create list with all factor levels
#     level_list <- lapply(factor_names, function(n) unique(incomplete_context[, n]))
#     #create empty list for pseudo factorial context candidates
#     pseudo_candidates_list <- list()
#     #index
#     ind <- 1
#     for(i in 1:length(factor_names)) {
#       for(j in 1:length(level_list[[i]])) {
#         if(all(!is.na(incomplete_context %>% dplyr::filter(!!rlang::sym(factor_names[i]) != level_list[[i]][j])))) {
#           tmp <- incomplete_context %>%
#             dplyr::filter(!!rlang::sym(factor_names[i]) != level_list[[i]][j])
#           tmp2 <- tmp %>%
#             dplyr::select(all_of(factor_names))
#           if (all(apply(tmp2, 2, function(x){length(unique(x))}) > 1)) {
#             pseudo_candidates_list[[ind]] <- tmp
#           } else {
#             pseudo_candidates_list[[ind]] <- NULL
#           }
#         } else {
#           pseudo_candidates_list[[ind]] <- data.frame("Not Complete" = 999)
#         }
#         ind <- ind + 1
#       }
#     }
#
#     max_ <- max(unlist(lapply(pseudo_candidates_list, function(x){dim(x)[1]})))
#
#     if (max_ > 1) {
#       rc <- pseudo_candidates_list[[which(unlist(lapply(pseudo_candidates_list, function(x){dim(x)[1]})) == max(unlist(lapply(pseudo_candidates_list, function(x){dim(x)[1]}))))[1]]]
#     } else {
#       rc <- NULL
#   }
#   rc
#   }
# }
