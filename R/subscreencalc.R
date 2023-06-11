#' (i) Calculation of the results for the subgroups
#'
#' This function systematically calculates the defined outcome for every combination of subgroups
#' up to the given level (max_comb), i.e. the number of maximum combinations of subgroup defining factors.
#' If, e.g., in a study sex, age group (<=60, >60), BMI group (<=25, >25) are of interest, subgroups of level 2
#' would be, e.g, male subjects with BMI>25 or young females, while subgroups of level 3 would be any combination
#' of all three variables.
#'
#' The evaluation function (eval_function) has to defined by the user. The result needs to be a vector of numerical values,
#' e.g., outcome variable(s) and number of observations/subjects. The input of eval_function is a data frame with the same
#' structure as the input data frame (data) used in the subsreencalc call. See example below.
#' Potential errors occurring due to small subgroups should be caught and handled within eval_function.
#' As the eval_function will be called with every subgroup it may happen that there is only one observation or
#' only one treatment arm or only observations with missing data going into the eval_function. There should always be valid
#' result vector be returned (NAs allowed) and no error causing program abort.
#' For a better display the results may be cut-off to a reasonable range. For example: If my endpoint is a hazard ratio
#' that is expected to be between 0.5 and 2 I would set all values smaller than 0.01 to 0.01 and values above 100 to 100.
#'
#'
#' @param data dataframe with study data
#' @param eval_function name of the function for data analysis
#' @param endpoints vector containing the names of the endpoint variables in data
#' @param treat name of variable in data that contains the treatment identfier, defaults to trtp
#' @param subjectid name of variable in data that contains the subject identifier, defaults to subjid
#' @param factors vector containg the names of variables that define the subgroups, defaults to NULL. If set to NULL, all variables in data are used that are not included in subjectid, treat, and endpoints
#' @param min_comb minimum number of factor combination levels to define subgroups, defaults to 1
#' @param max_comb maximum number of factor combination levels to define subgruops, defaults to 3
#' @param nkernel number of kernels for parallelization (defaults to 1)
#' @param par_functions vector of names of functions used in eval_function to be exported to cluster (needed only if nkernel > 1)
#' @param verbose switch on/off output of computational information
#' @param factorial switch on/off calculation of factorial contexts
#' @param use_complement switch on/off calculation of complement subgroups
#' @return an object of type SubScreenResult of the form
#' list(sge=H,
#'      max_comb=max_comb,
#'      min_comb=min_comb,
#'      subjectid=subjectid,
#'      endpoints=endpoints,
#'      treat=treat,
#'      factors=factors,
#'      results_total=eval_function(cbind(F,T)))
#' @keywords subgroup analysis
#' @export subscreencalc
#' @examples
#' # get the pbc data from the survival package
#' require(survival)
#' data(pbc, package="survival")
#' # generate categorical versions of some of the baseline covariates
#' pbc$ageg[!is.na(pbc$age)]        <-
#'    ifelse(pbc$age[!is.na(pbc$age)]          <= median(pbc$age,     na.rm=TRUE), "Low", "High")
#' pbc$albuming[!is.na(pbc$albumin)]<-
#'    ifelse(pbc$albumin[!is.na(pbc$albumin)]  <= median(pbc$albumin, na.rm=TRUE), "Low", "High")
#' pbc$phosg[!is.na(pbc$alk.phos)]  <-
#'    ifelse(pbc$alk.phos[!is.na(pbc$alk.phos)]<= median(pbc$alk.phos,na.rm=TRUE), "Low", "High")
#' pbc$astg[!is.na(pbc$ast)]        <-
#'    ifelse(pbc$ast[!is.na(pbc$ast)]          <= median(pbc$ast,     na.rm=TRUE), "Low", "High")
#' pbc$bilig[!is.na(pbc$bili)]      <-
#'    ifelse(pbc$bili[!is.na(pbc$bili)]        <= median(pbc$bili,    na.rm=TRUE), "Low", "High")
#' pbc$cholg[!is.na(pbc$chol)]      <-
#'    ifelse(pbc$chol[!is.na(pbc$chol)]        <= median(pbc$chol,    na.rm=TRUE), "Low", "High")
#' pbc$copperg[!is.na(pbc$copper)]  <-
#'    ifelse(pbc$copper[!is.na(pbc$copper)]    <= median(pbc$copper,  na.rm=TRUE), "Low", "High")
#' pbc$ageg[is.na(pbc$age)]         <- "No Data"
#' pbc$albuming[is.na(pbc$albumin)] <- "No Data"
#' pbc$phosg[is.na(pbc$alk.phos)]   <- "No Data"
#' pbc$astg[is.na(pbc$ast)]         <- "No Data"
#' pbc$bilig[is.na(pbc$bili)]       <- "No Data"
#' pbc$cholg[is.na(pbc$chol)]       <- "No Data"
#' pbc$copperg[is.na(pbc$copper)]   <- "No Data"
#' #eliminate treatment NAs
#' pbcdat <- pbc[!is.na(pbc$trt), ]
#'# PFS and OS endpoints
#' set.seed(2006)
#' pbcdat$'event.pfs' <- sample(c(0,1),dim(pbcdat)[1],replace=TRUE)
#' pbcdat$'timepfs' <- sample(1:5000,dim(pbcdat)[1],replace=TRUE)
#' pbcdat$'event.os' <- pbcdat$event
#' pbcdat$'timeos' <- pbcdat$time
#'#variable importance for OS for the created categorical variables
#'#(higher is more important, also works for numeric variables)
#' varnames <- c('ageg', 'sex', 'bilig', 'cholg', 'astg', 'albuming', 'phosg')
#' # define function the eval_function()
#' # Attention: The eval_function ALWAYS needs to return a dataframe with one row.
#' #            Include exception handling, like if(N1>0 && N2>0) hr <- exp(coxph(...) )
#' #            to avoid program abort due to errors
#'hazardratio <- function(D) {
#'
#'  HRpfs <- tryCatch(exp(coxph(Surv(D$timepfs, D$event.pfs) ~ D$trt )$coefficients[[1]]),
#'   warning=function(w) {NA})
#'  HRpfs <- 1/HRpfs
#'  HR.pfs <- round(HRpfs, 2)
#'  HR.pfs[HR.pfs > 10]      <- 10
#'  HR.pfs[HR.pfs < 0.00001] <- 0.00001
#'  HRos <- tryCatch(exp(coxph(Surv(D$timeos, D$event.os) ~ D$trt )$coefficients[[1]]),
#'   warning=function(w) {NA})
#'  HRos <- 1/HRos
#'  HR.os <- round(HRos, 2)
#'  HR.os[HR.os > 10]      <- 10
#'  HR.os[HR.os < 0.00001] <- 0.00001
#'  data.frame( HR.pfs, HR.os#, N.of.subjects,N1 ,N2
#'  )
#'}
#'
#'  # run subscreen
#'
#' \dontrun{
#' results <- subscreencalc(data=pbcdat,
#'                      eval_function=hazardratio,
#'                      endpoints = c("timepfs" , "event.pfs", "timeos", "event.os"),
#'                      treat="trt",
#'                      subjectid = "id",
#'                      factors=c("ageg", "sex", "bilig", "cholg", "copperg"),
#'                      use_complement = FALSE,
#'                      factorial = FALSE)
#'
#' # visualize the results of the subgroup screening with a Shiny app
#' subscreenshow(results)}

subscreencalc <- function(
  data,
  eval_function,
  endpoints,
  treat = "trtp",
  subjectid = "subjid",
  factors = NULL,
  min_comb = 1,
  max_comb = 3,
  nkernel = 1,
  par_functions = "",
  verbose = TRUE,
  factorial = FALSE,
  use_complement = FALSE,
  calc_funnel = FALSE, # Sollen Konfidenzintervalle berechnet werden?
  min_start = 2,     # Minimale Subgruppengröße, für die Konfidenzintervalle berechnet werden sollen
  n_support_points = 50,  # Anzahl der Subgruppengrößen, für die Konfidenzintervalle berechnet werden
  nperm = 1000,           # Anzahl Permutationen pro Stuetzpunkt
  alpha = c(0.01,0.05,0.1),
  stratified = TRUE
  ){

  #### WARNING & ERROR MESSAGES ####
  if (!is.data.frame(data)) {
    stop("Error! subscreen: data has to be a dataframe")
  }
  if (!is.function(eval_function)) {
    stop("Error! subscreen: eval_function has to be a function")
  }
  if (!is.null(treat) && (!treat == "") && !(treat %in% names(data))) {
    stop("Error! subscreen: specified treatment variable not in dataframe")
  }
  if (!(subjectid %in% names(data))) {
    stop("Error! subscreen: specified subject variable not in dataframe")
  }
  if (!is.numeric(min_comb)) {
    stop("Error! subscreen: min_comb has to be a number")
  }
  if (!is.numeric(max_comb)) {
    stop("Error! subscreen: max_comb has to be a number")
  }
  if (!is.null(factors) && (length(factors) != sum(factors %in%
                                                   names(data)))) {
    for (item in factors) {
      if (!(item %in% names(data))) {
        stop("Error! subscreen: not all variables in factors included in data")
      }
    }
  }
  if (nkernel > 1) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      cat("Warning: Use of more than one kernel requires package parallel to be installed. \n nkernel set to 1 \n")
    }
  }

  #### FUNCTIONS ####
  createCombinationMatrix <- function(n, k, l) {
    t(do.call(cbind, lapply(k:l, function(x) {
      utils::combn(n,x, tabulate, nbins = n)})))
  }

  sugruCount <- function(M, StAn) {
    return(apply(M, 1, function(x) {
      x = x * StAn
      prod(x[x != 0])
    }))
  }

  sugruCalc <- function(i) {
    m = M[i, ]
    S = character()
    S = append(S, names(FFF)[(1:length(m)) * m])
    d = plyr::ddply(cbind(FFF, TTT), S, eval_function)
    if(use_complement == TRUE) {
      d_comp = plyr::ddply(cbind(FFF, TTT), S, function(x){eval_function(dplyr::anti_join(cbind(FFF,TTT), x, by = colnames(FFF)))})
      names(d_comp)[!names(d_comp) %in% S] <- paste0("Complement_",names(d_comp)[!names(d_comp) %in% S])
    }
    d_N <- plyr::ddply(cbind(FFF,TTT),S,function(x){
      N.of.subjects <- sum(!is.na(x[subjectid]))
      data.frame(N.of.subjects)
    })
    d <- merge(d,d_N)
    if(use_complement == TRUE) {
      d <- merge(d,d_comp)
    }
    nfactors = sum(m)
    h = cbind(nfactors, d)
    return(h)
  }

  combineDataFrame <- function(h, version = 2) {
    if (version == 1) {
      hf = c("nfactors", names(FFF))
      ha = names(h[[1]])
      for (i in 2:dim(M)[1]) ha = union(ha, names(h[[i]]))
      ht = setdiff(ha, hf)
      hn = union(ht, hf)
      for (i in 1:dim(M)[1]) {
        for (j in hn) {
          if (sum(j == names(h[[i]])) == 0) {
            h[[i]][j] = rep(NA, dim(h[[i]])[1])
          }
        }
      }
      H = h[[1]]
      for (i in 2:dim(M)[1]) {
        H = rbind(H, h[[i]])
      }
    }
    if (version == 2) {

      #add FCID_all variable
      for( i in seq_along(h)){h[[i]]$FCID_all <- rep(i,nrow(h[[i]]))}

      H = as.data.frame(data.table::rbindlist(h, use.names = TRUE,
                                              fill = TRUE))

      for (fac in factors) {
        H[[fac]] <- addNA(H[[fac]])
        levels(H[[fac]])[is.na(levels(H[[fac]]))] <- "Not used"
      }
    }
    if (version == 3) {
      H = do.call("rbind", h)
    }
    SGID = 1:dim(H)[1]
    return(cbind(SGID, H))
  }

  TTT <- data[, (colnames(data) %in% c(subjectid, treat, endpoints))]

  if (is.null(factors)) {

    FFF <- data[, !(colnames(data) %in% c(subjectid, treat,
                                         endpoints))]
    factors <- names(FFF)

  } else {
    FFF <- data[, (colnames(data) %in% factors)]
  }

  if (verbose == TRUE) {
    cat("\n",
        "subscreencalc started at ", format(Sys.time(), format = "%F %R %Z"))}


  AnFa <- dim(FFF)[2]
  AaS <- dim(data)[1]
  StAn <- apply(FFF, 2, function(x) length(table(x)))
  pc <- StAn[StAn > 2]
  pt0 <- proc.time()
  M <- createCombinationMatrix(AnFa, min_comb, max_comb)
  AnSu <- sugruCount(M, StAn)
  pt1 <- proc.time()
  rowsM <- dim(M)[1]

  if (verbose == TRUE) {
    cat("\n", "Number of Subjects                     ",AaS,
        "\n", "Number of Subgroup Factors             ",AnFa,
        "\n", "Potential Subgroups                    ",sum(AnSu))
    }

  if (nkernel > 1) {

    clus <- parallel::makeCluster(nkernel)
    parallel::clusterExport(clus, c("FFF", "TTT", "M", "AaS",
                                    "eval_function"), environment())
    parallel::clusterExport(clus, c("ddply"), environment(plyr::ddply))
    parallel::clusterExport(clus, c("use_complement","subjectid"), environment())
    if (all(par_functions != ""))
      parallel::clusterExport(clus, par_functions)
    h <- parallel::parLapplyLB(cl = clus, 1:rowsM, sugruCalc)
    parallel::stopCluster(clus)

  } else {
    h <- sapply(1:rowsM, sugruCalc, simplify = FALSE)
  }

  #create factorial context ids
  pc_ids <- lapply(h, function(x){any(colnames(x)%in% names(pc)) & all(x$nfactors > 1)})
  pc_max_levels <- lapply(h, function(x){max(StAn[names(StAn) %in% colnames(x)[which(colnames(x) %in% factors)]])})
  pc_df <- data.frame(max_level = unlist(pc_max_levels))
  pc_df$FCID_all <- 1:nrow(pc_df)
  z = numeric()

  for (i in 1:length(h)) z[i] = dim(h[[i]])[1]

  pt2 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Non-existent/empty Subgroups           ",sum(AnSu - z),
        "\n", "Existent Subgroups                     ",sum(z),
        "\n\n", "Time for SG Analyses (s)               ", pt2 - pt1)}

  H <- combineDataFrame(h)
  H <- merge(H,pc_df,by="FCID_all")

  pt3 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for creating Data Frames (s)      ", pt3 - pt2)}


  evfu <- eval_function(cbind(FFF, TTT))
  N <- data.frame('N.of.subjects' = sum(!is.na(cbind(TTT,FFF)[subjectid])))
  res <- merge(evfu, N)

  pt3a <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for calculating N.of.subjects (s) ", pt3a - pt3)}

  if (factorial == TRUE) {
    for (i in 1:length(colnames(evfu))) {
      H <- pseudo_contexts(data = H, endpoint = colnames(evfu)[i], factors = factors)
    }
    fc <- H
  } else {
    fc <- H
  }

  pt4 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for factorial context (s)         ", pt4 - pt3a,
        "\n", "Overall time used (HH:MM:SS)           ",
        paste(((pt4 - pt0)[3]/3600)%/%1, (((pt4 - pt0)[3]/60)%/%1)%%60,
              round((pt4 - pt0)[3]%%60, digits = 4), sep = ":"),
        "\n")
  }




  H <- list(sge = fc, max_comb = max_comb, min_comb = min_comb,
            subjectid = subjectid, endpoints = endpoints, treat = treat,
            factors = factors, results_total = res)#, endpoint_params = endpoint_params)

  ##### funnel
  if(calc_funnel == TRUE){
    start.time = Sys.time() #  Zeit festhalten

    min <- min(H$sge$N.of.subjects)
    max <- max(H$sge$N.of.subjects)
    sampsize <- max


    # Generate vector of support points between min and max sample sizes
    sqrtvec = seq(sqrt(min), by = (sqrt(max) - sqrt(min))/n_support_points, length.out = (n_support_points+1))
    nsamp <- matrix(round(sqrtvec^2), nrow = 1)


    # First we only remove covariates, since they are not of interest,
    data_trimmed <- data[,c(treat, endpoints)]

    # Weighted Sampler: creates samples such that each sample has 50% of each treatment level (T = 0,1,0,1,0,...)

    weightedSampler <- function(dat, treat = 'T', size = 10){
      trts <- unique(dat[,treat])
      trtlevels <- as.numeric(length(unique(dat[,treat])))
      sizePerTrt <- round(size/trtlevels)

      trtlvl1 <- as.numeric(unlist(unique(dat[,treat]))[1])
      trtlvl2 <- as.numeric(unlist(unique(dat[,treat]))[2])

      trt1 <- slice_sample(dat[dat[,treat]== trtlvl1,] ,n =sizePerTrt)
      trt2 <- slice_sample(dat[dat[,treat]== trtlvl2,] ,n =sizePerTrt)

      samp <- data.frame(matrix(ncol = ncol(dat), nrow = size))
      colnames(samp) <- colnames(dat)
      samp[2*(1:sizePerTrt),] <- trt1
      samp[(2*(1:sizePerTrt)-1),] <- trt2
      samp

    }

    # Then create large matrix containing nperm of the largest possible sample
    if(stratified){  trts <- unique(data_trimmed[,treat])
                     trtlevels <- as.numeric(length(unique(data_trimmed[,treat])))

                     trtlvl1 <- as.numeric(unlist(unique(data_trimmed[,treat]))[1])
                     trtlvl2 <- as.numeric(unlist(unique(data_trimmed[,treat]))[2])


                     sampsize <- ifelse(sampsize>min(nrow(data_trimmed[data_trimmed[,treat]== trtlvl1,]), nrow(data_trimmed[data_trimmed[,treat]== trtlvl2,])), min(nrow(data_trimmed[data_trimmed[,treat]== trtlvl1,]), nrow(data_trimmed[data_trimmed[,treat]== trtlvl2,])),
                                      sampsize) # ensure that max. sampsize does not exceed min. required sampsize for each trt level
                     all_samples <- replicate(nperm, weightedSampler(data_trimmed, treat, (sampsize)))

                     }
    if(!stratified) all_samples <- replicate(nperm, slice_sample(data_trimmed, n = sampsize))


    # Function SliceR: takes as input a smaller number m <= nrow(df), all_samples matrix and calculates evaluation function for this smaller value
    # Sample
    SliceR <- function(dat,m) {
      to_be_sliced <- data.frame(treat = dat[treat], endpoint = dat[endpoints])
      eval_function(to_be_sliced[1:m,])
    }

    # QuantileR: gets as input the desired slice (size of support point) and outputs the quantiles of SliceR values

    QuantileR <- function(m) quantile(unlist(apply(all_samples,2, SliceR, m = m)), probs = c(alpha/2, 1-(alpha/2)), na.rm = TRUE)
    quantiles <- apply(nsamp, 2, QuantileR)

    point_estimates <- list(intervals = quantiles, nsamp = nsamp)

    time.taken = Sys.time() - start.time
    print(time.taken)
  }
  # add to object if bootstrap is calculated
  if(calc_funnel == TRUE){

    # Points outside
    # values <- fc[colnames(eval_function(data))]

    # below <- pred_lower > values
    # above <- pred_upper < values
    # outside <- below + above

    # list of all predictions

    # predictions = list(upper_predictions = pred_upper, lower_predictions = pred_lower, point_estimates = point_estimates,
                       # below = below, above = above, outside = outside)

    H <- append(H, point_estimates)
  }

  class(H) <- "SubScreenResult"

  if (verbose == TRUE) {
    cat("\n",
        "subscreencalc stopped at ", format(Sys.time(), format = "%F %R %Z"), "\n")}


  H
}

