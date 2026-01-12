# Suppress R CMD check NOTE for column name used in tidyr::nest()
utils::globalVariables("data")

#' Function to generate a table of EC values from a data frame of multiple
#' isolates.
#'
#' @inheritParams get_drm
#' @param result What result do you want returned? Default is "df" for a data
#'   frame of summary values. If you want the models returned, choose "model".
#'   If you want the summary output of the model, choose "summary".
#' @param response a numeric vector specifying what EC values you want to calculate.
#' @param plot if \code{TRUE}, a curve will be plotted for each sample.
#' @param ... parameters passed on to \code{\link[utils]{read.table}} if
#'   \code{x} is a file name.
#' @return a data frame that contains EC estimates and standard errors in
#'   columns and samples in rows.
#' @export
#' @author Zhian N. Kamvar
#' @examples
#' data(dummydata)
#' # Using 3 parameter Log-Logistic Model (default)
#' EC_table(dummydata, form = response ~ dose)
#'
#' # Using 4 parameter Weibull Model.
#' EC_table(dummydata, form = response ~ dose, model = "W2.4")
#'
#' # This function really only needs three columns.
#' newdat <- dummydata[c("ID", "dose", "response")]
#' EC_table(newdat, form = response ~ dose)
#'
#' # We can rename them, too.
#' colnames(newdat) <- c("identity", "dosage", "growth")
#' EC_table(newdat, form = growth ~ dosage, idcol = "identity")
EC_table <- function(x, form = NULL, model = "LL.3",
                     response = c(10, 50, 90), idcol = "ID",
                     result = "df", plot = TRUE, ...){
  RESARGS <- c("df", "model", "summary")
  if (is.null(form)){
  	the_call <- match.call()
  	the_call[["form"]] <- response ~ dose
  	the_call <- utils::capture.output(print(the_call))
  	msg <- paste("please supply a formula.\n\nExample:\n\t", the_call)
    stop(msg)
  }
  result <- match.arg(result, RESARGS)
  if (!is.data.frame(x)){
    dat <- read.table(x, header = TRUE, stringsAsFactors = FALSE, ...)
  } else {
    dat <- x
  }
  variables_exist <- all.vars(form) %in% names(dat)
  if (!all(variables_exist)){
  	dat  <- paste(names(x), collapse = ", ")
    formsg  <- utils::capture.output(print(form))
    msg <- paste("\n\nYou have the following variables in your data:\n\t", dat,
    						 "\n\nThe formula you supplied does not match:\n\t", formsg,
    						 "\n\nPlease correct the formula argument and try again")
    stop(msg)
  }
  models <- dat %>%
    tidyr::nest(.by = dplyr::all_of(idcol)) |>
    dplyr::mutate(model = mapply(
      function(d, id_val) {
        # Add column back
        d[[idcol]] <- id_val
        get_drm(d, model = model, form = form, idcol = idcol)
      }, 
      data, 
      .data[[idcol]], 
      SIMPLIFY = FALSE
    ))

  EC <- dplyr::bind_rows(!!!lapply(models$model, function(m) get_EC(m, response, disp = FALSE)))

  EC <- dplyr::tibble(sample = models[[idcol]]) %>% dplyr::bind_cols(EC)
  if (plot){
    for (i in seq_len(nrow(models))) {
      tryplot(list(models[[idcol]][i], model = models$model[[i]]))
    }
  }
  if (result == "df"){
    return(EC)
  } else {
    res <- models$model
    names(res) <- models[[idcol]]
    if (result == "summary"){
      res <- lapply(res, summary)
    }
    return(res)
  }

}

# internal plotting
tryplot <- function(x){
  if (length(x$model) > 0){
    plot(x$model, broken = TRUE, type = "all", main = x[[1]])
  } else {
    plot.new()
    text(x = 0.5, y = 0.5, paste0(x[[1]], "\n", "Insufficient data"), cex = 1.6,
         col = "black")
  }
}
