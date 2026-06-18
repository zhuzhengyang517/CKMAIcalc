#' Compute CKMAI_advance (CKMAI minus Age)
#'
#' @param data A data frame containing all variables required for CKMAI calculation 
#'   (as in \code{compute_CKMAI}) and a column named \code{Age}.
#' @return Numeric vector of CKMAI_advance values.
#' @export
compute_CKMAI_advance <- function(data) {
  if (!"Age" %in% colnames(data)) {
    stop("Column 'Age' not found in data.")
  }
  ckmai <- compute_CKMAI(data)   # 调用已存在的函数
  return(ckmai - data$Age)
}