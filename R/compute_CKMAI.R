#' Compute CKMAI (Composite Kidney-Metabolic-Aging Index)
#'
#' Calculate CKMAI scores for new data using pre-trained parameters.
#' The function automatically converts raw categorical variables into dummy variables
#' based on the coding rules used in the training set.
#'
#' @param data A data frame containing the required variables.
#'   The following columns are required:
#'   \itemize{
#'     \item \strong{Continuous variables}: \code{Age}, \code{RDW}, \code{MCV},
#'       \code{ALB}, \code{ALP}, \code{EGFR}, \code{LYM}, \code{UACR},
#'       \code{SCR}, \code{WBC}, \code{BUN}
#'     \item \strong{Categorical variables} (raw or dummy):
#'       \code{Sex} (1=male, 2=female), \code{PIR} (1-3),
#'       \code{Ethnicity} (1-3), \code{Marital_Status} (1-2),
#'       \code{Education} (1-3), \code{Diabetes} (0/1), \code{Smoking} (0/1)
#'   }
#'   If raw categorical variables are provided, they will be transformed
#'   to dummy variables as:
#'   \code{Sex1=1} if \code{Sex==1}, \code{PIR3=1} if \code{PIR==3},
#'   \code{Ethnicity2=1} if \code{Ethnicity==2},
#'   \code{Marital_Status2=1} if \code{Marital_Status==2},
#'   \code{Education3=1} if \code{Education==3},
#'   \code{Diabetes1=1} if \code{Diabetes==1},
#'   \code{Smoking1=1} if \code{Smoking==1}.
#' @param params_file Optional path to a custom parameter RDS file.
#'   If \code{NULL} (default), the built-in parameters from the package are used.
#' @return A numeric vector of CKMAI scores.
#'   The scores are transformed to a 0–100 scale, where higher values indicate
#'   higher composite risk. Note that values may fall outside [0,100] if
#'   new data are more extreme than the training set.
#' @examples
#' # Example with raw categorical variables
#' newdata <- data.frame(
#'   Age = 70, Sex = 1, Ethnicity = 2, Education = 3, Marital_Status = 1,
#'   PIR = 3, Smoking = 0, Diabetes = 1, WBC = 7.5, LYM = 29.4, MCV = 90.3,
#'   RDW = 12.5, ALB = 5, ALP = 48, BUN = 25, UACR = 4.012, EGFR = 67.926,
#'   SCR = 1.2
#' )
#' compute_CKMAI(newdata)
#'
#' # Example with dummy variables already present
#' newdata2 <- data.frame(
#'   Age = 55, WBC = 6.2, LYM = 22.1, MCV = 88.7, RDW = 13.1,
#'   ALB = 4.2, ALP = 62, BUN = 18, SCR = 0.95, UACR = 3.22,
#'   EGFR = 81.2, Sex1 = 0, PIR3 = 0, Ethnicity2 = 1,
#'   Marital_Status2 = 1, Education3 = 0, Diabetes1 = 0, Smoking1 = 0
#' )
#' compute_CKMAI(newdata2)
#' @seealso \code{\link{CKMAIcalc}} for package overview.
#' @export
compute_CKMAI <- function(data, params_file = NULL) {
  # 加载参数
  if (is.null(params_file)) {
    params_file <- system.file("extdata", "CKMAI_params.rds", package = "CKMAIcalc")
    if (!file.exists(params_file)) {
      stop("Built-in parameter file not found. Reinstall the package or specify params_file.")
    }
  }
  params <- readRDS(params_file)

  # 定义哑变量映射规则（与训练集一致）
  cat_vars <- list(
    Sex = list(dummy = "Sex1", value = 1),
    PIR = list(dummy = "PIR3", value = 3),
    Ethnicity = list(dummy = "Ethnicity2", value = 2),
    Marital_Status = list(dummy = "Marital_Status2", value = 2),
    Education = list(dummy = "Education3", value = 3),
    Diabetes = list(dummy = "Diabetes1", value = 1),
    Smoking = list(dummy = "Smoking1", value = 1)
  )

  # 自动生成哑变量（如果原始变量存在）
  for (orig in names(cat_vars)) {
    if (orig %in% colnames(data)) {
      dummy_name <- cat_vars[[orig]]$dummy
      value <- cat_vars[[orig]]$value
      data[[dummy_name]] <- as.numeric(data[[orig]] == value)
    } else {
      # 如果原始变量不存在，检查哑变量是否已存在
      if (!(cat_vars[[orig]]$dummy %in% colnames(data))) {
        stop(paste("Missing variable:", orig, "or", cat_vars[[orig]]$dummy))
      }
    }
  }

  # 检查所有必需特征是否齐全
  required <- params$features
  missing <- setdiff(required, colnames(data))
  if (length(missing) > 0) {
    stop("Missing required features: ", paste(missing, collapse=", "))
  }

  # 标准化（使用训练集的均值和标准差）
  std_mat <- matrix(NA, nrow = nrow(data), ncol = length(required))
  colnames(std_mat) <- required
  for (v in required) {
    x <- data[[v]]
    if (is.factor(x)) x <- as.numeric(as.character(x))
    if (is.character(x)) x <- as.numeric(x)
    std_mat[, v] <- (x - params$mean[v]) / params$sd[v]
  }

  # 计算原始得分并映射到 0-100
  raw <- as.vector(std_mat %*% params$weights)
  ckmai <- 100 * (raw - params$train_min) / (params$train_max - params$train_min)
  return(ckmai)
}
