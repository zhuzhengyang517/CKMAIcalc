# Version naming and build definitions: see NEWS.md (1.0.1 coefficient-correct / 1.0.0 manuscript-reproduction).
#' Predict 10-year mortality and CKM high risk (as percentages)
#'
#' This function adds three columns to the input data frame:
#' \itemize{
#'   \item \code{risk_all_pct}: 10-year all-cause mortality probability (\%)
#'   \item \code{risk_cvd_pct}: 10-year cardiovascular mortality probability (\%)
#'   \item \code{risk_ckm_high_pct}: probability of CKM stage >= 3 (\%)
#' }
#' It requires the same variables as CKMAI calculation, plus \code{Age},
#' \code{Sex}, \code{Smoking}, \code{Diabetes}, \code{Hypertension},
#' \code{EGFR}, \code{UACR} (for CKM model).
#'
#' @param data A data frame with all necessary variables.
#' @param times Numeric vector of follow-up times (in months) for prediction.
#'   Default is 120 (10 years).
#' @param as_percent Logical; if TRUE (default), return percentages; if FALSE, return probabilities (0-1).
#' @return The input data frame with added columns (risk percentages or probabilities).
#' @importFrom rms survest
#' @importFrom stats formula predict
#' @export
predict_risks <- function(data, times = 120, as_percent = TRUE) {
  # 加载模型文件（从包内部读取）
  model_all <- readRDS(system.file("extdata", "CKMAI_nomogram_model.rds", package = "CKMAIcalc"))
  model_cvd <- readRDS(system.file("extdata", "CKMAI_nomogram_model_cvd.rds", package = "CKMAIcalc"))
  ckm_model <- readRDS(system.file("extdata", "CKM_high_logistic.rds", package = "CKMAIcalc"))

  # 确保数据中有 CKMAI 列（如果没有则计算）
  if (!"CKMAI" %in% colnames(data)) {
    data$CKMAI <- compute_CKMAI(data)
  }

  # --- 准备Cox模型所需变量（排除时间和事件列） ---
  cox_vars <- all.vars(formula(model_all))
  time_event_vars <- c("Follow_up", "Mortality_Status")
  cox_vars <- setdiff(cox_vars, time_event_vars)

  missing <- setdiff(cox_vars, colnames(data))
  if (length(missing) > 0) {
    stop(paste("Missing variables needed for Cox model:", paste(missing, collapse=", ")))
  }

  newdata <- data[, cox_vars, drop = FALSE]

  # --- Sex 编码显式映射（补丁 1.0.1）---
  # 外部统一约定：Sex 1=male / 2=female。
  # 全因模型按 1/2 训练（newdata 保持原样）；CVD 模型按 1/0 训练，需映射。
  sex01 <- as.numeric(data$Sex == 1)          # male=1, female=0
  newdata_cvd <- newdata
  if ("Sex" %in% colnames(newdata_cvd)) {
    newdata_cvd$Sex <- sex01
  }

  # 预测全因死亡风险（Sex 1/2，原样）
  surv_all <- rms::survest(model_all, newdata = newdata, times = times)
  risk_all <- 1 - surv_all$surv

  # 预测 CVD 死亡风险（Sex 映射为 1/0）
  surv_cvd <- rms::survest(model_cvd, newdata = newdata_cvd, times = times)
  risk_cvd <- 1 - surv_cvd$surv

  # --- 预测 CKM 高风险（Logistic模型） ---
  ckm_vars <- c("CKMAI", "Age", "Sex", "Smoking", "Diabetes", "Hypertension", "EGFR", "UACR")
  missing_ckm <- setdiff(ckm_vars, colnames(data))
  if (length(missing_ckm) > 0) {
    stop(paste("Missing variables for CKM model:", paste(missing_ckm, collapse=", ")))
  }

  # CKM logistic 为冻结模型，训练时 CKMAI 为 abs 0–100 分数。
  # 为保持其输入尺度（不重拟合），内部用 legacy abs 参数件重算，仅作该模型输入；
  # 对外发布的 CKMAI 仍是 signed（data$CKMAI），legacy abs 不作临床输出。
  legacy_file <- system.file("extdata", "CKMAI_params_legacy_abs.rds", package = "CKMAIcalc")
  ckmai_legacy <- compute_CKMAI(data, params_file = legacy_file)

  ckm_data <- data.frame(
    CKMAI = ckmai_legacy,
    Age = data$Age,
    Sex = as.numeric(data$Sex == 1),   # CKM logistic 按 1/0 训练
    Smoking = as.numeric(data$Smoking),
    Diabetes = as.numeric(data$Diabetes),
    Hypertension = as.numeric(data$Hypertension),
    EGFR = data$EGFR,
    UACR = data$UACR
  )

  risk_ckm <- predict(ckm_model, newdata = ckm_data, type = "response")

  # 根据参数决定输出百分比还是概率
  if (as_percent) {
    data$risk_all_pct <- risk_all * 100
    data$risk_cvd_pct <- risk_cvd * 100
    data$risk_ckm_high_pct <- risk_ckm * 100
    # 同时保留原始概率（可选）
    data$risk_all_prob <- risk_all
    data$risk_cvd_prob <- risk_cvd
    data$risk_ckm_high_prob <- risk_ckm
  } else {
    data$risk_all <- risk_all
    data$risk_cvd <- risk_cvd
    data$risk_ckm_high <- risk_ckm
  }

  return(data)
}
