# =============================================================================
# tests/S5_assertions.R — 五条回归断言（对源码包 load_all 后实跑，确定性）
# 运行: C:\R-4.5.2\bin\x64\Rscript.exe tests/S5_assertions.R
# =============================================================================
suppressMessages({ library(pkgload); library(readxl); library(pROC) })
PKG <- "C:/Users/11492/Desktop/自己的包/CKMAIcalc"
S5 <- "C:/Users/11492/Desktop/科研思路/生物学年龄/返修2/_S5_代码修复_20261125"
DATA <- "C:/Users/11492/Desktop/科研思路/生物学年龄/返修2/S1_Data.xlsx"
load_all(PKG, quiet=TRUE)

d <- as.data.frame(read_excel(DATA))
res <- predict_risks(d, times=120, as_percent=TRUE)
fem <- res[res$Sex == 2, ]; male <- res[res$Sex == 1, ]
ckmhi <- as.numeric(res$CKM >= 3)

# 模型（早加载）
model_all <- readRDS(system.file("extdata","CKMAI_nomogram_model.rds",package="CKMAIcalc"))
model_cvd <- readRDS(system.file("extdata","CKMAI_nomogram_model_cvd.rds",package="CKMAIcalc"))
ckm_model <- readRDS(system.file("extdata","CKM_high_logistic.rds",package="CKMAIcalc"))

# A1: 女性 CVD 10 年 <= 全因 10 年，违例 0
v1 <- sum(fem$risk_cvd_pct > fem$risk_all_pct)
# A2: 女性 CVD 均值
a2 <- mean(fem$risk_cvd_pct)
# A3: 女性 CKM logistic 均值（现喂 legacy abs，训练尺度）+ logistic 输出 AUC
a3 <- mean(fem$risk_ckm_high_pct)
auc <- as.numeric(roc(ckmhi, res$risk_ckm_high_pct, quiet=TRUE)$auc)
# A4: 女性全因均值
a4 <- mean(fem$risk_all_pct)

# A5: 与独立参考（直接对模型做同一映射）逐行 max_abs_diff = 0
cox_vars <- setdiff(all.vars(formula(model_all)), c("Follow_up","Mortality_Status"))
nd <- res[, cox_vars, drop=FALSE]; ndc <- nd; ndc$Sex <- as.numeric(res$Sex==1)
ref_all <- (1 - rms::survest(model_all, newdata=nd, times=120)$surv)*100
ref_cvd <- (1 - rms::survest(model_cvd, newdata=ndc, times=120)$surv)*100
legacy_file <- system.file("extdata","CKMAI_params_legacy_abs.rds", package="CKMAIcalc")
ckmai_legacy <- compute_CKMAI(d, params_file=legacy_file)
ckd <- data.frame(CKMAI=ckmai_legacy, Age=res$Age, Sex=as.numeric(res$Sex==1),
  Smoking=res$Smoking, Diabetes=res$Diabetes, Hypertension=res$Hypertension,
  EGFR=res$EGFR, UACR=res$UACR)
ref_ckm <- predict(ckm_model, newdata=ckd, type="response")*100
a5 <- max(abs(ref_all-res$risk_all_pct), abs(ref_cvd-res$risk_cvd_pct),
          abs(ref_ckm-res$risk_ckm_high_pct))

out <- data.frame(Assertion=c(
  "A1 female CVD<=all-cause violations (expect 0)",
  "A2 female mean CVD % (expect 4.25)",
  "A3 female mean CKM % (legacy abs input; expect 11.48)",
  "A3 CKM logistic AUC vs CKM>=3 (expect 0.8383)",
  "A4 female mean all-cause % (expect 11.73)",
  "A5 max abs diff vs independent reference (expect 0)"),
  Observed=c(v1, round(a2,4), round(a3,4), round(auc,4), round(a4,4), round(a5,8)),
  Pass=c(v1==0, abs(a2-4.25)<0.05, abs(a3-11.48)<0.05,
         abs(auc-0.8383)<0.005, abs(a4-11.73)<0.05, a5<1e-6))
print(out)
cat("\nALL PASS:", all(out$Pass), "| n female:", nrow(fem), " n male:", nrow(male), "\n")
write.csv(out, file.path(S5,"A4_断言结果.csv"), row.names=FALSE, fileEncoding="UTF-8")
