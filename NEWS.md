# CKMAIcalc — Version 1.0.1 update statement (merged, 2026-11-25)

## Summary

Version 1.0.1 harmonizes the Sex coding across the serialized models and uses
the sign-preserving (signed) Cox coefficients as the published CKMAI index. No
serialized model object was refit or modified.

## Fixes

### Sex encoding

- The external convention is `Sex = 1 (male) / 2 (female)`.
- The all-cause Cox model, trained on 1/2, receives these values unchanged.
- The CVD Cox model and the CKM-high logistic model, trained on 1/0, receive an
  explicit mapping `as.numeric(Sex == 1)` before the call.
- The mapping is applied on every risk path: the base calculation, the dynamic
  panel, the age-sensitivity loop, and the exploratory interaction heatmap,
  dose-response and restricted cubic spline (RCS) panels. The shared helper
  `map_sex01` is used so that a single definition is reused across paths.

### Sign-preserving index

- The published CKMAI index now uses the signed coefficient weights
  (`b / max|b|`), preserving the direction of every component. The six
  protective factors (LYM, albumin, SCR, eGFR, education, PIR) retain their
  negative signs.
- The standardization parameters and the 0–100 training range are the signed
  values (train min/max -3.605196 / 5.055543).

### Frozen CKM-high model and the legacy coefficient file

- The frozen CKM-high logistic model was trained with the original
  absolute-value CKMAI score. To keep this non-refit model on its trained input
  scale, an internal coefficient file is retained as
  `inst/extdata/CKMAI_params_legacy_abs.rds`.
- That file is used only to build the input of the frozen CKM-high logistic
  model; it is not the released index and is not a published or clinical output.
  It reproduces the audited CKM AUC 0.8383 and the female mean of 11.48%.

## Version naming (added 2026-11-25)

Two builds are distinguished so that both the distributed index and the
published article remain reproducible:

* 1.0.1 - coefficient-correct build (current release).
  The CKMAI index is computed with the original signed Cox coefficients,
  preserving the direction of every component. This is the version
  distributed for risk calculation in CKMAIcalc and the online calculator.
  A frozen copy of the previous coefficient file is retained as
  inst/extdata/CKMAI_params_legacy_abs.rds, used only as the input of the
  frozen CKM-high logistic model; it is not the released index.
* 1.0.0 - manuscript-reproduction build (previous release).
  The index was computed with the absolute values of the Cox coefficients
  (code.R, line 648 in that build). This build reproduces the downstream
  quantities reported in the published article (high-risk thresholds, RERI,
  hazard ratios, RCS, Figure 3 and the supplementary tables).

The serialized model objects (all-cause Cox, CVD Cox, CKM-high logistic)
are byte-for-byte identical in both builds. No model was refitted.

## Notes

- The 1.0.0 build has no detectable tag or release in the repository (local or
  remote); its implementation is documented here and its coefficient file is
  archived as `inst/extdata/CKMAI_params_legacy_abs.rds`.
- The external Sex input change (0/1 to 1/2) and the score scale change are
  breaking changes for callers of the previous interface.
- The regression assertions run 26/26 pass, covering the package, the web
  calculator and the three exploratory panels.
