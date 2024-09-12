# useful cols collection
# screenings
screening_cols <- c("PriorBreastCancer", "BreastRemoved", "ChestRad",
                    "DCIS-LCIS", "BRCA1/2", "CowdenSyn", "LiFraumeni")
# expected to be complete columns, continuous?
# TODO: check missingness
risks_cols <- c("Experimental condition", "abs_risk_rounded", 
                "abs_risk_pct", "abs_risk_lifetime_rounded",
                "abs_risk_age_group_rounded", "abs_risk_age_group_pct",
                "abs_risk_no_cancer_cohort", 
                "abs_risk_age_group_no_cancer_cohort",
                "50yr_absolute_risk",
                "50yr_absolute_risk_lifetime",
                "50yr_absolute_risk_age_group",
                "50yr_RR_Star1", "50yr_RR_Star2", "50yr_abs_risk_rounded",
                "50yr_abs_risk_pct", "50yr_abs_risk_lifetime_rounded",
                "50yr_abs_risk_age_group_rounded", 
                "50yr_abs_risk_age_group_pct", 
                "50yr_abs_risk_no_cancer_cohort",
                "abs_risk_relative_text",
                "50yr_abs_risk_age_group_no_cancer_cohort")
# cross tabulation, categorical
conditions <- c("Experimental condition", "Pre-estimate")

# risk rejection related, better be complete, ordinal, RReject1
risk_rej_cols <- paste0("RReject", 1:7)

# risk comp, categorical
risk_comp_cols <- paste0("RiskComp", 1:2)
obj_label_cols <- paste0("ObjNum", 1:3)

# family history, categorical
fam_his_cols <- paste0("FamHis", 1:17)

# health his, categorical
health_his <- c(sort(c(paste0("HealthHis", 1:9),
                       paste0("HealthHis", c(5, 7),".1") )),
                paste0("HealthHis", 10:11), "HealthHis11.1")

# Risk recall, categorical vars
risk_recall_cols <- paste0("RRecall", 1:3)

# GroupMedMis, categorical
grp_medmis_cols <- paste0("GroupMedMis", 1:9)

# bayesian cols, categorical
bayesian_cols <- sort(c(paste0("Bayesian", 1:3), 
                        paste0("Bayesian", 1:3, "a")))
# info conflict, categorical
info_conflict_cols <- paste0("InfoConflict", 1:5)

# health sys dis, categorical
health_sysdis_cols <- paste0("HealthSysDis", 1:9)

# mot reason, categorical
mot_reason_cols <- paste0("MotReason", 1:11)

# riskcon info seeking, categorical
riskcon_info_seeking <- c("RiskCon Info Seeking")

# PerRel, categorical
perrel_cols <- paste0("PerRel", 1:3)

# screen Intent, categorical
screen_intent <- sort(c(paste0("ScreenIntent", 1:4), "ScreenIntent1a"))

# barriers, categorical
barrier_cols <- paste0("Barrier", 1:5)

# subject cols, categorical
sub_label_cols <- paste0("SubNum", 1:3)

# subject health lit, categorical
sub_health_cols <- paste0("SubHealthLit", 1:3)

# demographics
demo_cols <- c("Age", "Race")


# table(data$GroupMedMis1, useNA = "ifany")
# table(data$`Pre-estimate`, useNA = "ifany")
# table(old_data$`Experimental condition`, useNA = "ifany")

