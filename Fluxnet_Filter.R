###############################################################################
# Data Importing and Filtering for daily FLUXNET2015 DATA 
# Author: Edward Godfrey 
# Date: 06/06/2025
# Email: edwardgodfrey25@gmail.com | yy24665@bristol.ac.uk
# Note: Some FLUXNET sites do not contain SWD measurements so the code can be adjusted
# for this case.
###############################################################################

FLUXNET_Filter <- function(
    data_path,
    qc_threshold = 0.8,
    cols_to_keep = c(
      "TIMESTAMP",
      "TA_F_MDS", "TA_F_MDS_QC",
      "GPP_NT_VUT_MEAN",
      "SWC_F_MDS_1", "SWC_F_MDS_1_QC",   # remove these if SWC is absent
      "VPD_F_MDS", "VPD_F_MDS_QC",
      "CO2_F_MDS", "CO2_F_MDS_QC",
      "SW_IN_F_MDS", "SW_IN_F_MDS_QC",
      "NETRAD", "PA_F", "PA_F_QC",
      "PA_ERA", "LE_F_MDS", "LE_F_MDS_QC",
      "P_F", "P_F_QC")
) {
  
  vroom::vroom(data_path, show_col_types = FALSE) %>%
    dplyr::select(dplyr::all_of(cols_to_keep)) %>%
    dplyr::filter(
      complete.cases(.),
      TA_F_MDS_QC    >= qc_threshold,
      SWC_F_MDS_1_QC >= qc_threshold,   # drop this line if SWC not present
      VPD_F_MDS_QC   >= qc_threshold,
      CO2_F_MDS_QC   >= qc_threshold,
      SW_IN_F_MDS_QC >= qc_threshold,
      PA_F_QC        >= qc_threshold,
      LE_F_MDS_QC    >= qc_threshold,
      P_F_QC         >= qc_threshold,
      GPP_NT_VUT_MEAN > 0
    ) %>%
    dplyr::mutate(
      DATE     = as.Date(as.character(TIMESTAMP), "%Y%m%d"),
      DOY      = as.integer(format(DATE, "%j")),
      temp_lag_3 = dplyr::lag(TA_F_MDS, 3),
      VPD_lag_3  = dplyr::lag(VPD_F_MDS, 3),
      SWC_lag_3  = dplyr::lag(SWC_F_MDS_1, 3),
      temp_lag_5 = dplyr::lag(TA_F_MDS, 5),
      VPD_lag_5  = dplyr::lag(VPD_F_MDS, 5),
      SWC_lag_5  = dplyr::lag(SWC_F_MDS_1, 5),
      temp_lag_7 = dplyr::lag(TA_F_MDS, 7),
      VPD_lag_7  = dplyr::lag(VPD_F_MDS, 7),
      SWC_lag_7  = dplyr::lag(SWC_F_MDS_1, 7),
      temp_lag_14 = dplyr::lag(TA_F_MDS, 14),
      VPD_lag_14  = dplyr::lag(VPD_F_MDS, 14),
      SWC_lag_14  = dplyr::lag(SWC_F_MDS_1, 14),
    )
}
  