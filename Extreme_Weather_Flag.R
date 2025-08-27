###############################################################################
# Extreme Weather Flags 
# Author: Edward Godfrey 
# Date: 06/06/2025
# Email: edwardgodfrey25@gmail.com | yy24665@bristol.ac.uk
# Note: Threshold (p), rolling window (win) and consecutive run criteria (Run) can be adjusted 
###############################################################################

flag_extremes_doy <- function(df, p = 0.95, win = 15, run = 3) {
  df <- df %>% dplyr::mutate(doy = yday(DATE))
  clim <- df %>% complete(doy = 1:366)    
  
  roll_q <- function(x) slide_dbl(
    x, .f = ~ quantile(.x, probs = p, na.rm = TRUE),
    .before = win, .after = win)
  
  clim <- clim %>% 
    group_by(doy) %>% 
    dplyr::summarise(
      ta_raw  = quantile(TA_F_MDS,            p, na.rm = TRUE),
      vpd_raw = quantile(VPD_F_MDS,           p, na.rm = TRUE),
      cwd_raw = quantile(cumulative_cwd_7day, p, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      TA_F_MDS_thr            = roll_q(ta_raw),
      VPD_F_MDS_thr           = roll_q(vpd_raw),
      cumulative_cwd_7day_thr = roll_q(cwd_raw)
    )
  
  df %>% 
    left_join(clim, by = "doy") %>% 
    dplyr::mutate(
      heat_flag = TA_F_MDS            > TA_F_MDS_thr,
      vpd_flag  = VPD_F_MDS           > VPD_F_MDS_thr,
      dry_flag  = cumulative_cwd_7day > cumulative_cwd_7day_thr
    ) %>% 
    dplyr::mutate(
      heatwave_day  = frollsum(heat_flag, run, align = "right") >= run,
      drought_spell = frollsum(dry_flag, run, align = "right") >= run,
      high_vpd_spell = frollsum(vpd_flag, run, align = "right") >= run,
      any_extreme   = heatwave_day | drought_spell | high_vpd_spell
    )
}