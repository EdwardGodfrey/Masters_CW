###############################################################################
# Function to add 7 day cumlative water deficit to FLUXNET dataframe
# Author: Edward Godfrey, Martin De Kauwe
# Date: 06/06/2025
# Email: edwardgodfrey25@gmail.com | yy24665@bristol.ac.uk
###############################################################################
Add_CWD <- function(df,
                    window = 7,
                    lambda_Jkg = 2.45e6,       
                    sec_per_day = 86400 ){

###############################################################################    
  priestley_taylor_pet <- function(Rn, Tair, P) {
    alpha  <- 1.26
    cp     <- 1.013e-3
    eps    <- 0.622
    lambda <- 2.45        # MJ kg-1
    G      <- 0           # daily ground-heat flux
    
    gamma <- (cp * P) / (eps * lambda)
    es    <- 0.6108 * exp((17.27 * Tair) / (Tair + 237.3))
    delta <- (4098 * es) / ((Tair + 237.3)^2)
    
    # PET in mm day-¹
    alpha * (delta / (delta + gamma)) * ((Rn - G) / (lambda * 1e6)) * sec_per_day
  }
###############################################################################  
  df %>% 
    mutate(
      # convert LE to ET (mm day-¹) 
      ET  = if ("LE_F_MDS" %in% names(.))
        LE_F_MDS * sec_per_day / lambda_Jkg
      else NA_real_,
      
      ## Priestley–Taylor PET (needs NETRAD, TA_F_MDS, PA_F)
      pet = if (all(c("NETRAD", "TA_F_MDS", "PA_F") %in% names(.)))
        priestley_taylor_pet(NETRAD, TA_F_MDS, PA_F)
      else NA_real_,
      
      cwd = pet - ET
    ) %>% 
    # 7-day sum
    mutate(
      cumulative_cwd_7day = rollapply(
        cwd, width = window, FUN = sum,
        align = "right", fill = NA, na.rm = TRUE
      )
    )
}

