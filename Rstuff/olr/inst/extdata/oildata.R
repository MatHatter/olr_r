#' @title oildata
#'
#' @description Potential influencers of oil prices. Not every influencer is in this dataset however.
#'
#'
#' @format A data frame with 52 rows and 9 variables:
#' \describe{
#'   \item{OilPrices}{Weekly percent change in WTI spot prices}
#'   \item{SP500}{Weekly percent change in closing prices of S&P 500}
#'   \item{RigCount}{Weekly percent change in Crude Oil Drilling Activity}
#'   \item{API}{Weekly percent change in U.S. API Weekly Crude Oil Stock}
#'   \item{Field_Production}{Weekly percent change in U.S. Oil Production}
#'   \item{RefinerNetInput}{Weekly percent change in Refinery Net Input}
#'   \item{OperableCapacity}{Weekly percent change in Refinery Utilization and Capacity}
#'   \item{Imports}{Weekly percent change in Weekly Imports}
#'   \item{StocksExcludingSPR}{Weekly percent change in Total Crude Oil (Excl. SPR)}
#' }
#'
#'
#' @source \url{https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm}
#' @source \url{https://fred.stlouisfed.org/series/SP500}
#' @source \url{https://www.eia.gov/dnav/ng/ng_enr_drill_s1_m.htm}
#' @source \url{https://www.investing.com/economic-calendar/api-weekly-crude-stock-656}
#' @source \url{https://www.eia.gov/dnav/pet/pet_crd_crpdn_adc_mbbl_m.htm}
#' @source \url{https://www.eia.gov/dnav/pet/pet_pnp_inpt2_dc_nus_mbblpd_m.htm}
#' @source \url{https://www.eia.gov/dnav/pet/PET_PNP_UNC_A_(NA)_YRL_MBBLPD_M.htm}
#' @source \url{https://www.eia.gov/dnav/pet/pet_move_wkly_dc_NUS-Z00_mbblpd_w.htm}
#' @source \url{https://www.eia.gov/dnav/pet/pet_stoc_wstk_dcu_nus_w.htm}
"oildata"
