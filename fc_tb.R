# FUNCTION TO FORECAST INCIDENCE AND COST OF TB IN SELECT COUNTRIES -------

fc_tb <- function (location) {
  
  for(i in location) {
    
    tryCatch( {
      
      mdr <- incidence %>% 
        filter(country == i) %>% 
        transmute(index,
                  tb_type = "mdr",
                  value = mdr_tb)
      
      xdr <- incidence %>% 
        filter(country == i) %>% 
        transmute(index, 
                  tb_type = "xdr",
                  value = xdr_tb)
      
      ts_mdr <- as_tsibble(ts(mdr$value, start = 1990, end = 2017, frequency = 1))
      ts_xdr <- as_tsibble(ts(xdr$value, start = 1990, end = 2017, frequency = 1))
      
      fc_mdr <- as_tibble(
        ts_mdr %>%
          model(ARIMA(ic = "aicc")) %>% 
          forecast(h = 8)
      )
      
      fc_xdr <- as_tibble(
        ts_xdr %>% 
          model(ARIMA(ic = "aicc")) %>% 
          forecast(h = 8)
      )
      
      # TABULATE FORECAST -------------------------------------------------------
      
      sd_mdr <- as_tibble(fc_mdr %>% 
                            unnest(.distribution) %>% 
                            filter(row_number() %% 3 == 2) %>% 
                            transmute(.distribution) %>% 
                            unlist(fc_mdr$.distribution, use.names = FALSE))
      
      sd_xdr <- as_tibble(fc_xdr %>% 
                            unnest(.distribution) %>% 
                            filter(row_number() %% 3 == 2) %>% 
                            transmute(.distribution) %>% 
                            unlist(fc_xdr$.distribution, use.names = FALSE))
      
      exp <- exp_data %>% 
        filter(country == i)
      
      inf <- inflation_avg %>% 
        filter(country == i)
      
      # change negative predictions to zero
      fc_tbl_mdr <- fc_mdr %>% 
      transmute(index, tb_type = "mdr",
                value = if_else(value < 0, 0, value),
                upper_90 = value + 1.315 * (sd_mdr$value / sqrt(27)),
                lower_90 = value - 1.315 * (sd_mdr$value / sqrt(27)),
                cost_tot = value * (exp$cpc_mdr * ((1 + inf$inflation) ^ row_number())),
                )
      
      fc_tbl_xdr <- fc_xdr %>% 
      transmute(index, tb_type = "xdr",
                value = if_else(value < 0, 0, value),
                upper_90 = value + 1.315 * (sd_xdr$value / sqrt(27)),
                lower_90 = value - 1.315 * (sd_xdr$value / sqrt(27)),
                cost_tot = value * (exp$cpc_xdr * ((1 + inf$inflation) ^ row_number())),
                )
      
      fc_tbl <- fc_tbl_mdr %>% 
        rbind(fc_tbl_xdr) %>% 
        full_join(mdr) %>% 
        full_join(xdr) %>% 
        arrange(index)
      
      # PLOT --------------------------------------------------------------------
      
      plot_a <- fc_tbl %>% ggplot(aes(x = index)) +
        geom_point(aes(y = value, colour = tb_type), show.legend = FALSE) +
        geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = tb_type), alpha = 0.2) +
        geom_vline(aes(xintercept = 2018), alpha = 0.2) +
        labs(x = "Year", 
             y = "Number of cases", 
             title = paste("Drug-resistant TB in", i, "(1990-2025)"),
             fill = "Type of tb")
      
      plot_b <- fc_tbl %>% ggplot(aes(x = index)) +
        geom_col(aes(y = cost_tot / 1000, fill = tb_type)) +
        geom_point(aes(y = bud_tot)) +
        labs(x = "Year", 
             y = "US Dollars ('000s)", 
             title = paste("Cost of drugs to treat drug-resistant TB in", i, "(2018-2025)"), 
             fill = "Type of TB")
      
      
      # this needs legend title and line description
      
      plot <- plot_grid(plot_a, plot_b, ncol = 1, align = "v")
      
      
      # SAVE OUTPUT -------------------------------------------------------------
      
      write_csv(fc_tbl, path = paste("output/", i, "_fc.csv"))
      ggsave2(paste("output/", i, "_fc.png"))
      
    }, 
    error = function(cond) {
      message(paste(i, " has thrown up the following error:"))
      message(cond)
      return(NULL)
    },
    finally = message(paste("
  ", i, "has been run :)"))
    )
    
  }
}