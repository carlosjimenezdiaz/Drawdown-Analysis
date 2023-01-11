library(tidyquant)
library(tidyverse)
library(gt)
library(directlabels)

# Threshold for the Drawdown
DD_Criteria <- -20 

# Local Variables
Ticker      <- "^GSPC"
Ticker_Name <- "SP500"
eDate       <- Sys.Date() # Sys.Date()

# Calculating the Drawdowns
DB_Drawdowns <- tq_get(Ticker,
       from = eDate - lubridate::years(100),
       to   = eDate %>% as.Date()) %>%
  dplyr::select(date, adjusted) %>%
  column_to_rownames(var = "date") %>%
  dplyr::mutate(adjusted = TTR::ROC(adjusted, n = 1, type = "discrete")) %>%
  replace(is.na(.), 0) %>%
  as.xts() %>%
  PerformanceAnalytics::table.Drawdowns(top = 500) %>%
  as.data.frame() %>%
  dplyr::filter(Depth < DD_Criteria/100)

# Generating the table
DB_Drawdowns %>%
  gt() %>%
  tab_header(
    title    = "Drawdown Analysis of the SP500",
    subtitle = str_glue("Depth greater than {-1*DD_Criteria}% - Daily Timeframe")
  ) %>%
  fmt_percent(
    columns  = Depth,
    decimals = 1
  )  %>%
  tab_source_note(
    source_note = str_glue("By: Carlos Jimenez - {eDate}")
  )

# Calculating the performance during those years (From to Trough)
DB_Performance <- NULL
for(i in 1:nrow(DB_Drawdowns)){ # i <- 1
  
  DB_Hist_Performance <- tq_get(Ticker,
         from = DB_Drawdowns$From[i],
         to   = DB_Drawdowns$Trough[i]) %>%
    dplyr::select(date, adjusted) %>%
    column_to_rownames(var = "date") %>%
    dplyr::mutate(adjusted = TTR::ROC(adjusted, n = 1, type = "discrete")) %>%
    as.xts() %>%
    PerformanceAnalytics::Return.portfolio(wealth.index = TRUE)

  if(is.null(DB_Performance)){
    DB_Performance <- data.frame(Days = 1:nrow(DB_Hist_Performance),
                                 Ret  = DB_Hist_Performance$portfolio.wealthindex,
                                 ID   = DB_Drawdowns$From[i] %>% as.Date())
  }else{
    DB_Performance <- DB_Performance %>%
      bind_rows(data.frame(Days = 1:nrow(DB_Hist_Performance),
                           Ret  = DB_Hist_Performance$portfolio.wealthindex,
                           ID   = DB_Drawdowns$From[i] %>% as.Date()))
  }
}

# Adding current year
Current_Year_Performance <- tq_get(Ticker,
       from = str_glue("{lubridate::year(eDate)}-01-01") %>% as.Date(),
       to   = eDate) %>%
  dplyr::select(date, adjusted) %>%
  column_to_rownames(var = "date") %>%
  dplyr::mutate(adjusted = TTR::ROC(adjusted, n = 1, type = "discrete")) %>%
  replace(is.na(.), 0) %>%
  as.xts() %>%
  PerformanceAnalytics::Return.portfolio(wealth.index = TRUE)

DB_Performance <- DB_Performance %>%
  bind_rows(data.frame(Days = 1:nrow(Current_Year_Performance),
                       Ret  = Current_Year_Performance$portfolio.wealthindex,
                       ID   = str_glue("{lubridate::year(eDate)}-01-01") %>% as.Date())) %>%
  remove_rownames() %>%
  purrr::set_names(c("Days", "NAV", "ID")) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(NAV = -1*(1 - NAV),
                ID  = as.character(ID)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = lubridate::year(ID) %>% as.numeric(),
                color = ifelse(label == lubridate::year(eDate), "Current", "Old"))

# Chart
DB_Performance %>%
  group_by(label) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  ggplot(aes(x = Days, y = NAV, colour = color, group = label, linetype = color)) +
  geom_line() +
  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")), cex = 0.8) + 
  scale_color_manual(values = c("red", "gray")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Drawdown Analysis of the {Ticker_Name}"),
       subtitle = str_glue("From-Trough Analysis. Current performance vs those years with a drawdown higher, or equal, to {DD_Criteria}%"),
       caption  = "By: Carlos Jimenez",
       x = "Days since drawdown started",
       y = "Drawdown") +
  theme(legend.position = "none") +
  geom_hline(yintercept = DD_Criteria/100, 
             linetype   = "dotted", 
             color      = "darkgreen",  
             size       = 1)
