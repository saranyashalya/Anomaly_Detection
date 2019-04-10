########################## Anomaly Detection - Univariate time series ##############################
####################################################################################################

library(tidyquant)
library(tidyverse)
library(tibbletime)
library(anomalize)

# Stock Prices from Yahoo - Finance
FANG_symbols <- c("FB", "AMZN", "NFLX", "GOOG")

FANG_tbl_d <- FANG_symbols %>%
  tq_get(get = "stock.prices", from = "2016-01-01", to = "2018-06-30") 

backup_tab <- FANG_tbl_d

FANG_tbl_d <- FANG_tbl_d %>%  group_by(symbol)

FANG_tbl_d %>% filter(symbol == "FB") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() %>% plot_anomalies(time_recomposed=TRUE)
FANG_tbl_d %>% filter(symbol == "AMZN") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() %>% plot_anomalies(time_recomposed = TRUE)
FANG_tbl_d %>% filter(symbol == "NFLX") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() %>% plot_anomalies(time_recomposed = TRUE)
FANG_tbl_d %>% filter(symbol == "GOOG") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() %>% plot_anomalies(time_recomposed = TRUE)

f_fb <- FANG_tbl_d %>% filter(symbol == "FB") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose()
f_amzn <- FANG_tbl_d %>% filter(symbol == "AMZN") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose()
f_nflx <- FANG_tbl_d %>% filter(symbol == "NFLX") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() 
f_goog <- FANG_tbl_d %>% filter(symbol == "GOOG") %>% ungroup() %>% time_decompose(adjusted, merge = TRUE) %>% anomalize(remainder) %>% time_recompose()

ggplot(aes(x=anomaly), data = f_fb) + geom_bar(aes(fill=anomaly)) + geom_text(stat='count', aes(label=..count..), vjust=-1) + ggtitle("Distribution of Facebook Anomaly")
ggplot(aes(x=anomaly), data = f_amzn) + geom_bar(aes(fill=anomaly)) + geom_text(stat='count', aes(label=..count..), vjust=-1) + ggtitle("Distribution of Amazon Anomaly")
ggplot(aes(x=anomaly), data = f_nflx) + geom_bar(aes(fill=anomaly)) + geom_text(stat='count', aes(label=..count..), vjust=-1) + ggtitle("Distribution of Netflix Anomaly")
ggplot(aes(x=anomaly), data = f_goog) + geom_bar(aes(fill=anomaly)) + geom_text(stat='count', aes(label=..count..), vjust=-1) + ggtitle("Distribution of Google Anomaly")

f_fb %>% mutate(anomaly_flag = ifelse(anomaly=="Yes",T,F)) %>% ggplot(aes(x=date, y = adjusted)) +geom_line() +geom_point(aes(col=anomaly_flag))
f_amzn %>% mutate(anomaly_flag = ifelse(anomaly=="Yes",T,F)) %>% ggplot(aes(x=date, y = adjusted)) +geom_line() +geom_point(aes(col=anomaly_flag))
f_nflx %>% mutate(anomaly_flag = ifelse(anomaly=="Yes",T,F)) %>% ggplot(aes(x=date, y = adjusted)) +geom_line() +geom_point(aes(col=anomaly_flag))
f_goog %>% mutate(anomaly_flag = ifelse(anomaly=="Yes",T,F)) %>% ggplot(aes(x=date, y = adjusted)) +geom_line() +geom_point(aes(col=anomaly_flag))

