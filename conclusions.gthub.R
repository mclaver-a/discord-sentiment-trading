# DADES MONEDAS ----
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyverse)

get_crypto_data <- function(symbol) {
  url <- paste0(
    "https://data-api.coindesk.com/index/cc/v1/historical/days?",
    "market=cadli&instrument=", symbol, "-USD",
    "&aggregate=1&fill=true&apply_mapping=true",
    "&response_format=JSON&limit=1000"
  )
  
  response <- GET(url)
  if (status_code(response) != 200) {
    warning(paste("No s'han pogut obtenir dades per a", symbol))
    return(NULL)
  }
  
  json_data <- fromJSON(content(response, "text"))
  entries <- json_data$Data
  
  df <- as.data.frame(entries) %>%
    mutate(
      date = as_date(as.POSIXct(TIMESTAMP, origin = "1970-01-01")),
      .keep = "unused"
    ) %>%
    select(date, OPEN, HIGH, LOW, CLOSE, VOLUME) %>%
    rename_with(tolower)
  
  return(df)
}

df_ENA     <- get_crypto_data("ENA")
df_PENDLE  <- get_crypto_data("PENDLE")
df_HYPE    <- get_crypto_data("HYPE")
df_JUP     <- get_crypto_data("JUP")

# DADES SENTIMENT ----
sentiment <- read_csv("/Users/andreusanmillan/Desktop/TFG/Data Sentiment Analysis - Data Clean.csv")  # Ajusta nom si cal

# 🧹 Prepara data i passa a format llarg
sentiment_long <- sentiment %>%
  mutate(date = as_date(Period_Start_Timestamp)) %>%
  select(date, ENA_Sent, ENA_Pol, JUP_Sent, JUP_Pol, 
         HYPE_Sent, HYPE_Pola, Pendle_Sent, Pendle_Pol) %>%
  pivot_longer(
    cols = -date,
    names_to = c("crypto", ".value"),
    names_pattern = "(.*)_([A-Za-z]+)"  # divideix a crypto: ENA, valor: Sent o Pol
  ) %>%
  filter(Sent != 0 | Pol != 0)  # elimina files buides

# 🧮 Agrega per dia i cripto
daily_sentiment <- sentiment_long %>%
  group_by(date, crypto) %>%
  summarise(
    sentiment = mean(Sent, na.rm = TRUE),
    polarity = mean(Pol, na.rm = TRUE),
    count = n(),  # opcional: nombre de missatges
    .groups = "drop"
  )
# HISTOGRAMES I ESTADISTICA SENCILLA ----
library(ggplot2)
library(dplyr)

ggplot(daily_sentiment, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "#69b3a2", color = "white", alpha = 0.8) +
  facet_wrap(~ crypto, scales = "free_y") +  # un gràfic per cada cripto
  theme_minimal() +
  labs(
    title = "Histograma del sentiment per criptomoneda",
    x = "Sentiment mitjà diari",
    y = "Freqüència"
  )


summary_stats <- daily_sentiment %>%
  group_by(crypto) %>%
  summarise(
    Mitjana = mean(sentiment, na.rm = TRUE),
    Asimetria = skewness(sentiment, na.rm = TRUE),
    Kurtosis = kurtosis(sentiment, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)
# VARIACIÓ EN MOENDAS vs SENTIMENTS ----

ena <- df_ENA %>% mutate(crypto = "ENA")
jup <- df_JUP %>% mutate(crypto = "JUP")
hype <- df_HYPE %>% mutate(crypto = "HYPE")
pendle <- df_PENDLE %>% mutate(crypto = "Pendle")

price_data <- bind_rows(ena, jup, hype, pendle)
price_data <- price_data %>%
  rename(
    date = `date`,     # ⛔ canvia-ho pel nom real de la columna data
    price = `close`    # ⛔ canvia-ho pel nom real del preu (close, price, etc.)
  ) %>%
  mutate(date = as.Date(date))

rescale_to_minus1_1 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))  # evitar divisió per 0
  (2 * (x - rng[1]) / diff(rng)) - 1
}

combined_data <- combined_data %>%
  group_by(crypto) %>%
  mutate(sentiment = rescale_to_minus1_1(sentiment)) %>%
  ungroup()

grafico <- ggplot(combined_data, aes(x = as.Date(date))) +
  geom_line(aes(y = sentiment, color = "Sentiment"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = scale(pct_change)[,1], color = "Canvi de preu (%)"), 
            linetype = "solid", size = 0.5, na.rm = TRUE) +
  facet_wrap(~ crypto, scales = "free") +
  scale_color_manual(
    name = NULL,
    values = c("Sentiment" = "#69b3a2", "Canvi de preu (%)" = "darkred"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "solid")))
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Data",
    y = "Valor escalat",
    caption = "Font: Elaboració pròpia"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption = element_text(hjust = 1, face = "italic", color = "gray50")
  )

final_plot <- ggdraw(grafico) + 
  draw_label("Gràfica 1", x = 0.02, y = 0.02, hjust = 0, vjust = 0,
             fontface = "bold", color = "gray30", size = 10)

final_plot
# MONEDA I SENTIMENT ----
  
  plot_price_with_sentiment <- function(df, date_col, price_col, sent_col, crypto_name) {
    ggplot(df, aes(x = !!sym(date_col))) +
      geom_line(aes(y = !!sym(price_col)), color = "black", linewidth = 0.8) +
      
      geom_point(
        data = df %>% filter(abs(!!sym(sent_col)) > 0.5),
        aes(
          x = !!sym(date_col),
          y = !!sym(price_col),
          color = !!sym(sent_col),
          size = abs(!!sym(sent_col))
        ),
        alpha = 0.6
      ) +
      
      scale_color_gradient2(
        low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
        name = "Sentiment"
      ) +
      scale_size_continuous(name = "Intensitat del sentiment") +
      
      labs(
        title = paste(crypto_name),
        x = "Data",
        y = "Preu (close)"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.position = "none", 
        panel.grid.minor = element_blank()
      )
  }



p1 <- plot_price_with_sentiment(merged_ena, "timestamp", "close", "ENA_Sent", "Ethena")
p2 <- plot_price_with_sentiment(merged_jup, "timestamp", "close", "JUP_Sent", "Jupiter")
p3 <- plot_price_with_sentiment(merged_hype, "timestamp", "close", "HYPE_Sent", "Hyperliquid")
p4 <- plot_price_with_sentiment(merged_pendle, "timestamp", "close", "Pendle_Sent", "Pendle")

p1 +p2 +p3 + p4

# TAULA CORRELACIÓ ----
library(dplyr)
library(tibble)

correlation_results <- combined_data %>%
  group_by(crypto) %>%
  summarise(
    Pearson_r = cor(combined_data, pct_change, method = "pearson", use = "complete.obs"),
    p_pearson = cor.test(combined_data, pct_change, method = "pearson")$p.value,
    Spearman_rho = cor(combined_data, pct_change, method = "spearman", use = "complete.obs"),
    p_spearman = cor.test(combined_data, pct_change, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  # Format p-valors
  mutate(
    p_pearson = ifelse(p_pearson < 0.001, "<0.001", round(p_pearson, 3)),
    p_spearman = ifelse(p_spearman < 0.001, "<0.001", round(p_spearman, 3))
  )

print(correlation_results)


# CROSS CORRELATION ----
preprocessed <- combined_data %>%
  group_by(crypto) %>%
  arrange(date) %>%
  mutate(
    return = log(price / lag(price)),                
    sentiment_z = scale(sentiment)[,1],              
    return_z = scale(return)[,1]
  ) %>%
  ungroup()

lags <- -14:14
alpha <- 0.05
num_tests <- length(lags)
p_thresh <- alpha / num_tests 

compute_ccf <- function(df_crypto, coin_name) {
  map_dfr(lags, function(lag) {
    df_lagged <- df_crypto %>%
      mutate(
        sentiment_lagged = lag(sentiment_z, n = -lag)
      ) %>%
      filter(!is.na(sentiment_lagged), !is.na(return_z))
    
    test <- cor.test(df_lagged$sentiment_lagged, df_lagged$return_z, method = "pearson")
    
    tibble(
      crypto = coin_name,
      lag = lag,
      correlation = test$estimate,
      p_value = test$p.value,
      significant = test$p.value < p_thresh
    )
  })
}
compute_ccf <- function(df_crypto, coin_name) {
  map_dfr(lags, function(lag_value) {
    df_lagged <- df_crypto %>%
      mutate(
        sentiment_lagged = if (lag_value >= 0) {
          lag(sentiment_z, n = lag_value)
        } else {
          lead(sentiment_z, n = abs(lag_value))
        }
      ) %>%
      filter(!is.na(sentiment_lagged), !is.na(return_z))
    
    test <- cor.test(df_lagged$sentiment_lagged, df_lagged$return_z, method = "pearson")
    
    tibble(
      crypto = coin_name,
      lag = lag_value,
      correlation = test$estimate,
      p_value = test$p.value,
      significant = test$p.value < p_thresh
    )
  })
}

correlation_lags <- preprocessed %>%
  group_split(crypto) %>%
  map_dfr(~ compute_ccf(.x, unique(.x$crypto)))

ggplot(correlation_lags, aes(x = lag, y = crypto, fill = abs(correlation))) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  geom_point(data = correlation_lags %>% filter(significant),
             aes(x = lag, y = crypto),
             shape = 21, size = 3, stroke = 1, color = "white", fill = NA) +
  
  scale_fill_gradient(low = "white", high = "#08519c", name = "|r|") +
  scale_x_continuous(breaks = seq(-14, 14, 2)) +
  labs(
    title = "Figura 5. Cross-correlació entre sentiment i rendiments",
    subtitle = "Tons més foscos = correlació més alta; contorn blanc = p < 0.05 (Bonferroni)",
    x = "Retard (lag) en dies",
    y = "Criptomoneda",
    caption = "Font: Elaboració pròpia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right"
  )


ggplot(correlation_lags, aes(x = lag, y = crypto, fill = abs(correlation))) +
  # Color del tile
  geom_tile(color = "grey90", linewidth = 0.3) +
  
  # Punts blancs per marcar significància
  geom_point(data = correlation_lags %>% filter(significant),
             aes(x = lag, y = crypto),
             shape = 21, size = 3, stroke = 1.2,
             fill = NA, color = "white") +
  
  # Color millor definit i suau
  scale_fill_gradientn(
    colours = c("white", "#9ecae1", "#3182bd", "#08519c"),
    name = expression("|r|"~"(correlació)"),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  scale_x_continuous(breaks = seq(-14, 14, 2)) +
  
  labs(
    title = "Figura 5. Cross-correlació entre sentiment i rendiments",
    subtitle = "Tons més foscos = |correlació| més alta. Contorn blanc = p < 0.05 (correcció de Bonferroni)",
    x = "Retard (lag) en dies (negatiu: sentiment abans)",
    y = "Criptomoneda",
    caption = "Font: Elaboració pròpia"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )

geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4)


# TAULA CROSS CORR ----
lag_optims <- correlation_lags %>%
  group_by(crypto) %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
  ungroup()

sense_significancia <- setdiff(unique(correlation_lags$crypto), unique(lag_optims$crypto))

interpretacio <- function(lag, corr) {
  if (is.na(lag)) {
    return("No s’identifica cap retard clar.")
  } else if (lag < 0 & corr > 0) {
    return("Sentiment anticipa el preu ~" %+% abs(lag) * 24 %+% " h abans.")
  } else if (lag < 0 & corr < 0) {
    return("Sentiment negatiu anticipa caigudes de preu ~" %+% abs(lag) * 24 %+% " h abans.")
  } else if (lag > 0 & corr < 0) {
    return("Moviments de preu negatius influeixen el sentiment " %+% lag %+% " dies després.")
  } else if (lag > 0 & corr > 0) {
    return("Preu positiu impulsa sentiment " %+% lag %+% " dies després.")
  } else if (lag == 0) {
    return("Relació simultània entre sentiment i preu.")
  } else {
    return("Relació observada però interpretació incerta.")
  }
}

taula_2 <- lag_optims %>%
  mutate(
    `Lag òptim (dies)` = as.character(lag),
    XCorr = sprintf("%.2f", correlation),
    `p‑value` = sprintf("%.3f", p_value),
    Interpretació = mapply(interpretacio, lag, correlation)
  ) %>%
  select(Criptomoneda = crypto, `Lag òptim (dies)`, XCorr, `p‑value`, Interpretació)

if (length(sense_significancia) > 0) {
  taula_2 <- bind_rows(
    taula_2,
    tibble(
      Criptomoneda = sense_significancia,
      `Lag òptim (dies)` = "cap significatiu",
      XCorr = "—",
      `p‑value` = "—",
      Interpretació = "No s’identifica cap retard clar."
    )
  )
}

taula_2 <- taula_2 %>% arrange(Criptomoneda)

taula_2

# REAL VS PREDICTED ----
nou <- na.omit(nou)


resultats_models <- list()
plots_models <- list()

# Loop per cada criptomoneda
for (cripto in unique(nou$crypto)) {
  cat("Processant:", cripto, "\n")
  
  df <- nou %>% filter(crypto == cripto)
  
  if (nrow(df) < 20) {
    cat("Massa poques observacions per:", cripto, "\n")
    next
  }
  
  # Separació train/test
  set.seed(123)
  index <- createDataPartition(df$pct_change, p = 0.8, list = FALSE)
  train <- df[index, ]
  test <- df[-index, ]
  
  if (nrow(train) < 10 || nrow(test) < 5) {
    cat("Partició insuficient per:", cripto, "\n")
    next
  }
  
  tryCatch({
    # Model lineal
    model_lm <- lm(pct_change ~ sentiment, data = train)
    pred_lm <- predict(model_lm, newdata = test)
    
    # Mètriques
    metriques <- tibble(
      Cripto = cripto,
      Model = "Lineal",
      RMSE = rmse(test$pct_change, pred_lm),
      MAE = mae(test$pct_change, pred_lm),
      R2 = r2_manual(test$pct_change, pred_lm)
    )
    
    resultats_models[[cripto]] <- metriques
    
    # Gràfica real vs. predicció
    df_plot <- tibble(
      Real = test$pct_change,
      Pred = pred_lm
    )
    
    plot <- ggplot(df_plot, aes(x = Real, y = Pred)) +
      geom_point(color = "#0072B2", alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linewidth = 1) +
      labs(title = paste("Model Lineal:", cripto),
           x = "Valor real (%)",
           y = "Valor predit (%)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    plots_models[[cripto]] <- plot
  }, error = function(e) {
    cat("Error amb", cripto, ":", conditionMessage(e), "\n")
  })
}

# Unifica resultats
taula_resultats <- bind_rows(resultats_models)
print(taula_resultats)

# Mostra totes les gràfiques disponibles
for (cripto in names(plots_models)) {
  print(plots_models[[cripto]])
}



# Llista buida per agrupar les dades de totes les criptos
df_plot_all <- list()

# Construcció dels datasets per cada cripto
for (cripto in names(plots_models)) {
  df <- nou %>% filter(crypto == cripto)
  
  set.seed(123)
  index <- createDataPartition(df$pct_change, p = 0.8, list = FALSE)
  train <- df[index, ]
  test <- df[-index, ]
  
  model_lm <- lm(pct_change ~ sentiment, data = train)
  pred_lm <- predict(model_lm, newdata = test)
  
  df_plot <- tibble(
    Cripto = cripto,
    Real = test$pct_change,
    Pred = pred_lm
  )
  
  df_plot_all[[cripto]] <- df_plot
}

# Unim totes les dades en un únic dataframe
df_plot_total <- bind_rows(df_plot_all)

# Gràfica amb facet_wrap
library(ggplot2)

ggplot(df_plot_total, aes(x = Real, y = Pred)) +
  geom_point(alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linewidth = 1) +
  facet_wrap(~ Cripto, scales = "free") +
  labs(
    title = "Model lineal: Predicció vs Valor real",
    x = "Valor real (%)",
    y = "Valor predit (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )


# Llista buida per totes les dades per fer el facet
df_plot_all <- list()

for (cripto in unique(nou$crypto)) {
  df <- nou %>% filter(crypto == cripto)
  
  # Només fem el model si hi ha almenys 10 observacions
  if (nrow(df) < 10) {
    cat("Saltant", cripto, "- massa poques observacions.\n")
    next
  }
  
  set.seed(123)
  index <- createDataPartition(df$pct_change, p = 0.8, list = FALSE)
  train <- df[index, ]
  test <- df[-index, ]
  
  # Aquí assumim que tot i que hi hagi poques dades, volem visualitzar
  if (nrow(train) < 5 || nrow(test) < 2) {
    cat("Partició petita per", cripto, "- però s'inclou per a gràfica.\n")
  }
  
  tryCatch({
    model_lm <- lm(pct_change ~ sentiment, data = train)
    pred_lm <- predict(model_lm, newdata = test)
    
    df_plot <- tibble(
      Cripto = cripto,
      Real = test$pct_change,
      Pred = pred_lm
    )
    
    df_plot_all[[cripto]] <- df_plot
  }, error = function(e) {
    cat("Error amb", cripto, ":", conditionMessage(e), "\n")
  })
}

# Unim totes les dades
df_plot_total <- bind_rows(df_plot_all)

# Facet wrap amb totes les criptos inclosa HYPE
ggplot(df_plot_total, aes(x = Real, y = Pred)) +
  geom_point(alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linewidth = 1) +
  facet_wrap(~ Cripto, scales = "free") +
  labs(
    title = "Model lineal: Predicció vs Valor real (inclou HYPE)",
    x = "Valor real (%)",
    y = "Valor predit (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )