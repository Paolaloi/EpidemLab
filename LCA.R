# Installa pacchetti necessari
install.packages("poLCA")
install.packages("ggplot2")

# Carica pacchetti
library(poLCA)
library(ggplot2)

# Importa i dati (modifica il percorso se necessario)
data <- read.csv("/Users/lorispalmarin/PycharmProjects/EpidemLab/lca_data_discretized.csv")

# Assicurati che tutte le variabili siano categoriche
data[] <- lapply(data, factor)

# Costruisci la formula per LCA
lca_formula <- as.formula(paste("cbind(", paste(names(data), collapse = ", "), ") ~ 1"))

# Loop per testare diversi numeri di classi
bic_values <- c()
aic_values <- c()
nclass_range <- 2:6  # Modifica il range se vuoi testare più o meno classi

set.seed(123)  # Per riproducibilità
for (k in nclass_range) {
  model <- poLCA(lca_formula, data, nclass = k, maxiter = 1000, verbose = FALSE)
  bic_values <- c(bic_values, model$bic)
  aic_values <- c(aic_values, model$aic)
}

# Crea un dataframe per la visualizzazione
fit_stats <- data.frame(
  Classes = nclass_range,
  BIC = bic_values,
  AIC = aic_values
)

# Plot BIC e AIC
plot <- ggplot(fit_stats, aes(x = Classes)) +
  geom_line(aes(y = BIC, color = "BIC")) +
  geom_line(aes(y = AIC, color = "AIC")) +
  geom_point(aes(y = BIC, color = "BIC")) +
  geom_point(aes(y = AIC, color = "AIC")) +
  labs(title = "Model fit by number of classes",
       y = "Information Criterion",
       x = "Number of Classes") +
  scale_color_manual(values = c("BIC" = "blue", "AIC" = "red")) +
  theme_minimal()
print(plot)
print(fit_stats)