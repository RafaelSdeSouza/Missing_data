require(sbgcop)
require(MASS)
require(mice)
library(reshape2)
library(ggplot2)

# Create dataset
sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
complete.data <- mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)

# Perform quick amputation
result1 <- ampute(data = complete.data)

d <- as.data.frame(result1$amp)


# Visualize missing pattern
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_stata(name = "",
                    labels = c("Present","Missing")) +
    theme_economist_white() + 
    theme(legend.position="top",axis.text.x  = element_text(angle = 45, vjust = 0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(d)

# Imputation via Copula
fit <- sbgcop.mcmc(d)
plot(fit)

fit$Y.pmean


