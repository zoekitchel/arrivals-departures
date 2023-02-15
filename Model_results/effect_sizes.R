library(data.table)
library(ggplot2)
library(cowplot)

loss_model_comparison_taxonomy_akaikeweights <- 
  readRDS("~/Documents/grad_school/Rutgers/Repositories/Col_Ext/Model_results/loss_model_comparison_taxonomy_akaikeweights.rds")
gain_model_comparison_taxonomy_akaikeweights <- 
  readRDS("~/Documents/grad_school/Rutgers/Repositories/Col_Ext/Model_results/gain_model_comparison_taxonomy_akaikeweights.rds")

#how does AICc vary with coefficient
coef_AICc_gains <- ggplot(gain_model_comparison_taxonomy_akaikeweights) +
  geom_point(aes(x = coef, y = AICc)) +
  xlab("Coefficient P(Gain) ~ Scale/Centered Temp") +
  geom_vline(aes(xintercept = 0)) +
  theme_classic()

coef_AICc_losses <- ggplot(loss_model_comparison_taxonomy_akaikeweights) +
  geom_point(aes(x = coef, y = AICc)) +
  xlab("Coefficient P(Loss) ~ Scale/Centered Temp") +
  geom_vline(aes(xintercept = 0)) +
  theme_classic()

#merge
coef_AICc <- plot_grid(coef_AICc_gains, coef_AICc_losses, labels = c("gains","losses"), hjust = -2)

ggsave(coef_AICc, path = "/Users/zoekitchel/Documents/grad_school/Rutgers/Repositories/Col_Ext/Model_results",
       filename = "coef_AICc.jpg")

write.csv(loss_model_comparison_taxonomy_akaikeweights, "~/Documents/grad_school/Rutgers/Repositories/Col_Ext/Model_results/loss_model_comparison_taxonomy_akaikeweights.csv")
write.csv(gain_model_comparison_taxonomy_akaikeweights, "~/Documents/grad_school/Rutgers/Repositories/Col_Ext/Model_results/gain_model_comparison_taxonomy_akaikeweights.csv")