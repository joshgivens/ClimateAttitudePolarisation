library(dplyr)
library(tidyr)
library(ggplot2)
library(geomtextpath)
# Correlations of Political Leaning Variables
wave_ord <- c(1,2,4,"a1",7,10:20,22,23)
df <- readRDS("Data/full_tidied.rds")


polit_lean_vars <- df %>% select(left_vs_right,tax_and_soc_scale, 
                                 tax_rich_less, leave_income_inequality,
                                immigration_difficulty_scale,
                                foreigners_assimilate,
                                limit_refugees,
                                state_interfere_terror)
for_pw_correlations <- polit_lean_vars %>% as.matrix()

for_factor_analysis <- polit_lean_vars %>% drop_na() %>% as.matrix()

d <- dim(polit_lean_vars)[2]
P_val_mat <- matrix(NA,nrow=d,ncol=d)
Correlation_mat <- matrix(NA,nrow=d,ncol=d)

for (i in 1:d){
  for (j in 1:d){
    subdat <- for_pw_correlations[,c(i,j)]
    complete_subdat <- subdat[complete.cases(subdat),]
    cor_test <- cor.test(complete_subdat[,1],complete_subdat[,2])
    P_val_mat[i,j] <- cor_test$p.value
    Correlation_mat[i,j] <- cor_test$estimate
  }
}
row.names(Correlation_mat) <- row.names(P_val_mat) <- colnames(polit_lean_vars)
colnames(Correlation_mat) <- colnames(P_val_mat) <- colnames(polit_lean_vars)
Correlation_mat
P_val_mat

################################################################################
########################## PCA #################################################
################################################################################
sample_matrix <- matrix(c(0,1,1,1),nrow=2, byrow = TRUE)
pca <- prcomp(for_factor_analysis, center=TRUE,scale=TRUE)
pca$rotation
biplot(pca, choices=c(1,2))
biplot(sample_matrix,pca$rotation[,1:2],col=c(1,2))
cumsum(pca$sdev^2)/sum(pca$sdev^2)
pca$rotation[,1:2]/abs(rowSums(pca$rotation[,1:2]))

# Now remove left_vs_right
for_factor_analysis2 <- polit_lean_vars %>% select(-left_vs_right) %>% drop_na() %>% as.matrix()
pca2 <- prcomp(for_factor_analysis2, center=TRUE, scale=TRUE)             
pca2$rotation
biplot(pca2, choices=c(1,2))
biplot(sample_matrix,pca2$rotation[,1:2],col=c(1,2))
cumsum(pca2$sdev^2)/sum(pca2$sdev^2)
pca2$rotation[,1:2]/abs(rowSums(pca2$rotation[,1:2]))

pca2$rotation %>%
  as.data.frame() %>%
  select(1:2) %>%
  mutate(group=rep(c("Econ L/R","Cultural L/R"),c(3,4))) %>% 
  tibble::rownames_to_column(var = "var") %>%
  ggplot(aes(0, 0, color = group)) +
  geom_hline(yintercept = 0, alpha = 0.2) +
  geom_vline(xintercept = 0, alpha = 0.2) +
  geom_textsegment(aes(xend =PC1 , yend = PC2, label = var),
                   arrow = arrow()) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "PC1", y = "PC2") +
  coord_fixed(1,c(-0.2,0.8),c(-0.2,0.8))+
  ggtitle("Bi plot of potential L/R leaning variables projected \nonto 1st 2 Princpical Components")
