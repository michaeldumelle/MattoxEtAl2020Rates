library(tidyverse)
library(nlme)
set.seed(9719)

dy1 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/perm_data_y1.csv")

lme_peaky1 <- lme(Peak ~ as.factor(Group), random = ~ 1|as.factor(Block), data = dy1)
anova_peaky1 <- anova(lme_peaky1, type = "marginal")
df_peaky1 <- data.frame(anova_peaky1)
f_peaky1 <- df_peaky1[2, 3]

n_samp = 10000
new_f_peaky1 = rep(0, n_samp)
new_pdy1 <- pdy1
for (i in 1:n_samp){
  new_dy1$Group <- sample(new_dy1$Group)
  new_dy1$Block <- sample(new_dy1$Block)
  new_lme_peaky1 <- lme(Peak ~ as.factor(Group), random = ~ 1|as.factor(Block), data = new_dy1)
  new_anova_peaky1 <- anova(new_lme_peaky1, type = "marginal")
  new_df_peaky1 <- data.frame(new_anova_peaky1)
  new_f_peaky1[i] <- new_df_peaky1[2, 3]
}

pval_peaky1 <- mean(new_f_peaky1 >= f_peaky1)


lme_audpcy1 <- lme(AUDPC ~ as.factor(Group), random = ~ 1|as.factor(Block), data = dy1)
anova_audpcy1 <- anova(lme_audpcy1, type = "marginal")
df_audpcy1 <- data.frame(anova_audpcy1)
f_audpcy1 <- df_audpcy1[2, 3]

n_samp = 10000
new_f_audpcy1 = rep(0, n_samp)
new_dy1 <- dy1
for (i in 1:n_samp){
  new_dy1$Group <- sample(new_dy1$Group)
  new_dy1$Block <- sample(new_dy1$Block)
  new_lme_audpcy1 <- lme(AUDPC ~ as.factor(Group), random = ~ 1|as.factor(Block), data = new_dy1)
  new_anova_audpcy1 <- anova(new_lme_audpcy1, type = "marginal")
  new_df_audpcy1 <- data.frame(new_anova_audpcy1)
  new_f_audpcy1[i] <- new_df_audpcy1[2, 3]
}

pval_audpcy1 <- mean(new_f_audpcy1 >= f_audpcy1)





dy2 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/perm_data_y2.csv")

lme_peaky2 <- lme(Peak ~ as.factor(Group), random = ~ 1|as.factor(Block), data = dy2)
anova_peaky2 <- anova(lme_peaky2, type = "marginal")
df_peaky2 <- data.frame(anova_peaky2)
f_peaky2 <- df_peaky2[2, 3]

n_samp = 10000
new_f_peaky2 = rep(0, n_samp)
new_pdy2 <- pdy2
for (i in 1:n_samp){
  new_dy2$Group <- sample(new_dy2$Group)
  new_dy2$Block <- sample(new_dy2$Block)
  new_lme_peaky2 <- lme(Peak ~ as.factor(Group), random = ~ 1|as.factor(Block), data = new_dy2)
  new_anova_peaky2 <- anova(new_lme_peaky2, type = "marginal")
  new_df_peaky2 <- data.frame(new_anova_peaky2)
  new_f_peaky2[i] <- new_df_peaky2[2, 3]
}

pval_peaky2 <- mean(new_f_peaky2 >= f_peaky2)


lme_audpcy2 <- lme(AUDPC ~ as.factor(Group), random = ~ 1|as.factor(Block), data = dy2)
anova_audpcy2 <- anova(lme_audpcy2, type = "marginal")
df_audpcy2 <- data.frame(anova_audpcy2)
f_audpcy2 <- df_audpcy2[2, 3]

n_samp = 10000
new_f_audpcy2 = rep(0, n_samp)
new_dy2 <- dy2
for (i in 1:n_samp){
  new_dy2$Group <- sample(new_dy2$Group)
  new_dy2$Block <- sample(new_dy2$Block)
  new_lme_audpcy2 <- lme(AUDPC ~ as.factor(Group), random = ~ 1|as.factor(Block), data = new_dy2)
  new_anova_audpcy2 <- anova(new_lme_audpcy2, type = "marginal")
  new_df_audpcy2 <- data.frame(new_anova_audpcy2)
  new_f_audpcy2[i] <- new_df_audpcy2[2, 3]
}

pval_audpcy2 <- mean(new_f_audpcy2 >= f_audpcy2)



#for the p-values here, just write up < .001 - do not actually put 0
data.frame(response = c("peak_year_1", "peak_year_2", "audpc_year_1", "audpc_year_2"), 
           p_value = c(pval_peaky1, pval_peaky2, pval_audpcy1, pval_audpcy2))
