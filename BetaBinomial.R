#--------------------------------------------
# Poisson (likelihood) - Gamma (prior) models 
# in R
#--------------------------------------------

# 1. Histogram of Binomial distributed observations

library(tidyverse)
set.seed(2023)
s1 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 1/100))
s2 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 10/100))
s3 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 50/100))
s4 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 60/100))
s5 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 75/100))
s6 <- data.frame('data' = rbinom(n = 10000, size = 10, prob = 90/100))

p1 <- s1 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.01, n = 10') + theme_classic()
scale_color_gradient(low="firebrick1", high="firebrick4")
p2 <- s2 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.1, n = 10')  + theme_classic()
p3 <- s3 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.5, n = 10') + theme_classic()
p4 <- s4 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.6', n = 10) + theme_classic()
scale_color_gradient(low="firebrick1", high="firebrick4")
p5 <- s5 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.75, n = 10')  + theme_classic()
p6 <- s6 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = p~ '= 0.9, n = 10') + theme_classic()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)


# 2. Density plots of examples of Beta family

par(mfrow = c(2,3))
p = seq(0, 1, length = 500)
plot(p, dbeta(p, 1/2, 1/2), col = 2, lwd = 2, main = 'Beta(1/2, 1/2)', type = 'l')
plot(p, dbeta(p, 1, 1), col = 2, lwd = 2, main = 'Beta(1, 1)', type = 'l')
plot(p, dbeta(p, 5, 1), col = 2, lwd = 2, main = 'Beta(5, 1)', type = 'l')
plot(p, dbeta(p, 1, 5), col = 2, lwd = 2, main = 'Beta(5, 5)', type = 'l')
plot(p, dbeta(p, 5, 5), col = 2, lwd = 2, main = 'Beta(5, 5)', type = 'l')
plot(p, dbeta(p, 5, 40), col = 2, lwd = 2, main = 'Beta(5, 40)', type = 'l')

# 3. Density plots of posteriors

# data
n = 20; p1 = 0.8 # Binomial likelihood parameters
p <- seq(0,1,by=0.001)

# priors
alpha1 = 8; beta1 = 2
prior_mean = alpha1 / (alpha1 + beta1) # [1] 0.05
prior_variance = (alpha1*beta1) / ((alpha1 + beta1)^2 * (alpha1 + beta1 + 1))
prior1 = p^(alpha1 - 1) * (1-p)^(beta1-1) ; prior1 = prior1 / sum(prior1)

# likelihood
set.seed(2023)
data1 = rbinom(n = n, size = 1, prob = p1)
likelihood1 = dbinom(x = p1*n, size = 20, prob = p)

# posterior
lp1 = likelihood1 * prior1 ; posterior1 = lp1 / sum(lp1)
plot(lp1)

# save a copy of entire dataset, training and testing datasets in .csv
write.csv(data1, 
          "C:/Users/julia/OneDrive/Desktop/github/2. Jeffrey prior binomial/Binomial_data.csv",
          row.names = FALSE)

# posterior mean and parameters
meandata1 = mean(data1) # [1] 0.8
alpha_posterior = ((alpha1 + n*mean(data1))) # 24
beta_posterior = (n - n*mean(data1) + beta1) # 6


# ggplot

# data
n = 20; p1 = 0.8 # Binomial likelihood parameters

# priors
alpha1 = 8; beta1 = 2
p <- seq(0,1,by=0.001)
prior1 = p^(alpha1 - 1) * (1-p)^(beta1-1) ; prior1 = prior1 / sum(prior1)

# likelihood
likelihood1 = dbinom(x = p1*n, size = 20, prob = p)

# posterior
lp1 = likelihood1 * prior1 ; posterior1 = lp1 / sum(lp1)

# dataframe
data_frame <- data.frame('p' = p, 'prior' = prior1, 'likelihood' = likelihood1, 'posterior' = posterior1)

ggplot(data=data_frame) +
  geom_line(aes(x = p, y = prior1, color = 'Be(8, 2) prior'), lwd = 1.2) +
  geom_line(aes(x = p, y = likelihood1/sum(likelihood1), 
                color = 'Scaled likelihood Bin(20, 0.8)'), lwd = 1.2) +
  geom_line(aes(x = p, y = posterior1, color = 'Be(24, 6) posterior'), lwd = 1.2) +
  xlim(0, 1) + ylim(0, max(c(prior1, posterior1, likelihood1 / sum(likelihood1)))) +
  scale_color_manual(name = "Distributions", values = c("Be(8, 2) prior" = "darkred", 
                                                        "Scaled likelihood Bin(20, 0.8)" = "black",
                                                        'Be(24, 6) posterior' = 'darkblue')) +
  labs(title = 'Posterior distribution in blue - Be(24,6)',
       subtitle = 'Working example data',
       y="density", x="p") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Posterior mean, posterior variance and 95% Credible Interval including the sample median
xbar = p1
alpha_prior = 2; beta_prior = 8
alpha_posterior = ((alpha1 + n*mean(data1))) # 24
beta_posterior = (n - n*mean(data1) + beta1) # 6

pmean = alpha_posterior / (alpha_posterior + beta_posterior)
pmean
# [1] 0.8

pvariance = (alpha_posterior *beta_posterior) / ((alpha_posterior + beta_posterior)^2 + (alpha_posterior + beta_posterior + 1) )
pvariance
# [1] 0.1546724

# 95% Cedible Interval obtained by direct sampling (simulation)
set.seed(2023)
round(quantile(rbeta(n = 10^8, alpha_posterior, beta_posterior), probs = c(0.025, 0.5, 0.975)),4)
#   2.5%    50%  97.5% 
# 0.6423 0.8067 0.9201 

# Posterior mean obtained from direct sampling
set.seed(2023)
mean(rbeta(n = 10^8, alpha_posterior, beta_posterior))
# [1] 0.7999991


# Jeffrey's prior

seq=seq(from = 0, to = 1, by = 0.01)
q=dbeta(seq, 1/2, 1/2)

df=data.frame(seq,q)
ggplot(df, aes(seq)) +              
  geom_line(aes(y=q), colour="red3", size = 1.5) + 
  xlab("theta ")+ylab("density")+
  geom_hline(yintercept=1, size = 1.5) +
  xlim(0, 1)+ ylim(0, 5) +
  geom_text(x=0.7, y=4, label="Beta(1/2, 1/2)", size = 6)+
  labs(title = 'Jeffreys Beta prior',
       subtitle = 'in red Beta(1/2, 1/2) prior for a Binomial likelihood',
       y="density", x="p") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# posterior mean
mean(rbeta(n=100000000, shape1 = 16.5, shape2 = 4.5))
# 0.7857248

# credible interval
quantile(rbeta(n=100000000, shape1 = 16.5, shape2 = 4.5), probs = c(0.025, 0.975))
#      2.5%     97.5% 
# 0.5917689 0.9284637 

#----
# end
#----
