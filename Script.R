# Assuming you have the tidyverse package installed
# If not, install it using install.packages("tidyverse")

library(tidyverse)

set.seed(123)  # Setting seed for reproducibility

farm_data <- tibble(
  farm_id = seq(1, 100, by = 1),
  region = sample(c("North Island", "South Island"), 100, replace = TRUE),
  herd_size = sample(100:500, 100, replace = TRUE),
  milk_production = herd_size * rnorm(100, mean = 450, sd = 50),
  farm_size = herd_size*(1/rnorm(100, mean = 3.5, sd = .75)),
)
# Create contour plots
contour_plot_herd_size_milk <- ggplot(farm_data, aes(x = herd_size, y = milk_production)) +
  geom_point() +
  geom_density_2d_filled() +
  labs(title = "Contour Plot: Herd Size vs Milk Production")

contour_plot_farm_size_rainfall <- ggplot(farm_data, aes(x = farm_size, y = milk_production)) +
  geom_point() +
  geom_density_2d_filled() +
  labs(title = "Contour Plot: Farm Size vs Rainfall")

# Display the plots
print(contour_plot_herd_size_milk)
print(contour_plot_farm_size_rainfall)

farm_data<-farm_data_2<-farm_data %>% 
  mutate(farm_size = log(farm_size),
         herd_size = log(herd_size),
         milk_production = log(milk_production))




# Placeholder -------------------------------------------------------------


library(readxl)
library(lme4)
library(mnonr)
library(moments)
library(readr)
library(tidyr)
library(tidyverse)


centered_BOP_data<-scale(farm_data %>% filter(region =="North Island") %>% 
                           select(-region),center = T,scale = F) 

eigen_decomp<-eigen(t(centered_BOP_data)%*%centered_BOP_data)

data_proj<-centered_BOP_data%*%eigen_decomp$vectors
colnames(data_proj)<-colnames(reduced_data_BOP_DBL)

pla_av<-colMeans(data_proj)
pla_cv<-cov(data_proj)
pla_sk<-skewness(data_proj)
pla_ku<-kurtosis(data_proj)


plc<-unonr(2500,
           pla_av,
           pla_cv,
           skewness=pla_sk,
           kurtosis=pla_ku,
           empirical=FALSE)
colnames(plc)<-colnames(farm_data %>% filter(region =="North Island") %>% 
                          select(-region))

real_pred<-plc%*%t(eigen_decomp$vectors)

means<-t(replicate(nrow(real_pred),colMeans(farm_data %>%
                                              filter(region =="North Island") %>% 
                                              select(-region))))

real_pred<-real_pred + means


real_pred %>% as_tibble() %>% 
  mutate(herd_size = exp(herd_size),
         milk_production = exp(milk_production),
         farm_size =exp(farm_size)) %>% 
ggplot(aes(x = farm_size , y = milk_production )) +
  geom_point() +
  labs(title = "Contour Plot: Farm Size vs Rainfall")

farm_data %>%
  mutate(herd_size = exp(herd_size),
         milk_production = exp(milk_production),
         farm_size =exp(farm_size)) %>% 
  ggplot(aes(x = farm_size , y = milk_production ))  +
  geom_point() +
  labs(title = "Contour Plot: Farm Size vs Rainfall")

