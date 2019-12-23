base = read.csv("HTRU_2.csv", header=FALSE)

colnames(base) <- c("mean_integrated", "standard_integrated", "excess_integrated", "skewness_integrated", "mean_curve", "standard_curve", "excess_curve", "skewness_curve", "class")

summary (base)
table (base$class)
sapply(base, class)

library(ggplot2)
ggplot(base, aes(x=class))+ 
  geom_histogram(binwidth=0.5,color="red", fill="orange")
table(base$class)

# Divisão entre treinamento e teste
library(caTools)
set.seed(1)
divisao = sample.split(base$class, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

# aplicando o modelo
library(h2o)
h2o.init(nthreads = -1) 
classificador = h2o.deeplearning(y = 'class',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(100),
                                 epochs = 100)
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-9]))
previsoes = (previsoes > 0.5)
previsoes = as.vector(previsoes)
matriz_confusao = table(base_teste[, 9], previsoes)
library(e1071)
confusionMatrix(matriz_confusao)

# Acurácia de 98.17 %
