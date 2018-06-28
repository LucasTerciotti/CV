load("C:/Users/lucas/Desktop/Analise de Dados/CV/wine_data.Rdata")
library(caret)

wine <- wine_df
head(wine)
str(wine)
summary(wine)

#definindo V1 como fator por indicar de qual região o vinho provém:
wine$V1 <- as.factor(wine$V1)

#separação dos dados em treinamento e teste:
set.seed(300)
indiceTrain <- createDataPartition(y = wine$V1, p = 0.75, list = FALSE) 
#a intenção é criar uma partição com distribuição semelhante entre treinamento e teste
training <- wine[indiceTrain,]
testing <- wine[-indiceTrain,]

#gerar coluna na tabela indicando a qual grupo o dado pertence:
wine$grupo = "Treinamento"
wine$grupo[-indiceTrain]="Teste"  

#Verificara a distribuição dos dados de teste em relação aos dados de treinamento:
m <- ggplot(data = wine, aes(x=V1,fill=grupo)) 
m + geom_density(alpha=0.4) 

#Preparando os dados: é necessário normalizá-los, abaixo será feito isoladamente
trainX <- training[names(training) != "V1"]
#retiramos nossa coluna V1 que indica as classificações a serem preditas
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#Porém, o pacote carat permite implementar essa normalização no processamento completo abaixo:
set.seed(400)
#aqui definimos o método de Cross Validation a ser usado:
ctrl <- trainControl(method = "repeatedcv",number= 10,repeats = 10) 

help("trainControl")
#testado repeats = 3, 5 e 10, , além de alterar valores de k folders para 15 e 20 (10 default),
#e percebe-se o aumento no tempo de treinamento


knnFit <- train(V1 ~ V2 +V3 +V4 +V5 +V6 +V7 +V8 +V9 +V10 +V11+ V12+ V13 +V14, data = training,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center","scale"),
                tuneLength = 20)
knnFit

gridK <- expand.grid( k = c(15:25) )
knnFit <- train(V1 ~ V2 +V3 +V4 +V5 +V6 +V7 +V8 +V9 +V10 +V11+ V12+ V13 +V14, data = training,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center","scale"),
                )
knnFit

help(train)
#alterações a serem feitas aqui:

#Ao rodar repetidas vezes o algoritmo acima, pode-se perceber que 
#o valor varia. De acordo com a análise final, o k ideal era: k = 25
#Alcançando uma acurácia de 0.9762601

#matriz de confusão:
confusionMatrix(knnFit)

#Cross-Validated (10 fold, repeated 10 times) Confusion Matrix 

#(entries are percentual average cell counts across resamples)

#Reference
#Prediction    1    2    3
#       1   33.3  0.8  0.0
#       2    0.1 37.7  0.0
#       3    0.0  1.5 26.7

#Accuracy (average) : 0.9763 - isso com base nos próprios dados de treinamento. Devemos agora aplicar no
#grupo de teste. Ir para adendo da aula 19/06.

plot(knnFit)


help("train")
#tuneGrid:
#A data frame with possible tuning values. The columns are named the same as the tuning parameters. 
#Use getModelInfo to get a list of tuning parameters for each model or see 
#http://topepo.github.io/caret/available-models.html. (NOTE: If given, this argument must be named.)

plot(knnFit)

#Aula 19/06:
knnpredict <- predict(knnFit, newdata = testing)
table(knnpredict,testing$V1)
prop.table(table(knnpredict,testing$V1))
#Sensibilidade = acertos positivos / total de positivos -> TP/(TP+FN)
#Especificidade = acertos negativos / total de negativos -> TN/(TN+FP)
#eficiência = (sensi+espec)/2

0.32558140 + 0.32558140 + 0.27906977
#0.9302326 - Acurácia ao aplicar o modelo aos dados de teste

#grafico de análise de sensibilidade vs especificidade: curva ROC

#dataset de câncer de mama:
#pior um falso positivo ou um falso negativo?
#Ser um falso negativo nesse caso indicaria que a pessoa tem cancer, porem foi diagnosticada como
#saudavel. Entao nao quero que meu modelo tenha esse problema.

#todos como nao recorrentes: acurácia boa, porem modelo ruim. :/
#e o inverso: acuracia ruim, modelo tambem ruim

#paradoxo da acuracia: observar desequilibrio entre classes
#medidas adicionais para avaliar um classificador

#precisão: acertos positivos/total de predições positivas -> TP/(TP+FP)
#F1 score: meio termo entre sensi e precisao

#KAPPA: concordância entre dois observadores