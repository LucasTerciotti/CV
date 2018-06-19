load("C:/Users/lucas/Desktop/Analise de Dados/CV/regressaoKNNcv_pp.Rdata")
View(pp)

#Lazy Learner: não há abstração e generalização, mas não pode ser subestimado, geralmente usado em problemas de classificação

#HIPERPARÂMETROS 
library(caret)
#importantes todos normalizados na mesma escala
library(ggplot2)

#conhecendo os dados (trabalhando com regressão)
pp <- pp
head(pp)
str(pp)
summary(pp)

#dividindo os dados em testes e treinamentos usando carat
set.seed(300)
indiceTrain <- createDataPartition(y = pp$valor, p = 0.75, list = FALSE) #a intenção é criar uma distribuição semelhante
training <- pp[indiceTrain,]
testing <- pp[-indiceTrain,]

#checando a distribuição dos dados
pp$grupo = "Treinamento" #vai criar um grupo novo, já que não existe: estes vão para o grupo de trainning
pp$grupo[-indiceTrain]="Teste"  #aqui vão dar label aos testing

m <- ggplot(data = pp, aes(x=valor,fill=grupo)) 
m + geom_density(alpha=0.4) #alpha é a transparência

#muito sensivel à escala, ja que usa o mais próximo: uma parte de pré processamento, usando center e scale
#A coluna classe não será considerada por ser uma categoria (fator)

trainX <- training[,names(training) != "valor"] #retirei o valor, todas as outras vão. Pre-processamento automático
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues # a resposta incidca que 2 foram centralizados (retirar a média de todos os valores, centralizando elas na média), 2 escalados, e 1 ignorado - classes
str(trainX)

#algorito de uma tacada só
set.seed(400)
ctrl <- trainControl(method = "repeatedcv",repeats = 3) 

gridK <- expand.grid( k = c(3:5) )

knnFit <- train(valor ~ PredictorA + PredictorB,
                data = training, method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"),
                #Definindo a faixa de parametros a ser usada
                tuneGrid = gridK )

knnFit

knnFit <- train(valor ~ PredictorA + PredictorB, data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit
plot(knnFit)

