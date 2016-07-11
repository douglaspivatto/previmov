# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, Metrics, caret)

source("C:/Users/dougl/previmov/dados_novos.R") #dados salvos na variavel aptos
unique(aptos, incomparables = FALSE, fromLast = FALSE)
(uaptos <- unique(aptos))
(uaptos2 <- unique(aptos, fromLast = TRUE)) # different order
View(uaptos)

# Separar conjuntos train e test
set.seed(4321)
trainIndex <- createDataPartition(uaptos$preco, p=.7, list=F)
trainData <- uaptos[trainIndex, ]
testData <- uaptos[-trainIndex, ]

# Criar variaveis do conjunto train e escalonar dados
dummiesTrain <- predict(dummyVars(~ bairro, data = trainData), newdata = trainData)

dataTrain <- data.frame(preco = trainData$preco, areavaga = trainData$area*trainData$vaga, area = trainData$area, area2 = trainData$area^2, area3 = trainData$area^3, areaquarto = trainData$area/trainData$quartos, quartos = trainData$quartos, quartos2 = trainData$quartos^2, quartos3 = trainData$quartos^3, suites = trainData$suites, suites2 = trainData$suites^2, suites3 = trainData$suites^3, vagas = trainData$vagas, vagas2 = trainData$vagas^2, vagas3 = trainData$vagas^3, dummiesTrain[,2:ncol(dummiesTrain)]) # Incluir novas variaveis aqui

maxs <- apply(dataTrain, 2, max)
mins <- apply(dataTrain, 2, min)
scaledTrain <- as.data.frame(scale(dataTrain, center = mins, scale = maxs - mins))
adj1 <- (max(dataTrain$preco)-min(dataTrain$preco))
adj2 <- min(dataTrain$preco)

# Funcao para computar MAE utilizando train
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
   out <- mae(data$obs, data$pred)  
   names(out) <- "MAE"
   out
}

formula1 <- preco ~ area + quartos + suites + vagas + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
formula2 <- preco ~ area + area2 + area3 + areaquarto + quartos + quartos2 + quartos3 + suites + suites2 + vagas + vagas2 + vagas3 + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE

# Metodo de cross validacao da funcao train
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, summaryFunction = maeSummary, savePredictions = "final")
#diminuindo as repetições de 10 para 5, melhora-se o modelo diminuindo o seu custo computacional
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = maeSummary, savePredictions = "final")

# Melhor modelo ate o momento
svmGrid <- expand.grid(sigma= 2^c(-10, -5, 0), C= 2^c(0:5))
set.seed(21)
svm.radial2 <- train(formula2, data = scaledTrain, method = "svmRadial", trControl=fitControl, metric = "MAE", maximize = FALSE, tuneGrid = svmGrid)
mse.svmradial2 <- mse(svm.radial2$pred$obs*adj1+adj2, svm.radial2$pred$pred*adj1+adj2)
mae.svmradial2 <- mae(svm.radial2$pred$obs*adj1+adj2, svm.radial2$pred$pred*adj1+adj2)

# Modelo implementado, alterado pelo usuário
set.seed(21)
modelo <- train(formula2, data = scaledTrain, method = "lm", trControl = fitControl, metric = "MAE", maximize = FALSE)
mse.modelo <- mse(modelo$pred$obs*adj1+adj2, modelo$pred$pred*adj1+adj2)
mae.modelo <- mae(modelo$pred$obs*adj1+adj2, modelo$pred$pred*adj1+adj2)

# Criar variaveis do conjunto test e escalonar dados
dummiesTest <- predict(dummyVars(~ bairro, data = testData), newdata = testData)

dataTest <- data.frame(preco = testData$preco, areavaga = testData$area*testData$vaga, areaquarto = testData$area/testData$quartos, area = testData$area, area2 = testData$area^2, area3 = testData$area^3, quartos = testData$quartos, quartos2 = testData$quartos^2, quartos3 = testData$quartos^3, suites = testData$suites, suites2 = testData$suites^2, suites3 = testData$suites^3, vagas = testData$vagas, vagas2 = testData$vagas^2, vagas3 = testData$vagas^3, dummiesTest[,2:6]) #Incluir as mesmas variaveis do conjunto train

scaledTest <- as.data.frame(scale(dataTest, center = mins, scale = maxs - mins))

# Avaliar resultado do melhor modelo no conjunto test
pred_test <- predict(modelo, newdata = scaledTest)
pred_test <- pred_test*adj1+adj2
mse(pred_test, testData$preco)
mae(pred_test, testData$preco)

View(cbind(mae.svmradial2, mae.modelo))

# Pode se dizer que com o comando 'unique' no script houve uma pequena melhora no modelo,
# haja vista que retirando os itens que tem informações idênticas,
# diminui-se o risco de dupla contagem
# (cabe observar que podem ter apartamentos diferentes com as mesmas informações,
# o que se fez foi diminuir o risco de se ter dupla contagem de um mesmo apartamento),
# tornando o modelo, caso não houvesse melhora, mais de acordo com a realidade. 