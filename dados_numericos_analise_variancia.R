library(MASS)
anorexia
testeFT <- subset(anorexia, Treat == "FT")
testeCBT <- subset(anorexia, Treat == "CBT")
testeCont <- subset(anorexia, Treat == "Cont")
mean(testeFT$Postwt) - mean(testeFT$Prewt)
mean(testeCBT$Postwt) - mean(testeCBT$Prewt)
mean(testeCont$Postwt) - mean(testeCont$Prewt)

#Postwt-Prewt (post-therapy weight minus pre-therapy weight)
anorexia$Postwt-anorexia$Prew

mean(anorexia$Postwt) - mean(anorexia$Prewt)
negativos <- subset(anorexia, (anorexia$Postwt - anorexia$Prewt) < 0)
positivos <- subset(anorexia, (anorexia$Postwt - anorexia$Prewt) >= 0)
perc_neg <- length(negativos[,1]) / length(anorexia[,1])
paste("Percentual com perda de peso:", perc_neg, "%", sep = " ")
perc_pos <- length(positivos[,1]) / length(anorexia[,1])
paste("Percentual com ganho de peso:", perc_pos, "%", sep = " ")

t.test(anorexia$Postwt-anorexia$Prewt, mu=0) 

plot(Postwt-Prewt ~ Treat, data=anorexia, xlab= "Tratamento", ylab = "Diferença de peso pré e pós tratamento", main = "Tratamentos para Anorexia")
#Para analisar a diferença entre as tecnicas utilizamos a função Analisys of variance
aov(Postwt-Prewt ~ Treat, data=anorexia) 

