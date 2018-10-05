#######################
library(MASS)
Cars93
#Variavel independente Price - dependente Intervalos
hist(Cars93$Price, xlab="Preco (x $1,000)", col="red", xlim = c(0,70),main = "Pricos de 93 Modelos de carros ano 1993")
#Plotamos a probabilidade daquele resultado
hist(Cars93$Price, xlab="Preco (x $1,000)",col="red", xlim = c(0,70),main = "Pricos de 93 Modelos de carros ano 1993",probability= TRUE)
lines(density(Cars93$Price), col="blue", lwd=2)

table(Cars93$Type) 
barplot(table(Cars93$Type), col="green",ylim=c(0,25), xlab="Tipo",
        ylab="Frequencia", axis.lty = "solid", space = .05)       
#Pizza
pie(table(Cars93$Type)) 

#
type.frame <- data.frame(table(Cars93$Type))
type.frame
dotchart(type.frame$Freq,type.frame$Var1) 
dotchart(type.frame[,2],type.frame[,1]) 


#
rev.values <-c(1000,1300,1300,1100,1400,800,1200,1500,1850,2330,860,1300,1400,1600,1970,570,380,450,465,580,155,190,210,250,300)
space.rev <- matrix(rev.values,nrow=5,byrow = T) 
colnames(space.rev) <-c("1990","1991","1992","1993","1994") 
rownames(space.rev) <- c("Commercial SatellitesDelivered","Satellite Services","Satellite GroundEquipment","Commercial Launches","Remote Sensing Data") 
space.rev
color.names = c("black","grey25","grey50","grey75","white")
barplot(space.rev, beside = T, xlab= "Year",ylab= "Revenue(X $1,000)", col=color.names) 
legend(1,2300,rownames(space.rev), cex=0.7, fill = color.names, bty = "n") 
barplot(space.rev, xlab= "Year",ylab= "Revenue(X $1,000)", col=color.names) 
legend(0.1,6800,rownames(space.rev), cex=0.7, fill = color.names, bty = "n") 


plot(Cars93$Horsepower, Cars93$MPG.city,xlab="Horsepower",ylab="MPG City", 
     main ="MPG City vsHorsepower") 
plot(Cars93$MPG.city ~ Cars93$Horsepower,xlab="Horsepower",ylab="MPG City", 
     main ="MPG City vsHorsepower") 

#Mudo os pontos de exibição para caracteres
pchchar = as.character(Cars93$Cylinders)
#Insiro os caracteres em PCH
plot(Cars93$Horsepower,Cars93$MPG.city, xlab="Horsepower",ylab="MPG City", main = "MPG City vs Horsepower", pch= pchchar)
#Scatter plot matrix
#Crio os subconjuntos de dados
cars.subset <- subset(Cars93, select = c(MPG.city,Price,Horsepower)) 
head(cars.subset)
pairs(cars.subset) 
cars.subset <- subset(Cars93, select = c(MPG.city,Price,Horsepower, Cylinders)) 
head(cars.subset)
pairs(cars.subset) 
cars.subset <- subset(Cars93, select = c(MPG.city,Price,Horsepower, Cylinders, Passengers)) 
head(cars.subset)
pairs(cars.subset) 

#boxplot
#Se for utilizar dados de DF separados esta á a melhor maneira
boxplot(Cars93$Horsepower ~ Cars93$Cylinders, xlab="Cylinders",ylab="Horsepower") 
#Se os dados estiverem no mesmo DF basta informar o DF no parâmetro data e 
#indicar apenas as colunas em questão
boxplot(Horsepower ~ Cylinders, data = Cars93,xlab="Cylinders", ylab="Horsepower")
library(ggplot2)
##GGPLOT
ggplot(Cars93, aes(x=Price))
#para cada necessidade gráfica há uma função geom
#neste caso para histograma utilizamos geom_histogram()
ggplot(Cars93, aes(x=Price)) + geom_histogram() #Estas são as regras gramaticais para gerar o histograma
#ggplot tem os dados, mapeamentos esteticos e objetos geometricos para definir os dados plotados modos de plotam estetica (forma) e geometrica tipo de graficação
#cada barra é um bin, o padrão ggplot é 30 bins que pode ter seus parametros alterados
ggplot(Cars93, aes(x=Price)) + geom_histogram(binwidth=5, col="black", fill="red") +
  labs(x = "Preço (x $1000)", y="Frequencia",title="Preços de 93 modelos de carros de 1993") 

ggplot(Cars93, aes(x=Type))+geom_bar() +labs(y="Frequencia", title="Modelos de carros de 1993 e frequencia") 


type.frame <- data.frame(table(Cars93$Type)) 
colnames(type.frame)<- c("Tipo","Frequencia") 
type.frame
ggplot(type.frame, aes(x=Frequencia,y= Tipo))+
  geom_point() 

ggplot(type.frame, aes(x=Frequencia,y=reorder(Tipo,Frequencia))) +
         geom_point(size =4) 
       
#Funções adicionais modificam a aparência geral do gráfico. 
#Uma família dessas funções é chamada de theme. 
#Um membro desta família, theme_bw (), remove o fundo cinza. Adicionando o 
#tema () com argumentos apropriados a) remove as linhas verticais na grade e 
#b) enrubesce as linhas horizontais e as faz pontilhadas:
theme_bw() + 
  theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(color = "black",linetype = "dotted")) 
ggplot(type.frame, aes(x=Frequencia,y=reorder(Tipo,Frequencia))) + 
  geom_point(size = 4) +theme_bw() + 
  theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(color = "black",linetype= "dotted"))+
  labs(y="Tipo") 

##Bar plots
#Retomando space.rev
#Reshape ofere interessantes operações de transformaão
library(reshape2)
#reshape::melt transforma formato largo em longo e cast() faz o inverso longo para largo
space.melt <- melt(space.rev) 
colnames(space.melt) <- c("Industry","Year","Revenue") 
head(space.melt, 15)
ggplot(space.melt, aes(x=Year,y=Revenue,fill=Industry)) +
  geom_bar(stat = "identity", position = "dodge", color ="black") 
ggplot(space.melt, aes(x=Year,y=Revenue,fill=Industry)) +
  geom_bar(stat = "identity",color ="black") +
#stat = "identity" utiliza os proprios numeros como dados, sem ajustes
  #combinas as barras com níveis de cinza
  scale_fill_grey(start = 0,end = 1) +
  #Cria o lavel Y
  labs(y="Receita (X $1,000)", x="Anos") +
  #Remove o fundo cinza
  theme_bw() +
  #Remove as linhas verticais
  theme(panel.grid.major.x = element_blank()) 

##Scatter Plot
ggplot(Cars93,aes(x=Horsepower,y=MPG.city)) +
  geom_point() + labs(y="Milhas por galão na Cidade", x="CVs") +
  theme_bw() + theme(panel.grid=element_blank()) 

ggplot(Cars93, aes(x = Horsepower,y = MPG.city,label = Cylinders)) + 
  geom_text() + labs(y="Milhas por galão na Cidade", x="CVs")

cars.subset <- subset(Cars93, select = c(MPG.city,Price,Horsepower))
head(cars.subset)
#install.packages('GGally')
library(GGally)
#A função ggpairs de GGally fornece maios para criar uma matriz de vário gráficos de dispersão
ggpairs(cars.subset)

cars.subset <- subset(Cars93, select = c(MPG.city,Price,Horsepower,Cylinders))
ggpairs(cars.subset) 


########
data(tips, package="reshape")

ggpairs(data=tips, # data.frame with variables
        columns=1:3, # columns to plot, default to all.
        title="tips data", # title of the plot
        colour = "sex") # aesthetics, ggplot2 style

pm = ggpairs(data=tips,
             columns=1:3, 
             upper = list(continuous = "density"),
             lower = list(combo = "facetdensity"),
             title="tips data",
             colour = "sex")
print(pm)


cp = ggplot(data.frame(x=1:10, y=1:10)) +
  geom_point(aes(x, y))

putPlot(pm, cp, 2, 3)
########

ggplot(Cars93, aes(x=Cylinders, y= Horsepower)) +
  geom_boxplot() 

#adicionar os pontos de dados em adição ao boxplot
ggplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
  geom_boxplot()+
  geom_point() 


ggplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
  geom_boxplot()+
  geom_point()+
  #gem_jitter permite um overplotting onde junto com boxplot podemos ter o scatterplot
  #Ele adiciona uma pequena quantidade de variação aleatória à localização de cada ponto
  geom_jitter() 

#medias
Horsepower.USA <- Cars93$Horsepower[Cars93$Origin == "USA"] 
mean(Horsepower.USA) 
Horsepower.ForaUSA <- Cars93$Horsepower[Cars93$Origin == "non-USA"] 
mean(Horsepower.ForaUSA)
#Eliminando as recursividades Cars93$Horsepower
with(Cars93, mean(Horsepower[Origin == "USA"]))
with(Cars93, mean(Horsepower[Origin == "USA" & Cylinders ==4])) 
ggplot(Cars93, aes(x=Horsepower)) + geom_histogram(color="black", fill="white",binwidth = 10)
ggplot(Cars93, aes(x=Horsepower)) +
  geom_histogram(color="black", fill="white",binwidth = 10)+
  facet_wrap(~Origin)
#Elimino os 5% maiores e os 5% menores para podar os boxplots
mean(Horsepower.USA, trim =.05) 

#Média Geométrica
library(psych)
x <- seq(1,5)
x2 <- x^2
x2[2] <- NA
X <- data.frame(x,x2)
geometric.mean(x)
geometric.mean(x2)
geometric.mean(X)
geometric.mean(X,na.rm=FALSE)

## generate random lognormal data
#Calculates the geometric mean or geometric standard deviation.
#install.packages('FSA')
library(FSA)
d <- rlnorm(500,meanlog=0,sdlog=1)
# d has a mean on log scale of 0; thus, gm should be exp(0)~=1
# d has a sd on log scale of 1; thus, gsd should be exp(1)~=2.7
geomean(d)
geosd(d)
## Demonstrate handling of zeros and negative values
x <- seq(1,5)
# this will given an error
try(geomean(x))
# this will only give a warning, but might not be what you want
geomean(x,zneg.rm=TRUE)

