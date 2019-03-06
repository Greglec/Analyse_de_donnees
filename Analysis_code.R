data = read.csv("/Users/gregoirelecallier/Desktop/UPX/ADD/data_Roy-Uni.csv",header = TRUE, sep =";")
data1 = data[2:6]
attach(data1)
#Question 1:

summary(data1)
#entre 1991 et 2010
#Taux de croissance :
plot(Annual_growth~data$Annee,type = "l")
n = 20
TCAM = (prod((Annual_growth/100)+1)^(1/n)-1)*100 #C'est le taux de croissance qu'il aurait fallu chaque année depuis 1991 pour arriver au niveau de PIB de 2010
TCAM
var(Annual_growth)

#Consommation privée :
plot(Private_consumption~data$Annee,type = "l",ylim=c(50,70))
var(Private_consumption)

#Chomage: 
var(Unemployment) #Variance élevée ce qui confirme une forte variabilité du chômage au RU entre 1991 et 2010.
plot(Unemployment~data$Annee,type = "l",ylim = c(1,16))

#Inflation:
var(Inflation)
#On retire les valeurs abérantes :
Inflation1 = Inflation[-4]
Inflation2 = Inflation1[-4]
var(Inflation2)
plot(Inflation~data$Annee,type = "l")

boxplot(data1)
cor(data1)
plot(Unemployment~Private_consumption,col=grey(0.6))
plot(Investment~Private_consumption,col=grey(0.6))
plot(Inflation~Private_consumption,col=grey(0.6))
pairs(data1) #Représentation générale des liaisons entre les variables

#Question 2 :
#b)
boxplot(data1)
res.acp = prcomp(data1, scale=TRUE)
summary(res.acp)
val.propres = res.acp$sdev^2 
plot(val.propres,type = "b",ylab="Valeurs propres",xlab="Composante",main="Eboulis") #Eboulis des valeurs propres

z = res.acp$x
boxplot(z)
#c)d)
cc = cor(data1,z[,1:2])
cc
s.corcircle(cc,lab=names(data1))

#question 3:

tdata= data.frame(t(data1))
names(tdata) = tdata[1,]
tdata1=tdata[-1,]
tdata=tdata1
# perte d'inertie : 
d=dist(scale(tdata)) #sur les donnees centrees reduites
clas=hclust(d^2,meth="ward")
plot(clas$height,type="b",main="Perte d’inertie inter-classes", xlab="nombre d’iterations")
#et le dendogramme :
clas=hclust(d^2,meth="ward")
plot(clas)
#etape 0 50 groupes
#etape 49 1 groupe
#trouver le juste milieu 
##pour trois groupes on a la variabilité entre les classes 
gr3 = cutree(clas,k=3)
table(gr3)