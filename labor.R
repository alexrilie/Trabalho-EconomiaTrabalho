library(readxl)
library(ggplot2)
library(tidyverse)

df = read.table("m:/Users/Marcus/Downloads/Rais 2/RAIS_VINC_PUB_CENTRO_OESTE.txt",
                sep=';', nrows=75000, colClasses=cols, header=TRUE)
rm(df)


df2 = read_excel("m:/Users/Marcus/Desktop/festa no ric.xlsx", 
                 sheet = "Informalidade")
df2$Cursos = as.numeric(df2$Cursos)
a = colnames(df2)

for(i in 3:4){
  df3 = pivot_longer(df2[,a[c(1,i,5:8)]], cols=3:6)
  colnames(df3)[2] = "Informalidade"
  g = ggplot(df3, aes(x=value, y=Informalidade)) +
    geom_point() + geom_smooth(method="lm") +
    facet_wrap(vars(name), ncol=2, scales="free_x") +
    labs(title="")
  plot(g)}

df3 = df2[,a[c(1,4,5:8)]]
df3$`2019` = df3$`2019` - df2$`2018`
df3 = pivot_longer(df3, cols=3:6)
colnames(df3)[2] = "Informalidade"
g = ggplot(df3, aes(x=value, y=Informalidade)) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(vars(name), ncol=2, scales="free_x") +
  theme_classic() + xlab("Valor") + labs(title="Figura 4: programas versus informalidade")
plot(g)

df2 = df2[4:8]
df3 = pivot_longer(df2, cols=2:5)
df3$name = as.factor(df3$name)


df4 = read_excel("m:/Users/Marcus/Desktop/festa no ric.xlsx", 
                 sheet = "parc")
df4$Total = cumsum(df4$Total)
df4$Público = cumsum(df4$Público)
df4$Privado = cumsum(df4$Privado)
ccf(df4$Total, df4$Informalidade)
#df4 = df4[,c(1,4,5)]

df4$Total = (df4$Total - mean(df4$Total))/sd(df4$Total)
df4$Informalidade = (df4$Informalidade - mean(df4$Informalidade))/sd(df4$Informalidade)
ggplot(df4, aes(x=Data)) + 
  geom_line(data=df4, aes(y=Total)) +
  geom_line(data=df4, aes(y=Informalidade))

df4 = pivot_longer(df4, cols=2:5)
df4$facet = ifelse(df4$name=="Informalidade","Informalidade","Parceiros")
ggplot(df4, aes(x=Data, y=value, color=name)) + 
  geom_line() + facet_wrap(vars(facet), nrow=2, scales="free") +
  labs(color="Legenda") +
  ylab("Valor") + theme_classic()
  


df4$Público = (df4$Público - mean(df4$Público))/sd(df4$Público)
df4$Privado = (df4$Privado - mean(df4$Privado))/sd(df4$Privado)
df4$Total = (df4$Total - mean(df4$Total))/sd(df4$Total)
df4$Informalidade = (df4$Informalidade - mean(df4$Informalidade))/sd(df4$Informalidade)
#df4 = pivot_longer(df4, cols=2:3)
df4 = pivot_longer(df4, cols=2:5)

ggplot(df4, aes(x=Data, y=value, color=name)) + 
  geom_line() + labs(color="Legenda") + ylab("Valor") +
  theme_classic()

df5 = read_excel("m:/Users/Marcus/Desktop/festa no ric.xlsx", 
                 sheet = "cred")
df5 = df5[,c(1,2,3)]
colnames(df5)[3] = "Informalidade"
ggplot(df5, aes(x=Microcrédito, y=Informalidade)) + geom_point() + geom_smooth(method="lm")

a = ccf(df5$Microcrédito, df5$Informalidade, main="Figura 5: microcrédito versus informalidade")

df5$Microcrédito = (df5$Microcrédito - mean(df5$Microcrédito))/sd(df5$Microcrédito)
df5$Informalidade = (df5$Informalidade - mean(df5$Informalidade))/sd(df5$Informalidade)
df5 = pivot_longer(df5, cols=2:3)
ggplot(df5, aes(x=Data, y=value, color=name)) +
  geom_line()

a = data.frame(x = seq(0.05,0.2,0.001))
a$y = exp(1/a$x)*sin(exp(1/a$x))
ggplot(a, aes(x=x,y=y)) +
  geom_line()

\plot(a$x, a$y)
