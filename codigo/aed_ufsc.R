# Análise Exploratória de Dados
# Fonte: https://www.inf.ufsc.br/~andre.zibetti/probabilidade/aed.html

#Os dados estão disponíveis em um arquivo csv no endereço http://www.leg.ufpr.br/~fernandomayer/dados/milsa.csv.

##Apresentação e organização de dados
# A organização dos dados coletados é fundamental para que não hajam erros de 
# processamento e perda de informações. Deve ser feito em um programa apropriado. 
# Exemplo: planilhas eletrônicas e bancos de dados.


#Um arquivo csv é um arquivo de texto (assim como um txt), mas as colunas de um
#conjunto de dados são separados por vírgula (csv = comma separated values). 
#Arquivos de texto como esse podem ser abertos em qualquer editor de texto, e 
#também em planilhas eletrônicas.

## Importando os dados para o R
milsa <- read.csv("./dados/milsa.csv")
str(milsa)

#O  primeiro passo é classificar todas as variáveis desse conjunto de dados:

#IVariável	    Classificação
#I-------------I-----------------------I
#I Funcionario I Quantitativa discreta I
#I Est.civil   I Qualitativa nominal   I
#I Inst	       I Qualitativa ordinal   I
#I Filhos	   I Quantitativa discreta I 
#I Salario	   I Quantitativa contínua I
#I Anos	       I Quantitativa discreta I
#I Meses       I Quantitativa discreta I
#I Regiao	   I Qualitativa nominal   I
#I-------------I-----------------------I

#Análise univariada
#A análise univariada consiste basicamente em, para cada uma das variáveis individualmente:
#Classificar a variável quanto a seu tipo: qualitativa (nominal ou ordinal) ou quantitativa (discreta ou contínua)
#Obter tabelas, gráficos e/ou medidas que resumam a variável

##Variável Qualitativa Nominal
#A variável Est.civil é uma qualitativa nominal. 
#Desta forma podemos obter:
# - Uma tabela de frequências (absolutas e/ou relativas)
# - Um gráfico de barras ou de setores
# - A “moda”, i.e. o valor que ocorre com maior frequência

civil.tb <- table(milsa$Est.civil)
cbind("f" = civil.tb)
#          f
#casado   20
#solteiro 16
civil.tb
#casado solteiro 
#    20       16
cbind("f" = addmargins(civil.tb))
#          f
#casado   20
#solteiro 16
#Sum      36

#frequência relativa
cbind("fr" = prop.table(civil.tb))
#                fr
#casado   0.5555556
#solteiro 0.4444444

cbind("fr" = addmargins(prop.table(civil.tb)))
#                fr
#casado   0.5555556
#solteiro 0.4444444
#Sum      1.0000000

#Os gráficos de barras e de setores são adequados para representar esta variável.
#O gráfico de barras é formado pelas categorias no eixo X, e pela frequência 
#no eixo Y. A frequência utilizada pode ser tanto a absoluta quanto a relativa, 
#conforme for o caso.
par(mfrow = c(1, 2))
barplot(civil.tb, ylab = "Frequência absoluta")
barplot(prop.table(civil.tb), ylab = "Frequência relativa",
        ylim = c(0, .6))
par(mfrow = c(1,1))

#O gráfico de setores (ou de pizza, ou torta, ou diagrama circular) também pode 
#ser utilizado, mas apresenta uma maior limitação. Independente da frequência 
#utilizada, cada setor terá a mesma área. Além disso, quando existem muitas 
#categorias, e/ou as categorias possuem frequências semelhantes, a diferenciação
#dos setores é dificultada.
pie(civil.tb)

#A moda de qualquer variável é definida como o valor mais frequente encontrado 
#na amostra. No caso de variáveis qualitativas, a moda é a categoria que 
#apresenta maior frequência. Nesse exemplo, a moda seria então:
names(civil.tb)[which.max(civil.tb)]
#[1] "casado"

#Variável Qualitativa Ordinal
#Para exemplificar como obter análises para uma variável qualitativa ordinal 
#vamos selecionar a variável Inst, que verificou o grau de instrução dos 
#funcionários.
#A frequência absoluta é a contagem do número de vezes que cada categoria foi observada
inst.tb <- table(milsa$Inst)
cbind("f" = addmargins(inst.tb))

#As frequências relativas também são obtidas através da divisão da frequência 
#absoluta de cada classe pelo total
cbind("f" = addmargins(inst.tb),
      "fr" = addmargins(prop.table(inst.tb)))

#O gráfico de setores não é adequado para este tipo de variável por não 
#expressar a ordem dos possíveis valores. Usamos então apenas um gráfico de 
#barras conforme mostrado abaixo

barplot(inst.tb)

#Em alguns casos podemos querer mostrar o gráfico de barras com as barras 
#classificadas da menor para a maior, ou vice-versa, independente da ordem 
#dos níveis. O importante é sempre deixar claro as categorias de cada barra.
par(mfrow = c(1,2))
## Menor para maior
barplot(sort(inst.tb))
## Maior para menor
barplot(sort(inst.tb, decreasing = TRUE))
par(mfrow = c(1,1))

#Para uma variável ordinal, a moda também é especificada como a categoria de 
#maior frequência, ou seja,
names(inst.tb)[which.max(inst.tb)]
#[1] "2o Grau"

#Variável quantitativa discreta
#Vamos agora usar a variável Filhos (número de filhos) para ilustrar algumas 
#análises que podem ser feitas com uma quantitativa discreta.

#Frequência absoluta, com o total de observações
filhos.tb <- table(milsa$Filhos)
cbind("f" = addmargins(filhos.tb))
#     f
#0    4
#1    5
#2    7
#3    3
#5    1
#Sum 20

#Note que a soma foi 20, ao invés das 36 observações totais da planilha original.
#Como você deve imaginar ao ter inspecionado a tabela, isso ocorre pelo fato de
#que existem algumas observações perdidas para essa variável. 
#Mais especificamente, existem 16 observações faltantes, aquelas marcadas com NA. 
#Se for desejável, pode-se incluir a contagem de observações faltantes na tabela 
#de frequência.
cbind("f" = addmargins(table(milsa$Filhos, useNA = "always")))
#      f
#0     4
#1     5
#2     7
#3     3
#5     1
#<NA> 16
#Sum  36

#Portanto, para esse exemplo, as frequências relativas seriam
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)))
#    f   fr
#0    4 0.20
#1    5 0.25
#2    7 0.35
#3    3 0.15
#5    1 0.05
#Sum 20 1.00

#Para variáveis cujos valores possuem ordenação natural (qualitativas ordinais e 
#quantitativas em geral), faz sentido calcularmos também as frequências acumuladas.
#A frequência acumulada até um certo valor é obtida pela soma das frequências de 
#todos os valores da variável, menores ou iguais ao valor considerado.

filhos.tba <- as.table(cumsum(filhos.tb))
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)),
      "F" = c(filhos.tba, NA))
#     f   fr  F
#0    4 0.20  4
#1    5 0.25  9
#2    7 0.35 16
#3    3 0.15 19
#5    1 0.05 20
#Sum 20 1.00 NA

#Também podemos definir a frequência acumulada relativa, que é o resultado da
#divisão das frequências acumuladas pelo total de observações, ou seja
filhos.tba <- as.table(cumsum(filhos.tb))
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)),
      "F" = c(filhos.tba, NA),
      "Fr" = c(filhos.tba/20, NA))
#     f   fr  F   Fr
#0    4 0.20  4 0.20
#1    5 0.25  9 0.45
#2    7 0.35 16 0.80
#3    3 0.15 19 0.95
#5    1 0.05 20 1.00
#Sum 20 1.00 NA   NA

#O gráfico adequado para frequências absolutas, relativas, ou acumuladas de uma 
#variável discreta é parecido com um gráfico de barras, mas nesse caso, as 
#frequências são indicadas por linhas.
plot(filhos.tb, xlab = "Número de filhos",
     ylab = "Frequência absoluta")

#Outra possibilidade seria fazer gráficos de frequências relativa e de frequências 
#relativas acumuladas conforme mostrado nas figuras 
par(mfrow = c(1,2))
## Frequência relativa
plot(prop.table(filhos.tb), xlab = "Número de filhos",
     ylab = "Frequência relativa")
## Frequência relativa acumulada
plot(filhos.tba/20, type = "S", # tipo step (escada)
     xlab = "Número de filhos",
     ylab = "Frequência acumulada relativa")
par(mfrow = c(1,1))

#Variável quantitativa contínua
# Concluindo os exemplos para análise univariada vamos considerar a variável 
# quantitativa contínua Salario
# Para facilitar o cálculo da amplitude de classe e posteriormente para montar a 
# tabela de distribuição de frequência, podemos ordenar o vetor de dados brutos. 
# Por exemplo, os valores ordenados (em ordem crescente) de Salario são
sort(milsa$Salario)
#[1]  4.00  4.56  5.25  5.73  6.26  6.66  6.86  7.39  7.44  7.59  8.12  8.46  8.74
#[14]  8.95  9.13  9.35  9.77  9.80 10.53 10.76 11.06 11.59 12.00 12.79 13.23 13.60
#[27] 13.85 14.69 14.71 15.99 16.22 16.61 17.26 18.75 19.40 23.30
#Com isso, naturalmente já visualizamos os valores máximo e mínimo, e sabemos que a amplitude total é
AT <- max(milsa$Salario) - min(milsa$Salario)
AT
#[1] 19.3
#Como temos 36 observações, o número estimado de classes é
k = sqrt(nrow(milsa))
k
#[1] 6
#Portanto, a amplitude de classe é
h = AT / k
h
#[1] 3.216667
#O valor de amplitude de classe pode ser arredondado para um número inteiro, 
#geralmente para facilitar a interpretação da tabela. Nesse caso, poderíamos 
#arredondar a amplitude para 3 ou para 4 (inteiros mais próximos). Como o valor 
#mínimo é 4 e o máximo está próximo de 24, é natural arredondarmos a amplitude para 4. 
#Após esse arredondamento, vamos denominar a amplitude de classe definitiva por Δ, 
#apenas para diferenciar do valor calculado em h.
#Dessa forma, as classes seriam formadas pela sequência, ou “quebra de classes” 
#definida por
(quebra <- seq(4, 24, 4))
#[1]  4  8 12 16 20 24
#Portanto, ficamos com um total de 5 classes. A última etapa é definir os extremos 
#de classe que serão abertos (permitem incluir aquele valor exato) ou fechados (não incluem aquele valor). Para isso, usaremos a notação a seguir
#Classe	Notação	Denominação	Resultado
#[a,b)	a⊢b	Fechado em a, aberto em b	Inclui a, não inclui b
#(a,b]	a⊣b	Aberto em a, fechado em b	Não inclui a, inclui b
#Nesse exemplo, vamos usar intervalos do tipo fechados à esquerda (abertos à direita),
#pois o primeiro valor de quebra de classe é o igual ao valor mínimo do conjunto 
#de dados, e dessa forma garantimos que todos os valores pertençam a alguma das classes.
#Portanto, as classes completas com suas respectivas frequências absolutas estão na tabela abaixo.
classes <- cut(milsa$Salario, breaks = quebra, right = FALSE)
classes.tab <- table(classes)
cbind("f" = addmargins(classes.tab))
#         f
#[4,8)   10
#[8,12)  12
#[12,16)  8
#[16,20)  5
#[20,24)  1
#Sum     36

#vamos mostrar três possíveis gráficos para variáveis contínuas: o de dispersão, 
#o histograma, e o de ramo-e-folhas.
#O gráfico de dispersão unidimensional consiste da variável de interesse no eixo Y, 
#plotada com seus respectivos índices (entrada) da tabela de dados. 
#Nesse exemplo, o gráfico seria:
plot(milsa$Salario, xlab = "Índice", ylab = "Salário")

#Note que, como os dados nesta tabela estão ordenados (em ordem crescente) pelos 
#salários, então vemos esse padrão de cresimento. Mas a ordenação não é um pré-requisito 
#para fazer esse gráfico. A interpretação desse gráfico é limitada pelo fato de 
#que só faz sentido dizer alguma coisa sobre os salários se os índices 
#(nesse caso, os funcionários) tivessem algum tipo de identificação. 

#Por esse motivo, o histograma é uma forma mais eficiente de resumir variáveis contínuas.
#O histograma é um gráfico de barras contíguas, com as bases proporcionais aos 
#intervalos de classe, e a área de cada retângulo proporcional à respectiva frequência.

#Pode-se usar tanto a frequêcia absoluta (fi) quanto a relativa (fri). Indicamos 
#a amplitude do i-ésimo intervalo por Δi. Para que a área do retângulo respectivo 
#seja proporcional a fri, a sua altura deve ser proporcional a fri/Δi, que é 
#chamada densidade de frequência, ou simplesmente densidade da i-ésima classe. 
#Quando todos os intervalos de classe forem iguais a Δ (como é o caso nesse exemplo), 
#então a densidade de frequência simplifica para fri/Δ. Com essa convenção, garantimos 
#que a área total do histograma será igual a um, uma propriedade que será importante 
#quando estudarmos probabilidade.
#Abaixo, vemos os histogramas com as frequências absolutas, e com a densidade de frequência.
par(mfrow = c(1,2))
hist(milsa$Salario, breaks = quebra, right = FALSE, main = "",
     xlab = "Classes", ylab = "Frequência absoluta",
     xlim = c(4, 24), labels = TRUE, axes = FALSE)
axis(1, at = quebra, labels = quebra); axis(2)
h <- hist(milsa$Salario, breaks = quebra, right = FALSE, main = "",
          xlab = "Classes", ylab = "Densidade",
          xlim = c(4, 24), ylim = c(0, .09), freq = FALSE, axes = FALSE)
axis(1, at = quebra, labels = quebra); axis(2)
text(h$mids, h$density, labels = round(h$density * 4, 2), pos = 3)
par(mfrow = c(1,1))

#Note que, para construir o gráfico com as densidades, precisamos então de mais
#uma coluna na tabela, que seria a densidade fri/Δ=fri/4.
cbind("f" = addmargins(classes.tab),
      "fr" = addmargins(prop.table(classes.tab)),
      "F" = c(cumsum(classes.tab), NA),
      "Fr" = c(cumsum(classes.tab), NA)/36,
      "Dens" = c(prop.table(classes.tab)/4, NA))
#         f         fr  F        Fr        Dens
#[4,8)   10 0.27777778 10 0.2777778 0.069444444
#[8,12)  12 0.33333333 22 0.6111111 0.083333333
#[12,16)  8 0.22222222 30 0.8333333 0.055555556
#[16,20)  5 0.13888889 35 0.9722222 0.034722222
#[20,24)  1 0.02777778 36 1.0000000 0.006944444
#Sum     36 1.00000000 NA        NA          NA

#O que devemos observar em um histograma:
#Amplitude de valores
#Forma da distribuição (assimetria):
#  Positiva
#  Negativa
#  Simétrica
#Tendência central
#Valores extremos
#Tanto o histograma quanto os gráficos de barra, dão uma ideia da forma da 
#distribuição de uma variável

#Outro método para se resumir uma variável com o objetivo de obter uma ideia da 
#forma de sua distribuição, é através do “gráfico” de ramo-e-folhas.
#a ideia básica é dividir cada observação em duas partes: a primeira (o ramo) é 
#colocada no lado esquerdo de uma linha vertical, e a segunda (a folha) é colocada 
#à direita. Geralmente, a divisão natural é separar um número contínuo em sua 
#parte inteira à esquerda, e a parte decimal à direita. 
#Dessa forma o ramo-e-folha para a variável Salario está abaixo.
stem(milsa$Salario, scale = 2)
#The decimal point is at the |

#4 | 06
#5 | 37
#6 | 379
#7 | 446
#8 | 157
#9 | 01488
#10 | 58
#11 | 16
#12 | 08
#13 | 269
#14 | 77
#15 | 
#16 | 026
#17 | 3
#18 | 8
#19 | 4
#20 | 
#21 | 
#22 | 
#23 | 3

#Algumas informações que podemos obter a partir do ramo-e-folhas:
# - Há um destaque para o valor 23,3.
# - Os demais valores se concentram mais fortemente entre 4,0 e 14,7.
# - Um valor mais ou menos típico para essa variável seria em torno de 9.
# - Há uma leve assimetria em direção aos valores grandes.
