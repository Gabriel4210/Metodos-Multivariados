library(tidyverse)

df = read_csv("https://raw.githubusercontent.com/Gabriel4210/Metodos-Multivariados/main/banco.csv", locale = locale(decimal_mark = ','))
set.seed(4210)
df_sample = df |> sample_n(200)
summary(df_sample)

#Mean, sd, cov e cor
c('Média', 'Desvio Padrão') |> 
  bind_cols( df_sample[,-1] |>
    summarise_all( ~ mean(.x, na.rm = T)) |>
    bind_rows(df_sample[,-1] |>
                summarise_all( ~ sd(.x, na.rm = T)))
)

df_sample[,-1] |> cov()
df_sample[,-1] |> cor()

#Agrupando por sexo
df_sample_man = df_sample |> filter(Sexo == 0)
df_sample_woman = df_sample |> filter(Sexo == 1)

#Mean, sd, cor e cov por sexo

#Masculino
c('Média', 'Desvio Padrão') |> 
  bind_cols( df_sample_man[,-c(1,2)] |>
               summarise_all( ~ mean(.x, na.rm = T)) |>
               bind_rows(df_sample_man[,-c(1,2)] |>
                           summarise_all( ~ sd(.x, na.rm = T))))

df_sample_man[,-c(1,2)] |> cov()
df_sample_man[,-c(1,2)] |> cor()

#Feminino
c('Média', 'Desvio Padrão') |> 
  bind_cols( df_sample_woman[,-c(1,2)] |>
               summarise_all( ~ mean(.x, na.rm = T)) |>
               bind_rows(df_sample_woman[,-c(1,2)] |>
                           summarise_all( ~ sd(.x, na.rm = T))))

df_sample_woman[,-c(1,2)] |> cov()
df_sample_woman[,-c(1,2)] |> cor()

#letra b e c
m = c(0.3, 30, 0.48, 0.43, 3000, 2100, 1240, 0.28, 2150)
n = 200
p = 10

teste_mean = colMeans(df_sample[,-1])
teste_cov = cov(df_sample[,-1])

T2 = n*t(teste_mean - m)%*%solve(teste_cov)%*%(teste_mean - m)
qF = qf(p = 0.05, df1 = p, df2 = n - p, lower.tail = F, log.p = F,)
Fcal = (((n-1)*p)/(n-p))*qF

#Hipotese nula rejeitada,as médias são diferentes
#T2 > Fcal

# letra d
dif_mean = colMeans(df_sample_man[,-c(1,2)]) - colMeans(df_sample_woman[,-c(1,2)])

S_man = df_sample_man[,-c(1,2)]     |> cov()
S_woman = df_sample_woman[,-c(1,2)] |> cov()

n1 = nrow(df_sample_man)
n2 = nrow(df_sample_woman)

S_agrupado = ((n1-1)*S_man+(n2-1)*S_woman)/(n1+n2-2)

T2.obs = (n1*n2/(n1+n2))*(dif_mean)%*%solve(S_agrupado)%*%(dif_mean)
p = dim(df_sample_men)[2]
qF = qf(p=0.005, df1=p, df2=n1+n2-p-1, lower.tail = FALSE, log.p = FALSE)
F.cal = (((n1+n2-2)*p)/(n1+n2-p-1))*qF

#novamente T2>Fcal,  rejeitamos a hipótese nula, 
#as médias de homens e mulheres são diferentes entre si.