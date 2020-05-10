# INTERVALO DE CONFIANZA PARA PROPORCIONES

# prop conf int u = .4, n = 1600, conf = .99
phat <- .423
n <- 1000
conf <-  .95
#lower bound; upper bound

if(n >= 30) {
  z <- qnorm(conf + (1 - conf) / 2)
} else {
  z <- qt(conf + (1 - conf) / 2, n)
}

data.frame( min = phat - z * sqrt((phat*(1-phat)/n)),
              max = phat + z * sqrt((phat*(1-phat)/n)))

rm(list = ls())

# INTERVALO DE CONFIANZA EN MEDIA

n <- 9
uhat <- 0
desv <- .0365
conf <- .90


if(n >= 30) {
  z <- qnorm(conf + (1 - conf) / 2)
} else {
  z <- qt(conf + (1 - conf) / 2, n)
}

data.frame( min = uhat - z * (desv/sqrt(n)),
            max = uhat + z * (desv/sqrt(n)))

rm(list = ls())


######### HIPOTHESIS TESTING


hypothesis_test_mean <- function(n,sample_u,desv,u0,lower.tail = T, conf, use.Z = F){
  empiric <- (sample_u - u0)/(desv/sqrt(n))
  
  if(lower.tail){
    alpha <- (1-conf)
  } else{
    alpha <- conf 
  } 
  
  if(!use.Z){
    critic <- qt(alpha,df = n-1)
    pvalue <- pt(empiric,n-1,lower.tail = lower.tail)
  } else{
    critic <- qnorm(alpha)
    pvalue <- pnorm(empiric,lower.tail = lower.tail)
  }
  
  return(data.frame(empiric = empiric,
                    critic = critic,
                    pvalue = pvalue))
  
}

hypothesis_test_mean(n = 20,sample_u = 1.7,desv = .9,u0 = 2,lower.tail = T,conf = .95, use.Z = T)

rm(list = ls())

#####2 - related group means MATCHED SAMPLES
#corr = Sxy/(Sx * Sy)
#var = varx + vary - 2*covxy

Sxy <- .1900109
n <- 30
ux <- 9.45
uy <- 9.36
varx <- .225
vary <- .212
conf <-  .95


var <- varx + vary - 2*Sxy
contrast <-(u0-u1)/(sqrt(var)/sqrt(n)) 
crit <- qt(conf + (1-conf)/2, n-1)
pt(contrast,df = n-1,lower.tail = F)

rm(list = ls())

### diferencias en medias de grupos con varianzas poblacionales iguales
n1 <- 75
n2 <- 75
s1 <- 52
s2 <- 70
conf <- .99
x1 <- 512
x2 <- 551

pooled_var <- ((n1-1) * s1^2 + (n2-1) * s2^2)/(n1 + n2 -2)
empiric <- (x1-x2)/sqrt(pooled_var * (1/n1 + 1/n2))
crit <- qt(.005,df = 148) # adaptar


###### POWER TEST


power_mean_test <- function(n, mu, mustar, conf, desv, lower){
  if(n >=30){
    xcrit <-  mu + qnorm(1-conf, lower.tail = lower)*(desv/sqrt(n))
  } else {
    xcrit <- mu + qt(1-conf,n-1, lower.tail = lower)*(desv/sqrt(n))
  }
  
  empiric <- (xcrit - mustar)/(desv/sqrt(n))
  
  if(n>=30){
    beta <- 1-pnorm(empiric, lower.tail = lower)
  } else{
    beta <- 1-pt(empiric, n-1, lower.tail = lower)
  }
  
  potencia <- 1- beta
  
  return(data.frame(beta = beta,
                    potencia = potencia))
  
}

power_mean_test(n = 20,mu = 2,mustar = 1.95,conf = .95,desv = .9,lower = T)



##### Regression ######

# parametros de modelo logit
# exp(beta * x)/
#   (1 + (exp(beta * x))






