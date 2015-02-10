# this function takes three levels of percentages and creates a random sample
expLevels <- function(l1, l2, l3, N){
  # all levels in one vector
  exp.levels <- c(rep(1, l1/100*N), rep(2, l2/100*N), rep(3, l3/100*N))
  
  # if sum of all levels is different than a hundred then recycle to avoid error
  if(sum(l1, l2, l3) < 100 | sum(l1, l2, l3) > 100){ 
    exp.levels <- sample(exp.levels, size = N, replace = TRUE)
  }
  else exp.levels <- sample(exp.levels)
  return(exp.levels)
}

interchange2V <- function(v1, v2){
  as.vector(rbind(v1, v2))
}

N <- 20

value <- vector(mode = 'numeric', length = N/2)
value2 <- vector(mode = 'numeric', length = N/2)
transProbMatrix           <- read.csv("disease_transmission_matrix_2.csv", header = FALSE)

population <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
population <- sample(population)
n1 <- population[1:(length(population)/2)]
n2 <- population[(length(population)/2+1):length(population)]

# exp.levels <- c(rep(1,3), rep(2, 3), rep(3,4))
exp.levels <- expLevels(20, 20, 60, (N/2))

for(i in 1:(N/2)){
  value[i] <- simDiseaseTrans(recipient = n1[i], exposer =  n2[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
  value2[i] <- simDiseaseTrans(recipient = n2[i], exposer =  n1[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
}

DT<- data.table("recipient" = interchange2V(n1, n2), "exposer" = interchange2V(n2, n1), 
                 "exposure" = interchange2V(exp.levels, exp.levels), "recipient.new" = interchange2V(value, value2))
