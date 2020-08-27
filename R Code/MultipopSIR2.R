Output <- 10
Capital <- 2.5
Labour <- 2

generator <- function(steps,s, n, k){
  for(i in 2:(steps)){
    Capital[i] <- (1-s)*Output[i-1]+Capital[i-1]
    Labour[i] <- n*Labour[i-1]
    Output[i] <- Capital[i]*Labour[i]*k
  }
  return(data.frame(Output = Output, Capital = Capital, Labour = Labour))
}

print(generator(steps=10, s=.01, n=1.002, k=1.01))

# Setting parameters
RecoveryRate <- 0.1
h <- 0
N <- 10
R0 <- c(1.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,3.0,5.0)

