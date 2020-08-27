Infected=c()
num_sim=2000
Fraction=c()

for (n in 2:20) {
exp_vec=c()
infect_dur_vec=c()
for (g in 1:num_sim) {
  #   print(paste("Simulation",i))    #
  R0f=5;gamma_E=1/R0f;n=n;gamma_I=1/n;delta=1.5;p=0.7;h=0.5
  t=rand_time()
  infect=rand_inf()
  # Initialize State and Time Vectors
  State=rep("S",n)
  Time=rep(0,n)
  Infect_Duration=rep(0,n)
  # Run algorithm
  algo(t,infect,R0f,gamma_E,gamma_I,n,delta,p,h)
  # Assign to vectors/lists for graphs
  exp_vec=append(exp_vec,exp)
  infect_dur_vec=append(infect_dur_vec,Infect_Duration)
  
  if (g==num_sim) {
    # Getting the mean fraction of people E or above
    frac<<-(mean(exp_vec)/n)
    # Getting the mean time people are infectious in each group for 
    mean_infect_total_sim=colMeans(matrix(infect_dur_vec,n))
    mean_infect<-mean(mean_infect_total_sim)
    assign('frac',frac,envir = .GlobalEnv)
    assign('mean_infect',mean_infect,envir = .GlobalEnv)
    
  }
  
}
Infected=append(Infected,mean_infect)
}




