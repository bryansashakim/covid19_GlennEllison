#### Next Action Time ####
action_time=function() {
  temp_Time=Time
  # if no one is infected (E and below), Susceptible members have infinite (very large) values. If there is an E, only update   their time value
  if (!("I" %in% State)) {
    # Assigning big values for S individuals
    s=which(State=="S")
    temp_Time[s]=1000000
    assign('temp_Time',temp_Time,envir = .GlobalEnv)
    # The rest of the family members can be either "E" or "I" - we update the times for these individuals
    e=which(State=="E")
    for (e in e) {
      temp_Time[e]=temp_Time[e]+rexp(1,(1/gamma_E))
    }
    i=which(State=="I")
    for (i in i) {
      temp_Time[i]=temp_Time[i]+rexp(1,(1/gamma_I))
    }
    if (("E" %in% State) | ("R" %in% State)) {
      if (State[missing_int(s)]=="E") {
        temp_Time[missing_int(s)]=temp_Time[missing_int(s)]+rexp(1,(1/gamma_E)) #The random draw that comes from the                 exponential distribution for transition E to I.
        return(temp_Time[missing_int(s)])
        assign('temp_Time',temp_Time,envir = .GlobalEnv)
      }
      else if (State[missing_int(s)]=="R"){
        temp_Time[missing_int(s)]=temp_Time[missing_int(s)]+rexp(1,(1/gamma_E)) #The random draw that comes from the                    exponential distribution for transition E to I.
        return(temp_Time[missing_int(s)])
        assign('temp_Time',temp_Time,envir = .GlobalEnv)
      }
    }
  }
  else if (("S" %in% State) | ("E" %in% State) | ("I" %in% State)) {
    # Modifying S state individuals
    s=which(State=="S")
    for (s in s) {
      temp_Time[s]=temp_Time[s]+rexp(1,(1/R0f*gamma_I))
    }
    #assign('temp_Time',temp_Time,envir = .GlobalEnv)
    # Modifying E state individuals
    e=which(State=="E")
    for (e in e) {
      temp_Time[e]=temp_Time[e]+rexp(1,(1/gamma_E))
    }
    #assign('temp_Time',temp_Time,envir = .GlobalEnv)
    # Modifying I state individuals
    i=which(State=="I")
    for (i in i) {
      temp_Time[i]=temp_Time[i]+rexp(1,(1/gamma_I))
    }
    assign('temp_Time',temp_Time,envir = .GlobalEnv)
    return(min(temp_Time))
  }
  
}



action_timetest=function() {
  temp_Time=temp_Time
  # if no one is infected (E and below), Susceptible members have infinite (very large) values. If there is an E, only update   their time value
  if (!("I" %in% test)) {
    # Assigning big values for S individuals
    s=which(test=="S")
    temp_Time[s]=10000000
    assign('temp_Time',temp_Time,envir = .GlobalEnv)
    if (test[missing_int(s)]=="E") {
      temp_Time[missing_int(s)]=temp_Time[missing_int(s)]+rexp(1,(1/gamma_E)) #The random draw that comes from the                 exponential distribution for transition E to I.
      assign('temp_Time',temp_Time,envir = .GlobalEnv)
      return(temp_Time[missing_int(s)])
    }
  }
  else if (("S" %in% test) | ("E" %in% test) | ("I" %in% test)) {
    # Modifying S state individuals
    s=which(test=="S")
    for (s in s) {
      temp_Time[s]=temp_Time[s]+rexp(1,(1/R0f*gamma_I))
    }
    #assign('temp_Time',temp_Time,envir = .GlobalEnv)
    # Modifying E state individuals
    e=which(test=="E")
    for (e in e) {
      temp_Time[e]=temp_Time[e]+rexp(1,(1/gamma_E))
    }
    #assign('temp_Time',temp_Time,envir = .GlobalEnv)
    # Modifying I state individuals
    i=which(test=="I")
    for (i in i) {
      temp_Time[i]=temp_Time[i]+rexp(1,(1/gamma_I))
    }
    assign('temp_Time',temp_Time,envir = .GlobalEnv)
    return(min(temp_Time))
  }
  
}



algo2=function(t,infect,R0f,gamma_E,gamma_I,n,delta,p,h) {
  State[infect]="E"
  Time=rep(t,n)
  assign('State',State,envir = .GlobalEnv)
  assign('Time',Time,envir = .GlobalEnv)
  print("CHECK")
  ## Checks if algo should end ##
  for (i in 1:10){
  if (!(("R" %in% unique(State)) & (length(unique(State))==1))) {
    print("CHECKWHILE")
    #### if we do not stop the algo:
    ### run action_time & test_time
    ## if action_time < test_time: then change the value of the smallest temp_Time index to progress the disease. i.e. actually make changes to the values in State and update Time
    if (isTRUE(action_time()<test_time())) {
      print("ACTION<TEST")
      # Updating Time vector
      Time[1:length(Time)]=min(temp_action_Time)
      assign('Time',Time,envir = .GlobalEnv)
      print("ACTION")
      # Updating State vector
      if (State[which(temp_action_Time==min(temp_action_Time))]=="S") {
        State[which(temp_action_Time==min(temp_action_Time))]="E"
        assign('State',State,envir = .GlobalEnv)
        print("ACTION2")
      }
      if (State[which(temp_action_Time==min(temp_action_Time))]=="E") {
        State[which(temp_action_Time==min(temp_action_Time))]="I"
        assign('State',State,envir = .GlobalEnv)
      }
      else { #(State[which(temp_action_Time==min(temp_action_Time))]=="I" 
        State[which(temp_action_Time==min(temp_action_Time))]="R"
        assign('State',State,envir = .GlobalEnv)
      }
    }
    ## else test_time < action_time
    else  {
      print("TEST<ACTION")
      # cycle through which person to test
      if (delta==n) {
        delta=1
        assign('delta',delta,envir = .GlobalEnv)
        print("CHECK1")
      }
      else {
        delta=delta+1
        assign('delta',delta,envir = .GlobalEnv)
        print("CHECK2")
      }
      # Here we test an infected person so commence end process calculations
      if (State[delta]=="I") {
        # end process calculations
        
        break
        
      }
      # If we do not test an infected person
      else  {
        print("TEST")
        # Updating Time vector
        Time[1:length(Time)]=temp_test_Time
        assign('Time',Time,envir = .GlobalEnv)
        # Updating State vector
      }
    }
    print("NO")
  }
  }
  # Did we test someone who is infected? See above for if test_time < action_time
  # Are all elements of the State vector Recovered?
  # -> Begin end process calcs.
  print("DO END PROCESS CALCULATIONS")
}

algo_2=function(t,infect,R0f,gamma_E,gamma_I,n,delta,p,h) {
  State[infect]="E"
  Time=rep(t,n)
  assign('State',State,envir = .GlobalEnv)
  assign('Time',Time,envir = .GlobalEnv)
  print("CHECK")
  ## Checks if algo should end first ##
  while (!(("R" %in% unique(State)) & (length(unique(State))==1))) {
    print("CHECKWHILE")
    #### if we do not stop the algo:
    ### run action_time & test_time
    ## if action_time < test_time: then change the value of the smallest temp_Time index to progress the disease. i.e. actually make changes to the values in State and update Time
    if (isTRUE(action_time()<test_time())) {
      print("ACTION<TEST")
      # Updating Time vector
      Time[1:length(Time)]=min(temp_action_Time)
      assign('Time',Time,envir = .GlobalEnv)
      print("ACTION")
      # Updating State vector
      if (State[which(temp_action_Time==min(temp_action_Time))]=="S") {
        State[which(temp_action_Time==min(temp_action_Time))]="E"
        assign('State',State,envir = .GlobalEnv)
        print("S to E")
      }
      else if (State[which(temp_action_Time==min(temp_action_Time))]=="E") {
        State[which(temp_action_Time==min(temp_action_Time))]="I"
        assign('State',State,envir = .GlobalEnv)
        print("E to I")
      }
      else { #(State[which(temp_action_Time==min(temp_action_Time))]=="I" 
        State[which(temp_action_Time==min(temp_action_Time))]="R"
        assign('State',State,envir = .GlobalEnv)
        print("I to R")
      }
    }
    ## else test_time < action_time
    else  {
      print("TEST<ACTION")
      # Here we test an infected person so commence end process calculations
      if (State[delta]=="I") {
        # end process calculations
        print("Tested an infected person")
        break
        
      }
      # If we do not test an infected person
      else  {
        print("Did not Test and Infected person")
        # Updating Time vector
        Time[1:length(Time)]=temp_test_Time
        assign('Time',Time,envir = .GlobalEnv)
        # Updating State vector
      }
      # cycle through which person to test
      if (delta==n) {
        delta=1
        assign('delta',delta,envir = .GlobalEnv)
        print("CHECK1")
      }
      else {
        delta=delta+1
        assign('delta',delta,envir = .GlobalEnv)
        print("CHECK2")
      }
    }
    print(State)
  }
  # Did we test someone who is infected? See above for if test_time < action_time
  # Are all elements of the State vector Recovered?
  # -> Begin end process calcs.
  print("DO END PROCESS CALCULATIONS")
}


R0f=5;gamma_E=1/5;gamma_I=1/7;n=7;delta=1;p=0.7;h=0.5
t=rand_time()
infect=rand_inf()

# Initialize State and Time Vectors
State=rep("S",n)
Time=rep(0,n)

algo(t,infect,R0f,gamma_E,gamma_I,n,delta,p,h)







