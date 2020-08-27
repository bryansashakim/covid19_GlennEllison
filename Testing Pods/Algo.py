from __future__ import division
import numpy as np
import pandas as pd
from IPython.display import display
from sympy.interactive import printing

## Parameters:
# γEI, γIIs, γIR, γIsR - describe the speed at which transitions from one disease state to the next occur.
# β - the rate at which infectious people infect others.
# p_test - the probability that someone who is positive will test positive.
# d_test - the length of time for test results to come back.
# ∆ - the interval between times when each individual is periodically tested.
# p_contact - the probability that contact tracing of an infected person will identify people who were infected by them.
# μ_contact, σ_contact - mean and variance of the the length of time for tracing of a contact to occur.
# ρSQ - factor by which infection rates are multiplied when someone is in self quarantine.

# Initialize parameters
N=7
R0f=5
γEI
γIIs
γIR
γIsR
β
p_test
d_test
∆
p_contact
μ_contact
ρSQ

# Initialize Matrix A
state=np.empty([N,10],dtype=object)
# For individual 1, first five characteristics are (E, Free, Unidentified, Known, NA)
chr=["E","Free","Unidentified", "Known","NA"]
# Editing first five elements characteristics
for i in range(0,len(state[0])-5):
    state[0,i]=chr[i]

γEI
γIIs
γIR
γIsR
## E->I Transition time random draw
def exp_inf() {
    return(np.random.exponential(1/R0f))
}
susc_exp=function(curr_time){
  return(curr_time+rexp(1,(1/h*R0f*gamma_I)))
}
## E->I Transition time random draw
exp_infect=function(curr_time){
  return(curr_time+rexp(1,(1/gamma_E)))
}
## I->R Transition time random draw
infect_recov=function(curr_time){
  return(curr_time+rexp(1,(1/gamma_I)))
}










