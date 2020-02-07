#### Set working directory
setwd("C:/Users/Administrator/Desktop/nonparametric project/mycode")

#### Load the necessary packages
library(MASS)
library(pstest)
library(foreach)
library(parallel)
library(snow)
library(doSNOW)
library(doRNG)

#### Set basic parameters of the simulation 
nrep = 1000        # Number of replications
nboot = 999        # Numbe of boostraps draws
ncores = 10         # Number of cores to use in parallel
seed0 = 1234        #Set the initial seed

#### Connect to the files with the Auxiliary R-functions
# File to compute Shaikh et al. (2009, JoE) test
source("C:/Users/Administrator/Desktop/nonparametric project/mycode/shaikh.R")
# File to generate data
source("C:/Users/Administrator/Desktop/nonparametric project/mycode/dgp.R")

for (i in 1:6){
  for (set in 1:10){ 
    #set sample size
    if(i==1) {n <- 100}
    if(i==2) {n <- 200}
    if(i==3) {n <- 400}
    if(i==4) {n <- 600}
    if(i==5) {n <- 800}
    if(i==6) {n <- 1000}
    #do the Monte Carlo without trimming
    source("C:/Users/Administrator/Desktop/nonparametric project/mycode/simulation.R")
    
    #Export output
    write.table(out.p,file="result.txt",append = T)
  }
}
################################################################
# Save simulation results as RData
save.image("simulation-results.RData")


