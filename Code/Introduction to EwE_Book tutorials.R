#Introduction to EwE - book tutorials

#R-code of Lotka Volterra model 

work.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(work.dir)

#———————–Definition of variables————————————————
dt = 0.2 # time per timestep
timesteps = 1000 # number of time steps
time = seq(1,timesteps) * dt # time for plotting
Nt = rep(0,timesteps) # number of prey per timestep
Vt = rep(0,timesteps) # number of vulnerable prey per timestep
dNdt = rep(0,timesteps) # change in prey numbers per timestep
prey.birth = rep(0,timesteps) # prey births per timestep
prey.death = rep(0,timesteps) # prey deaths per timestep
Pt = rep(0,timesteps) # number of predators per timestep
dPdt = rep(0,timesteps) # change in predator numbers per timestep
prey.eaten = rep(0,timesteps) # prey eaten per timestep
prey.forag = rep(0,timesteps) # prey eaten per timestep (foraging arena)
pred.birth = rep(0,timesteps) # predator births per timestep
pred.death = rep(0,timesteps) # predator deaths per timestep
#for foraging arena
vone = 100 # movement to vul state
vtwo = 100 # movement to safety
br = 0.4 # prey birth rate
mr = 0.2 # prey death rate
Rmax =0.5 # prey max recruits
ar = 10 # rate of effective search
md = 0.2 # predator death rate
eff = 0.3 # predator food conversion efficiency
ht = 2 # handling time

#———————–Definition of variables————————————————
for(i in 1:timesteps) {
  if(i==1) {
    Nt[i] = 0.17 # initial prey number
    Pt[i] = 0.07 # initial predator number
  } else if(i==2) {
    Nt[i] = Nt[i-1] + dNdt[i-1] * dt
    Pt[i] = Pt[i-1] + dPdt[i-1] * dt
  } else {
    Nt[i] = Nt[i-1] + (3*dNdt[i-1]-dNdt[i-2]) / 2 * dt
    Pt[i] = Pt[i-1] + (3*dPdt[i-1]-dPdt[i-2]) / 2 * dt
  }
  Vt[i] = vone * Nt[i] / (vone + vtwo + ar * Pt[i])
  # prey calculations
  prey.death[i] = Nt[i] * mr # initial prey deaths
  prey.eaten[i] = ar * Nt[i] * Pt[i] / (1 + ht * Nt[i])
  prey.forag[i] = ar * Vt[i] * Pt[i] / (1 + ht * Vt[i])
  prey.birth[i] = Nt[i] * br / (1 + br * Nt[i]/Rmax) #Beverton-Holt recruitment
  dNdt[i] = prey.birth[i] - prey.eaten[i] - prey.death[i]
  # predator calculations
  pred.death[i] = Pt[i] * md
  pred.birth[i] = prey.eaten[i] * eff
  dPdt[i] = pred.birth[i] - pred.death[i]
}
#Set a plotting window with one column and two rows.
par(mfrow=c(2,1))
plot(time, Nt,type="l", xlab="Time", ylab = "Numbers",lty=1,lwd=3,col="red", main="Mass action model")
lines(time, Pt, xlab="Time", ylab = "Numbers",lty=1,lwd=3,col="blue")
legend("topright", legend=c("Prey", "Predator"),col=c("red", "blue"), lty=1, cex=1)
plot(Nt, Pt,type="l", xlab="Prey numbers", ylab = "Predator numbers", main="State space",
     lty=1,lwd=1.5,col="darkgreen")

#R-code of Lotka-Volterra model with foraging arena calculations
#============= Set up directory links and folders =============================
work.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(work.dir)
#———————–Definition of variables————————————————
dt = 0.2 # time per timestep
timesteps = 1000 # number of time steps
time = seq(1,timesteps) * dt # time for plotting
# masss action
Nt = rep(0,timesteps) # number of prey per timestep
dNdt = rep(0,timesteps) # change in prey numbers per timestep
Pt = rep(0,timesteps) # number of predators per timestep
dPdt = rep(0,timesteps) # change in predator numbers per timestep
#foraging arena
Nv = rep(0,timesteps) # number of prey per timestep with foraging arena
Vt = rep(0,timesteps) # number of vulnerable prey per timestep
dVdt = rep(0,timesteps) # change in vulnerable prey numbers per timestep
Qt = rep(0,timesteps) # number of predators per timestep with foraging arena
dQdt = rep(0,timesteps) # change in predator numbers per timestep with foraging arena
br = 0.4 # prey birth rate
mr = 0.2 # prey death rate
Rmax =0.5 # prey max recruits
ar = 10 # rate of effective search
md = 0.2 # predator death rate
eff = 0.3 # predator food conversion efficiency
ht = 2 # handling time
#for foraging arena
vone = 100 # movement to vul state
vtwo = 100 # movement to safety
#———————–Definition of variables————————————————
for(i in 1:timesteps) {
  if(i==1) {
    # mass action
    Pt[i] = 0.07 # initial predator number
    Nt[i] = 0.17 # initial prey number
    # foraging arena
    Qt[i] = Pt[i] # initial predator number with foraging
    Nv[i] = Nt[i] # initial preyr number with foraging
  } else if(i==2) {
    # mass action
    Pt[i] = Pt[i-1] + dPdt[i-1] * dt
    Nt[i] = Nt[i-1] + dNdt[i-1] * dt
    # foraging arena
    Qt[i] = Qt[i-1] + dQdt[i-1] * dt
    Nv[i] = Nv[i-1] + dVdt[i-1] * dt
  } else {
    # mass action
    Pt[i] = Pt[i-1] + (3*dPdt[i-1]-dPdt[i-2]) / 2 * dt # Second-Order Adams-Bashforth Method
    Nt[i] = Nt[i-1] + (3*dNdt[i-1]-dNdt[i-2]) / 2 * dt # yi+1 = yi + h2 [3f(xi , yi)− f(xi−1 , yi−1)]
    # foraging arena
    Qt[i] = Qt[i-1] + (3*dQdt[i-1]-dQdt[i-2]) / 2 * dt
    Nv[i] = Nv[i-1] + (3*dVdt[i-1]-dVdt[i-2]) / 2 * dt
  }
  Vt[i] = vone * Nv[i] / (vone + vtwo + ar * Qt[i])
  # prey calculations
  prey.death = Nt[i] * mr # initial prey deaths
  v.death = Nv[i] * mr
  prey.eaten = ar * Nt[i] * Pt[i] / (1 + ht * Nt[i]) # Hollings disk equation
  v.eaten = ar * Vt[i] * Qt[i] / (1 + ht * Vt[i]) # Hollings disk equation (foraging)
  prey.birth = Nt[i] * br / (1 + br * Nt[i]/Rmax) # Beverton-Holt recruitment
  v.birth = Nv[i] * br / (1 + br * Nv[i]/Rmax) # Beverton-Holt recruitment (foraging)
  dNdt[i] = prey.birth - prey.eaten - prey.death
  dVdt[i] = v.birth - v.eaten - v.death
  # predator calculations
  pred.death = Pt[i] * md
  pred.birth = prey.eaten * eff
  dPdt[i] = pred.birth - pred.death
  #foraging predator
  q.death = Qt[i] * md
  q.birth = v.eaten * eff
  dQdt[i] = q.birth - q.death
}
#Set a plotting window with one column and two rows.
par(mfrow=c(2,2))
plot(time, Nt,type="l", xlab="Time", ylab = "Numbers",lty=1,lwd=3,col="red", main="Mass action model")
lines(time, Pt, xlab="Time", ylab = "Numbers",lty=1,lwd=3,col="blue")
legend("topright", legend=c("Prey", "Predator"),col=c("red", "blue"), lty=1, cex=1)
plot(Nt, Pt,type= "l", xlab="Prey numbers", ylab = "Predator numbers", main="Phase space (mass action)",
     lty=1,lwd=1.5,col="darkgreen")
plot(time, Nv,type="l", xlab="Time", ylab = "Numbers",ylim=c(0,max(Nv)),lty=1,lwd=3,col="red", main="Foraging arena model")
lines(time, Qt, xlab="Time", ylab = "Numbers",lty=1,lwd=3,col="blue")
legend("topright", legend=c("Prey", "Predator"),col=c("red", "blue"), lty=1, cex=1)
plot(Nv, Qt,type="l", xlab="Prey numbers", ylab = "Predator numbers", main="Phase space (foraging arena)",
     lty=1,lwd=1.5,col="darkgreen")


