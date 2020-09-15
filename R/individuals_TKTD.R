###############
### PRIVATE ###
###############

# emily : modele ecotoxico simple : ashauer et al 2007
# cint : concentration interne à suivre dans le temps + seuil de mortalité à fixer sur cette concentration
# cmil : concentration dans le milieu (vecteur de 30 valeurs pour 30 jours)
# cint_start : concentration interne au debut
# kin : constante d'absorption ( 25% du pollen d'un pixel est absorbé par jour par larve)
# kout : constante d'élimination (50% du pollen est éliminé d'un jour au suivant)
# min.time temps de début
# max.time temps de fin
# deltat
conc.int = function(cmil,
                    cint_start = 0,
                    kin = 0.25,
                    kout = 0.5,
                    min.time = 1,
                    max.time,
                    deltat = 0.01) {
  if (min.time > max.time) {
    stop("ERROR : ecoToxic time value error")
  }
  cint = numeric(0)
  cint_previous = cint_start
  for (t in min.time:max.time) {
    cmilieut <- cmil[t]
    cint_temp <- cint_previous
    j <- 1
    indt <- deltat * kin * cmilieut
    outdt <- 1 + deltat * kout
    for (dt in seq(0, 0.99, deltat)) {
      # max.time+1
      # attention vecteur indices=entiers
      cint_temp[j + 1] <- (cint_temp[j] + indt) / (outdt)
      j = j + 1
    }
    cint[t] <- cint_temp[length(cint_temp)]
    cint_previous <- cint[t]
  }
  return(cint[min.time:max.time])
}

