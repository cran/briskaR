############ kernel for pollen dispersal

# --- Implementation in R

# See Angevin et al. 2008 Modelling impact of cropping systems and
# climate on maize cross-pollination in agricultural landscapes: The MAPOD model
NIG <-
  function(x,
           y,
           kernel.options = list(
             "a1" = 0.2073 ,
             "a2" = 0.2073 ,
             "b1" = 0.3971 ,
             "b2" = 0.3971 ,
             "b3" = 0.0649,
             "theta" = 0
           )) {
    if (is.null(kernel.options$theta)) {
      theta = 0
    }
    else {
      theta = kernel.options$theta
    }
    lambda_z = kernel.options$b3                 #=0.027*h/0.831 avec h=2
    lambda_x = kernel.options$b1 * cos(theta)    #0.165*h*mu*cos(theta)/(2*0.831) # avec h=mu=2
    lambda_y = kernel.options$b2 * sin(theta)    #0.165*h*mu*sin(theta)/(2*0.831)  avec h=mu=2
    delta_x = kernel.options$a1                  #0.499*0.831/h
    delta_y = kernel.options$a2
    
    p = lambda_z^2 + lambda_x^2 + lambda_y^2
    q = 1 + delta_x^2 * x^2 + delta_y^2 * y^2
    A = delta_x * delta_y * exp(lambda_z) / (2 * pi)
    B = (q^-0.5 + p^0.5) / q
    C = exp(-sqrt(p * q)) * exp(lambda_x * delta_x * x + delta_y * lambda_y * y)
    
    return(A * B * C)
  }

#2Dt Student kernel

student <-
  function(x,
           y,
           kernel.options = list(
             "c1" = 1.12,
             "a" = 1.55,
             "b" = 1.45,
             "c2" = 0,
             "theta" = 0
           )) {
    a = kernel.options$a
    b = kernel.options$b
    c2 = kernel.options$c2
    c1 = kernel.options$c1
    theta = kernel.options$theta
    A = (b - 1) / (pi * a * a)
    B = (1 + (x * x + y * y) / (a * a)) ^ (-b)
    C = exp(c1 * cos(theta - c2))
    return(A * B * C)
  }

# Geometric kernel Soubeyrand

geometric <- function(x, y, kernel.options = list("a" = -2.59))   {
  beta = kernel.options$a
  A = (1 + sqrt(x * x + y * y)) ^ beta
  B = (beta + 1) * (beta + 2) / (2 * pi)
  return(A * B)
}


#### FatTail
FatTail <- function(x, y) {
  return(sqrt(x^2 + y^2)^-2)
}

#-------------------------------------
#Hoffman 2014, heavy tail power dispersal kernel
kernel_fat_tail <-  function(x, y, kernel.options = list("a" = 1.271e6, "b" = -0.585)) {
    a = kernel.options$a
    b = kernel.options$b
    res <- a * sqrt(x * x + y * y) ^ b
    return(res)
  }

# gaussian
gaussian <- function(x, y, kernel.options = list("a" = 1)){
  a = kernel.options$a
  r = sqrt(x^2 + y^2)
  res <- 1/ (pi*a^2) * exp(-r^2/a^2)
  return(res)
}

# negative exponential
negExponential <- function(x, y, kernel.options = list("a" = 1)){
  a = kernel.options$a
  r = sqrt(x^2 + y^2)
  res <- 1/ (2*pi*a^2) * exp(-r/a)
  return(res)
}

# bivariate student
k2Dt <- function(x, y, kernel.options = list("a" = 1, "b" = 1)){
  a = kernel.options$a
  b = kernel.options$b
  r = sqrt(x^2 + y^2)
  res <- (b-1)/ (pi*a^2) * (1+r^2/a^2)^(-b)
  return(res)
}

# Inverse power law
invPowerLaw <- function(x, y, kernel.options = list("a" = 1, "b" = 1)){
  a = kernel.options$a
  b = kernel.options$b
  r = sqrt(x^2 + y^2)
  res <- (b-1) * (b-2)/ (2*pi*a^2) * (1+r/a)^(-b)
  return(res)
}

# Inverse power law undifined
invPowerLawUndif <- function(x, y, kernel.options = list("a" = 1, "b" = 1)){
  a = kernel.options$a
  b = kernel.options$b
  r = sqrt(x^2 + y^2)
  res <- (r/a)^(-b)
  return(res)
}

# weibull
weibull <- function(x, y, kernel.options = list("a" = 1, "b" = 1)){
  a = kernel.options$a
  b = kernel.options$b
  r = sqrt(x^2 + y^2)
  res <- b / (2*pi*a^2) * r^(b-2) * exp(-r^b/a^b)
  return(res)
}

# loch-sech
lochSech <- function(x, y, kernel.options = list("a" = 1, "b" = 1)){
  a = kernel.options$a
  b = kernel.options$b
  r = sqrt(x^2 + y^2)
  res <- 1 / (pi^2 * b * r^2) * 1 / ((r/a)^(1/b) + (r/a)^(-1/b))
  return(res)
}


# -----------------------------------------------------------------------------
# --------- IMPLEMENTATION IN C++ ---------------------------------------------
# -----------------------------------------------------------------------------
#
#
# 
# @export
NIG_Cpp <- function(x, y,
                    kernel.options = list(
                      "a1" = 0.2073 ,
                      "a2" = 0.2073 ,
                      "b1" = 0.3971 ,
                      "b2" = 0.3971 ,
                      "b3" = 0.0649,
                      "theta" = 0)){
  if (is.null(kernel.options$theta)) {
    theta = 0
  } else {
    theta = kernel.options$theta
  }
  NIG_return <- nigCpp(x, y,
                       kernel.options$a1,
                       kernel.options$a2,
                       kernel.options$b1,
                       kernel.options$b2,
                       kernel.options$b3,
                       theta, pi)
  
  return(NIG_return)
}

#
# @export
geometric_Cpp <- function(x, y,
                          kernel.options = list(
                            "a" = -2.59 )){
  
  geometric_return <- geometricCpp(x, y, kernel.options$a, pi)
  return(geometric_return)
}

#
# @export
# FatTail_Cpp <- function(x, y){
#   return(FatTailCpp(x, y, 1, -2))
# }


# Hoffman 2014, heavy tail power dispersal kernel
#
# @export
kernel_fat_tail_Cpp <- function(x, y,
                                kernel.options = list("a" = 1.271e6, "b" = -0.58)) {
    return(FatTailCpp(x, y, kernel.options$a, kernel.options$b))
    # return(FatTailCpp(x, y))
}

#
# @export
student_Cpp <-
  function(x,
           y,
           kernel.options = list(
             "c1" = 1.12,
             "a" = 1.55,
             "b" = 1.45,
             "c2" = 0,
             "theta" = 0
           )) {
    
    student_return <- studentCpp(x, y,
                                 kernel.options$a,
                                 kernel.options$b,
                                 kernel.options$c1,
                                 kernel.options$c2,
                                 kernel.options$theta,
                                 pi)
    return(student_return)
  }
