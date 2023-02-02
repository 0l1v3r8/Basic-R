############################Q1###########################################
#Q1a
q1a = function(x)
  {
  1/(2*(3+x)^2) - (2/(1+(1/(4*x))-5))*x
  }

#Q1b
q1b=  function(x)
  {
  log(x+5^(2*(x+1))) + 18*x*exp(-x)
}

#Q1c
q1c = function(x)
{
  cos(x + (4*x*(1/(x+7)))) +32
}

#Q1d
q1d = function(x)
{
  log(x/(1-x))
}

#Q1e
q1e = function(x)
{
  (exp(-x)*(x^x))/factorial(x)
}

#Q1f
q1f = function(t,theta)
{
  exp(-(t^(1/theta)))
}

#Q1g
q1g = function(t,theta)
{
  (1+t)^(-1/theta)
}

#Q1h
q1h = function(t,theta)
{
  1 - (1-(exp(-t)))^(1/theta)
}

#Q1i
q1i = function(t,theta)
{
  -log(1-((1-exp(-theta))*exp(-t)))
}

#Q1j
q1j = function(x,y,theta)
{
  exp(-((x^theta)+(y^theta))^(1/theta))
}

#Q1k
q1k = function(x,y,theta)
{
  ((x^(-theta))+(y^(-theta))-1)^(-1/theta)
}

#Q1l
q1l = function(x,y,theta)
{
  1-((x^theta)+(y^theta)-((x^theta)*(y^theta)))^(1/theta)
}

#Q1m
q1m = function(x,y,theta)
{
  (-1/theta)*log(1+(((exp(-theta*x)-1)*((exp(-theta*y)-1)))/((exp(-theta)-1))))
}

#Q1n
q1n = function(x,y,theta)
{
  x*y*exp(((x^-theta)+(y^-theta))^(-1/theta))
}

#Q1o
q1o = function(x,y,theta)
{
  (theta*(cos(x)^(theta+1)))*(sin((theta+1)*(y+(theta*(pi/2)))))
}
############################Q2#######################################
#Q2a
q2a = function(x)
{
  (12*(x>0))+(6*(x<=0))
}

#Q2b
q2b = function(x)
{
  (3*(x==5))+(4*(x>5))+(6*(x<5))
}

#Q2c
q2c = function(x)
{
  (5*((0<x)&(x<1))) + (-12*((x<=0)|(x>=1)))
}

############################Q3#######################################
#Q3a returns TRUE if passed and FALSE if failed
q3a = function(results)
{
  passed = c(results >= 50)
  failed = c(results < 50) 
  overall = length(failed[failed==TRUE])
  print(overall)
  !(overall>0)
}

#Q3a returns TRUE if passed and FALSE if failed
q3a = function(w,x,y,z)
{
  results = c(w,x,y,z)
  passed = c(results >= 50)
  failed = c(results < 50) 
  overall = length(failed[failed==TRUE])
  !(overall>0)
}

#Q3b returns TRUE if passed and FALSE if failed
q3b = function(w,x,y,z)
{
  results = c(w,x,y,z)
  passed = c(results >= 50)
  failed = c(results < 50) 
  overall = length(failed[failed==TRUE])
  avg_grade = sum(results)/length(results)
  print(avg_grade)
  !(overall>0)|!(overall>1)&!(avg_grade>60)
}

#Q3c returns TRUE if passed and FALSE if failed
q3c = function(w,x,y,z)
{
  results = c(w,x,y,z)
  passed = c(results >= 50)
  failed = c(results < 50) 
  overall = length(failed[failed==TRUE])
  avg_grade = sum(results)/length(results)
  print(avg_grade)
  ((overall==0)|((overall==1)&(avg_grade>60))|((overall==2)&(avg_grade>70)))
}