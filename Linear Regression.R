linear = function(x,y,lr,ite)
{
  theta0 = 0
  
  theta1 = 0
  
  m=length(x)
  
  J = vector(mode = "numeric" , length = m )
  
  hypo = hypothesis(x , theta0 , theta1)
  
  costt = cost(hypo , y , m)
  
  prev_cost = 1000000
  
 for (i in 1:ite) {    
  
    #prev_cost = costt
   
    theta0 = theta0 - ( lr/m ) * sum( hypo - y )
    
    theta1 = theta1 - ( lr/m ) * sum( (hypo - y)*x )
    
    hypo = hypothesis(x , theta0 , theta1)
  
    New_cost = cost(hypo , y , m)
  
    J[i] = New_cost


 }
  
  theta = c(theta0 , theta1)
  plot(J)
  return( theta )
}  


cost = function(hypo , y, m)
{
  
  interm = (hypo - y)^2
  
  interm1 = sum(interm)
  
  interm2 = interm1/(2 * m)

  return(interm2)
  
}


hypothesis = function(x , theta0 , theta1)
{
  
  X= theta1*x
  
  hypo = theta0 +X
  
  return(hypo)
  
}
