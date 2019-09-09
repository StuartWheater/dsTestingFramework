#This local function calculates the arithmetical mean, variance and the length of 
#some local values.  The function returns a vector of length 3; the first element 
#contains the mean, the second the variance and the third the number of elements.
.calc.distribution.locally <- function(some.local.values)
{
  value <- c(0,0,0)
  
  value[1] <- mean(some.local.values)
  value[2] <- var(some.local.values)
  value[3] <- length(some.local.values)
  return(value)
}

#This local function calculates the arithmetical mean, variance and the length of 
#some  values store on a server. The function returns a vector of length 3; the first element 
#contains the mean, the second the variance and the third the number of elements.
.calc.distribution.server <- function(name.variable)
{
  value <- c(0,0)
  mean.from.servers <- ds.mean.o(x=name.variable,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  var.from.servers <- ds.var.o(x=name.variable,type='combine',   datasources=ds.test_env$connection.opal)
  value[1] <- mean.from.servers[[1]][1]
  value[2] <- var.from.servers[[1]][1]
  value[3] <- mean.from.servers[[1]][3]
  return(value)
  
}

#This local function computes the difference between each element of two vectors. Then, it sorts these
#differences in decreasing order. The latter allows to test some remainders in recodeValues, for example. 
#returns a vector of the same length as second.vector.
.calc.differences <- function(first.vector, second.vector)
{
  
  differences <- c()
  for (i in 1:length(second.vector))
  {
    differences[i] <- first.vector[i] - second.vector[i]
  }
  differences <- sort(differences, decreasing = TRUE)
  return(differences)
}

.mult.vectors <- function(first.vector, second.vector)
{
   return(first.vector * second.vector)
}

.add.vectors <- function(first.vector, second.vector)
{
  return(first.vector + second.vector)
}

.divide.vectors <- function(first.vector, second.vector)
{
   result <- first.vector / second.vector
   return(result[!is.na(result)])
}

.substract.vectors <- function(first.vector, second.vector)
{
  return(first.vector - second.vector)
}


