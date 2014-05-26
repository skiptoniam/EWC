library(shiny)
library(deSolve)

# Define parameters
Y12 <- 2.18E8  
Y11 <- 2.51E6  
Y22 <- 2.59E7  
Y21 <- 4.2E6   
rat1 <- Y12/Y11
rat2 <- Y22/Y21
BB <- 4 # input conc BB
MD <- 20 # input conc MD
f <- 0.25
D <- log(1/(1-f), 2.718) #
params <- c(1.6,.8,3.94,.25,1.44,.25,Y11,Y12,Y21,Y22,oS1,oS2,D,rat1,rat2)
t <- c(0:90)
# Define initial conditions
N1 <- 1000000   
N2 <- 1000000
S1 <- c(1:10)
S2 <- c(1:10)
Y <- c(N1,N2,S1,S2)
S1S2<-expand.grid(S1,S2)

comp <- function(t,y,par)
{
  n1 <- y[1] 
  n2 <- y[2] 
  s1 <- y[3] 
  s2 <- y[4]
  
  # Pop for species 1
  if(s1/s2 < rat1) {
    dN1 <- (r1*s1/(K11+s1)-D)*n1     # resource 1 limitation 
  } else {
    dN1 <- (r1*s2/(K12+s2)-D)*n1     # resource 2 limitation 
  }
  # Pop for species 2
  if(s1/s2 < rat2) {
    dN2 <- (r2*s1/(K21+s1)-D)*n2     # resource 1 limitation 
  } else {
    dN2 <- (r2*s2/(K22+s2)-D)*n2     # resource 2 limitation 
  }
  # Conc for resource 1
  dS1 <- D*(oS1-s1) - ((n1*r1*s1/((K11+s1)*Y11))+(n2*r2*s1/((K21+s1)*Y21))) 
  # Conc for resource 2
  dS2 <- D*(oS2-s2) - ((n1*r1*s2/((K12+s2)*Y12))+(n2*r2*s2/((K22+s2)*Y22)))
  
  return(list(as.vector(c(dN1,dN2,dS1,dS2))))
}  

outMatS1S2 <- array(NA,dim=c(91,100,2))
for(i in 1:100){
  Y <- c(N1,N2,S1S2[i,1],S1S2[i,2])
  outMatS1S2[,i,1] <- lsode(y=Y, times=t, func=comp)[,2]
  outMatS1S2[,i,2] <- lsode(y=Y, times=t, func=comp)[,3]
}

shinyServer(function(input, output){
  output$wcPlot <- renderPlot({
    X1 <- input$Aslice
    X2 <- input$Rslice
    plot(c(0:90), log10(outMatS1S2[,as.numeric(rownames(S1S2[S1S2[,1]==X1 & S1S2[,2]==X2,])),1]), type="l", lwd=2, col='orange',yaxt='n', ylim=c(6,7.2), xlab="Minutes", ylab="Match outcome")
    lines(c(0:90), log10(outMatS1S2[,as.numeric(rownames(S1S2[S1S2[,1]==X1 & S1S2[,2]==X2,])),2]), lwd=2, col="dark red")
    legend("topleft", bty="n", legend=c("Nederlands", "Spain"), text.col=c("orange", "dark red"))
  })
})
