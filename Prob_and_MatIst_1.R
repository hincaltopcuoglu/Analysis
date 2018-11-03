##
#permutation#
#in permutation the order of the objects is important

###########################################################################################################
#calculation of factorial
factorial<-function(n){
  n_fac=1
  if(n < 0) {
    print("Sorry, factorial does not exist for negative numbers")
  } else if(n == 0) {
    return(1)
  } else {  
  for (i in 1:n){
   n_fac=n_fac*i
  }
}
  return(n_fac)
}

factorial(7)

###########################################################################################################

#the calculation of factoriel for bigger numbers
stirling_formula<-function(n){
  n_fac=sqrt(2*pi*n)*(n^n)*(exp(1)^-n)
  print(n_fac)
}

stirling_formula(100)

###########################################################################################################

#problem order n of k different objects (with use at once) and calculate different order count
p<-function(n,k){
  cnt_diff_order<-factorial(n)/factorial(n-k)
  print(cnt_diff_order)
}

p(5,2)

###########################################################################################################
library(stringr)
#problem use the every character of "statistics" and calculate how many different words can be produced?
p1<-function(word){
  n<-NULL
  for (i in unique(strsplit(word,"")[[1]])){
    cnt<-sum(str_count(unique(strsplit(word,"")[[1]],letters)))
    n[i]<-str_count(word,i)
    xx=1
    for (m in 1:length(n)){
      xx=xx*(factorial(n[[m]]))
      }
    res=factorial(cnt)/xx
  }
  return(res)
}

p1("istatistik")

###########################################################################################################

#problem how many combinations are there be if we order n object around a circle ?
p2<-function(n){
  comb=factorial(n-1)
  return(comb)
}

p2(7)

###########################################################################################################

##combination##
#in combination the order of the objects are unimportant

###########################################################################################################

#for n>k the combination of n of k is:
c1<-function(n,k){
  if (n>k){
  c_res<-factorial(n)/(factorial(k)*factorial(n-k))
  return(c_res)
  }else{return(0)}
}

#problem:how many different combinations are there if we want to select 4 books within 8 books?
c1(8,4)


#problem: within 4 girls and 7 boys,5 person will be selected to a meeting.Meeting should be set up with 2
#girls and 3 boys,how many combinations are there ?
c1(4,2)*c1(7,3)


###########################################################################################################

#theorem : if k is the sub parts of n and k1+k2+k3=n then we should use permutation equation
#problem 8 different books are wanted to be distribute to 3 children.
#first child is given 1 ,second is given 4 and third is given 3 books.
#so how many different ways these books can be given to children?
#attention here : 1+4+3=8 so formula is 8!/(1!*4!*3!)
#lets solve it function above

factorial(8)/(factorial(1)*factorial(4)*factorial(3))


###########################################################################################################


#binom theorem
#lets say (a+b) is a binary definition.if we want to make expansion of this definition regard to some base
#for example (a+b)^2 we should write a^2 + 2*a*b + b^2
#what if base is so big and want to calculate a score ?

#for example solve (3x+2y)^5 wih binom expansions with arbitrary x,y values which wanted to be calculated
c_binom_expansion<-function(a,x,b,y,n){
    
if(n==0)
   {res=1}
else{
    res=0
    for(i in 1:n){
    res=res+(c1(n,i)*((a*x)^(n-i))*((b*y)^i))
    }
    res=res + (a*x)^n + (b*y)^n
    }
    return(res)
}


#solve (3x+2y)^5 for x=1 and y=1
c_binom_expansion(3,1,2,1,5)

#with same function you can find simple pascal triangle coefficient sum
c_binom_expansion(1,1,1,1,0)
c_binom_expansion(1,1,1,1,1)
c_binom_expansion(1,1,1,1,2)
c_binom_expansion(1,1,1,1,3)
c_binom_expansion(1,1,1,1,4)
  

###########################################################################################################

pasc_triangle<-function(n){
 pasc<-vector('list', n)
 pasc[[1]]<-1
 for(i in 2:n){
   x0<-pasc[[i-1]]
   x1 <- c(0, x0)
   x10 <- c(x0, 0)
   pasc[[i]] <- x1+x10
  }
 return(pasc)
}

pasc_triangle(10)


#######################################




