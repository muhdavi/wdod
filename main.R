a= c("A","A","B","C","C","C")
b= c("E","D","G","D","G","F")
c= c("M","N","M","N","M","N")
data= cbind(a,b,c)
data=as.data.frame(data)
data
#Mendifinisikan tiap kolom
library(RoughSets)
data= SF.asDecisionTable(data)
A=1
B=2
C=3

## Compute the indiscernibility classes:
## 1. Mencari data yang sama pada setiap kolom
IND.A <- BC.IND.relation.RST(data, feature.set = A)
IND.B <- BC.IND.relation.RST(data, feature.set = B)
IND.C <- BC.IND.relation.RST(data, feature.set = C)
a = IND.A$IND.relation
b = IND.B$IND.relation
c = IND.C$IND.relation

##Menghitung banyak data dengan nilai yang sama pada setiap kolom
a1=length(a$A)
a2=length(a$B)
a3=length(a$C)
b1=length(b$D)
b2=length(b$E)
b3=length(b$F)
b4=length(b$G)
c1=length(c$M)
c2=length(c$N)

## Compute complement entropy
## 2. Menghitung nilai complement entropy setiap kolom
n=nrow(data)
ce.a=(a1/n*(1-(a1/n)))+(a2/n*(1-(a2/n)))+(a3/n*(1-(a3/n)))
ce.b=(b1/n*(1-(b1/n)))+(b2/n*(1-(b2/n)))+(b3/n*(1-(b3/n)))+(b4/n*(1-(b4/n)))
ce.c=(c1/n*(1-(c1/n)))+(c2/n*(1-(c2/n)))

## Compute Weight density
## 3. Menghitung nilai weight tiap kolom
ce.al=1-ce.a
ce.bl=1-ce.b
ce.cl=1-ce.c
wl=ce.al+ce.bl+ce.cl

#nilai weight tiap kolom
we.a=(1-ce.a)/wl
we.b=(1-ce.b)/wl
we.c=(1-ce.c)/wl

we=rbind(we.a,we.b,we.c)
we
we=as.data.frame(we)
colnames(we)= "weight"
we

## 4. Menhitung nilai WDOD
## Menghitung kemunculan tiap nilai data
freq <- table(unlist(unname(data)))
freq

## Menghitung skor Weight density based outlier detection tiap data
## bagian error cara 1
d[,"Score"] <-apply(d,1,function(x) {
  sum(freq[x])/nrow(x)*we
})

## bagian error cara 2
for (i in 1:1){
  for(j in 1:1){
    for (k in 1:3){
      a[k+1]= sum((freq[k]/nrow(d))*we[k])
    }
    b= sum(a)
  }
  print(b)
}
