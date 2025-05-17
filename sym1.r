library(copent)
library(symmetry)
library(stats)
library(ald)
library(FamilyRank)

n = 300
ce1 = 0
b1 = bh2 = bhc1 = bhc2 = bhi = bhk = cm = fm = hm = k2u = k2 = ks = sgn  = 0 
wcx = mgg = mi = moi = mok = nac1 = nac2 = nai = nak = rw = 0
x11(); par(mfrow=c(3,3))
for(i in 1:9){
  # x = rbeta(n,10-i,i) # simulation 1
  x = rALD(n, p=i/10) # simulation 2
  # x = rbinorm(n,0,5,1,1,i/10) # simulation 3
  hist(x,100)
  
  x1 = matrix(x-mean(x),n,1)
  ce1[i] = tst(x1,-x1)
  
  k = 3
  b1[i] = B1(x)
  bh2[i] = BH2(x)
  bhc1[i] = BHC1(x,k)
  bhc2[i] = BHC2(x,k)
  bhi[i] = BHI(x)
  bhk[i] = BHK(x)
  cm[i] = CM(x)
  fm[i] = FM(x)
  #hm[i] = HM(x,k)
  k2u[i] = K2U(x)
  k2[i] = K2(x)
  ks[i] = KS(x)
  sgn[i] = SGN(x)
  wcx[i] = WCX(x)
  mgg[i] = MGG(x)
  mi[i] = MI(x)
  moi[i] = MOI(x,k)
  mok[i] = MOK(x,k)
  nac1[i] = NAC1(x,k)
  nac2[i] = NAC2(x,k)
  nai[i] = NAI(x,k)
  nak[i] = NAK(x,k)
  rw[i] = RW(x)
}
x11(width = 8, height = 12)
par(mfrow=c(6,4))
# x1 = 1:9; xlab1 = "b" # simulation 1
x1 = seq(0.1,0.9,by=0.1); xlab1 = "k" # simulation 2
# x1 = seq(0.1,0.9,by=0.1); xlab1 = "p" # simulation 3
plot(x1,ce1,main="CE",xlab = xlab1,ylab = "stats");lines(x1,ce1)
plot(x1,b1,main="B1",xlab = xlab1,ylab = "stats");lines(x1,b1)
plot(x1,bh2,main="BH2",xlab = xlab1,ylab = "stats");lines(x1,bh2)
plot(x1,bhc1,main="BHC1",xlab = xlab1,ylab = "stats");lines(x1,bhc1)
plot(x1,bhc2,main="BHC2",xlab = xlab1,ylab = "stats");lines(x1,bhc2)
plot(x1,bhi,main="BHI",xlab = xlab1,ylab = "stats");lines(x1,bhi)
plot(x1,bhk,main="BHK",xlab = xlab1,ylab = "stats");lines(x1,bhk)
plot(x1,cm,main="CM",xlab = xlab1,ylab = "stats");lines(x1,cm)
plot(x1,fm,main="FM",xlab = xlab1,ylab = "stats");lines(x1,fm)
#plot(x1,hm,main="HM",xlab = xlab1,ylab = "stats");lines(x1,hm)
plot(x1,k2u,main="K2U",xlab = xlab1,ylab = "stats");lines(x1,k2u)
plot(x1,k2,main="K2",xlab = xlab1,ylab = "stats");lines(x1,k2)
plot(x1,ks,main="KS",xlab = xlab1,ylab = "stats");lines(x1,ks)
plot(x1,sgn,main="SGN",xlab = xlab1,ylab = "stats");lines(x1,sgn)
plot(x1,wcx,main="WCX",xlab = xlab1,ylab = "stats");lines(x1,wcx)
plot(x1,mgg,main="MGG",xlab = xlab1,ylab = "stats");lines(x1,mgg)
plot(x1,mi,main="MI",xlab = xlab1,ylab = "stats");lines(x1,mi)
plot(x1,moi,main="MOI",xlab = xlab1,ylab = "stats");lines(x1,moi)
plot(x1,mok,main="MOK",xlab = xlab1,ylab = "stats");lines(x1,mok)
plot(x1,nac1,main="NAC1",xlab = xlab1,ylab = "stats");lines(x1,nac1)
plot(x1,nac2,main="NAC2",xlab = xlab1,ylab = "stats");lines(x1,nac2)
plot(x1,nai,main="NAI",xlab = xlab1,ylab = "stats");lines(x1,nai)
plot(x1,nak,main="NAK",xlab = xlab1,ylab = "stats");lines(x1,nak)
plot(x1,rw,main="RW",xlab = xlab1,ylab = "stats");lines(x1,rw)
