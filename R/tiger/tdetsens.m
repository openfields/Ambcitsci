function [S,E]=tdetsens(F,g,AS)
pmat=zeros(2,2);
pmat(1)=0
pmat(2)=g
pmat(3)=F
pmat(4)=AS

[lambdas,lambda1,W,w,V,v]=eigenall(pmat);
lambda1
w
v
S=v*w'/(v'*w)
E=pmat.*S/lambda1

