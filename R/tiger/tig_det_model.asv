% A=projection matrix for declining semipalmated sandpiper pop.
% from Hitchcock and Gatto-Trevor, Ecology 78:522-534 (1997).
% Pre-breeding census - classes are 1, 2, and 3+ year-olds.
% The code below A calculates and displays the dominant eigenvalue
% lambda1, the stable distribution w, the vector of reproductive 
% values v, a matrix of eigenvalue senstitivities S, and a matrix
% of eigenvalue elasticities, E.

F = .4
g = 0.5
S = 0.6
A=[0 F; g S];

[lambdas,lambda1,W,w,V,v]=eigenall(A);
lambda1
w
v
S=v*w'/(v'*w)
E=A.*S/lambda1