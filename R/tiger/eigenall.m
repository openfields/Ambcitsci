function [lambdas,lambda1,W,w,V,v]=eigenall(A);
%	[lambdas,lambda1,W,w,V,v]=eigenall(A)
%   takes the projection matrix A as the argument of the function 
%   eigenall and returns: 
%      lambdas, a vector containing the eigenvalues of A;
%      lambda1, the dominant eigenvalue of A;
%      W, a matrix with the right eigenvectors of A as its columns;
%      w, the dominant right eigenvector of A (rescaled to proportions);
%      V, a matrix with the left eigenvectors of A as its rows; and 
%      v, the dominant left eigenvector of A (rescaled as multiples of 
%                                             its first element).
%   Eigenvalues and eigenvectors are sorted from largest 
%	to smallest.

[W,lambdas]=eig(A);			% W=matrix with right eigenvectors of A 
                            %       as columns
V=conj(inv(W));				% V=matrix with left eigenvectors of A 
                            %       as rows
lambdas=diag(lambdas);		% lambdas=vector of eigenvalues
[lambdas,I]=sort(lambdas);  % sort eigenvalues from smallest to 
                            %       largest
lambdas=flipud(lambdas);	% flip lambdas so that largest value 
                            %       comes first
lambda1=lambdas(1);			% lambda1=dominant eigenvalue
I=flipud(I);                % flip the index vector I
W=W(:,I);					% sort right eigenvectors
V=V(I,:);					% sort left eigenvectors
w=W(:,1);					% w=stable distribution
w=w/sum(w);					% rescale w to represent proportions
v=real(V(1,:))';			% v=vector of reproductive values
v=v/v(1);					% rescale v relative to class 1




