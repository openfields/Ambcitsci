% PROGRAM lynx
%   Performs a density-dependent demographic PVA for the Iberian 
%   lynx, with demographic stochasticity (survival of all 
%   classes), environmental stochasticity (class 1 survival 
%   only), and a maximum number of breeders set by the number of 
%   territories, K.  
%   Classes are:
%   (1) cubs; 
%   (2) subadults; 
%   (3) floaters (adults without territories);
%   (4) breeders (territory-holding adults).  
%   Assumes a post-breeding census, and tracks females only.
%   Parameters modified from Gaona et al., Ecological Monographs 
%   68: 349-370 (1998)
 
% NOTE: This code has been modified from the original code that
% appeared in Morris and Doak, 2002, Quantitative Conservation Biology,
% Sinauer Associates, Sunderland, MA.
 
% ************  USER-DEFINED PARAMETERS  ************************
s1min=0.2;      % class 1 survival in bad years
ds1=0.3;        % class 1 surv. in good years=s1min+ds1
freqbad=0.1;    % frequency of bad years
s2=0.7;         % class 2 survival
s3=0.7;         % class 3 survival
s4=0.86;        % class 4 survival
f4=0.5*0.6*2.9;  % reproduction of breeders=sex ratio x 
                %   breeding propensity x mean litter size 
Ks=[1 2 3 4 6 8 10];   % max. number of territories
imax=length(Ks);    
n0=[4 2 0 5];   % starting population vector
Nx=2;           % quasi-extinction threshold
tmax=50;        % time horizon
numreps=1000;   % no. of replicates for prob.(ext.) calculation
% ***************************************************************
 
rand('state',sum(100*clock)); % seed random number generator
 
for i=1:imax            % For each possible no. of territories,
    K=Ks(i);
    Ns=zeros(1,numreps);
    for rep=1:numreps   % for each replicate population, 
        n=n0;           % starting at the initial vector,
        for t=1:tmax    % for each year,
            s1=s1min+ds1*(rand>freqbad); % draw class 1 survival
                         % at random given environmental 
                         % stochasticity, then
            nn(2)=sum(rand(1,n(1))<s1);   % determine no. of 
            nn(3)=sum(rand(1,n(2))<s2)... % survivors in all  
                + sum(rand(1,n(3))<s3);   % classes by Monte Carlo
            nn(4)=sum(rand(1,n(4))<s4);   % simulation.
                                          % Before breeding season starts,
            newbreeders=min([nn(3) K-nn(4)]); % new breeders take 
                                     % empty territories, if any,
            nn(3)=nn(3)-newbreeders;  % leaving class 3
            nn(4)=nn(4)+newbreeders;  % and joining class 4.
            nn(1)=round(f4*nn(4));   % Simulate reproduction by
                                     % new and surviving breeders.
            N=sum(nn);               % Sum total population size & 
            if N<=Nx break; end;     % stop if threshold is hit.
            n=nn;                    % replace old vector with the new one.
        end; % for t
        Ns(rep)=N;                   % Store final population size.
    end; % for rep
    probext(i)=sum(Ns<=Nx)/numreps; % Calculate quasi-ext. prob.
end; % for i
 
plot(Ks,probext);  % Plot quasi-ext. prob. vs. no. of territories
axis([0 max(Ks) 0 max(probext)])
xlabel('Maximum number of territories')
ylabel('Probability of quasi-extinction') 