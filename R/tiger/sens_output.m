% run script to do sensitivity and elasticity values for range of adult
% survival: 0.05 to 0.95
series = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]

% 19 elements in vector
% store value, then store sensitivities, then store elasticities

output = zeros(19,9)
F=0.4
g=0.5

for(i=1:19)
    AS = series(i);
    [S,E]=tdetsens(F,g,AS);
    output(i,1)=AS;
    output(i,2)=S(1);
    output(i,3)=S(2);
    output(i,4)=S(3);
    output(i,5)=S(4);
    output(i,6)=E(1);
    output(i,7)=E(2);
    output(i,8)=E(3);
    output(i,9)=E(4);
    
end

outAS=output;


%%%%%%%%%%%%%
% juvenile survival
% run script to do sensitivity and elasticity values for range of adult
% survival: 0.05 to 0.95
series = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]

% 19 elements in vector
% store value, then store sensitivities, then store elasticities

output = zeros(19,9)
F=0.4
%g=series
AS= 0.6
for(i=1:19)
    g = series(i);
    [S,E]=tdetsens(F,g,AS);
    output(i,1)=AS;
    output(i,2)=S(1);
    output(i,3)=S(2);
    output(i,4)=S(3);
    output(i,5)=S(4);
    output(i,6)=E(1);
    output(i,7)=E(2);
    output(i,8)=E(3);
    output(i,9)=E(4);
    
end

outg=output;

%%%%%%%%%%%%%
% Effective fecundity
% run script to do sensitivity and elasticity values for range of adult
% survival: 0.05 to 0.95
series = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9]

% 19 elements in vector
% store value, then store sensitivities, then store elasticities

output = zeros(19,9)
%F=0.4
g=0.5
AS= 0.6
for(i=1:19)
    F = series(i);
    [S,E]=tdetsens(F,g,AS);
    output(i,1)=AS;
    output(i,2)=S(1);
    output(i,3)=S(2);
    output(i,4)=S(3);
    output(i,5)=S(4);
    output(i,6)=E(1);
    output(i,7)=E(2);
    output(i,8)=E(3);
    output(i,9)=E(4);
    
end

outF=output;