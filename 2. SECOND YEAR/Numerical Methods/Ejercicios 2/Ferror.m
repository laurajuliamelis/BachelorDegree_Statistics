function [ ea, er ] = ferror( x,y )
%calcul error absolut i relatiu
%   Detailed explanation goes here
ea=abs(x-y)
er=ea/y

end

