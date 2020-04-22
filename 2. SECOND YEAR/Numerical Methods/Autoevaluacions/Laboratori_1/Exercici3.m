%Script per resoldre equacions de segon grau:

a = input('Escriviu el valor de a: ');
b = input('Escriviu el valor de b: ');
c = input('Escriviu el valor de c: ');

if ((b^2)-(4*a*c)) < 0
    disp('Aquesta equació és irreal!')
  
else
    disp('La primera arrel val:')
    (-b + sqrt((b^2)-(4*a*c))) / 2*a
    disp('La segona arrel val:')
    (-b - sqrt((b^2)-(4*a*c))) / 2*a
    
end
