proc optmodel;

	/* PARAMETRES GENERALS */
	number n_alumnes = 19;

	/* GENERACIO ALEATORIA DE LA INSTANCIA */
	set<number> alumnes = 1..n_alumnes;
	number comp{a in alumnes, b in alumnes} = [
9 3 7 5 9 3 4 6 2 6 6 4 5 4 2 7 6 4 2
6 4 9 2 3 3 6 5 1 9 6 5 2 4 4 6 4 4 5
6 3 4 6 6 6 6 4 2 1 8 10 9 6 8 1 6 10 3
2 6 10 8 3 2 3 2 2 4 6 5 5 7 6 4 3 0 7
4 10 7 3 8 5 2 10 6 1 2 5 4 5 4 5 6 3 3
3 4 5 3 5 9 3 3 6 9 5 2 1 7 6 7 0 3 5
3 7 4 2 3 2 5 2 1 6 6 1 3 5 5 4 4 5 5
2 4 5 0 6 4 5 4 4 8 3 9 3 6 5 7 0 6 6
4 10 3 1 3 0 3 4 2 3 6 4 0 5 6 9 6 6 5
10 6 3 7 4 1 4 4 5 5 1 6 6 6 9 6 6 7 6
3 5 9 5 5 6 5 6 5 1 2 4 2 4 1 4 1 1 4
1 1 0 3 5 6 4 5 6 4 3 8 4 3 7 4 5 3 4
2 6 5 6 3 5 3 5 7 7 1 2 2 1 4 5 5 6 6
7 6 10 9 3 3 10 2 8 8 6 9 3 4 3 8 2 6 8
5 5 5 6 5 4 8 7 6 5 4 4 7 5 8 6 1 3 10
0 2 4 4 6 5 8 3 7 8 2 5 5 7 5 8 3 0 8
4 4 9 5 6 0 5 5 6 9 8 7 5 3 9 3 6 6 3
4 5 4 8 7 4 8 9 10 4 3 5 6 5 6 4 7 3 5
7 2 3 4 5 10 5 5 7 0 6 6 4 3 2 9 9 6 7
];
	
	var X{alumnes, alumnes} binary;

	max COMPATIBILITAT = sum{a in alumnes, b in alumnes} comp[a,b]*X[a,b];

	con ROWS{a in alumnes}: sum{b in alumnes} X[a,b] = 1;
	con COLS{b in alumnes}: sum{a in alumnes} X[a,b] = 1;
	con SYMM{a in alumnes, b in alumnes}: X[a,b] = X[b,a]; 
	con PAIRS: sum{a in alumnes} X[a,a] <= MOD(n_alumnes, 2);

	/* SOLUCIO I OUTPUT */
	solve; 

	print comp;
	print X.sol;

	number Parelles{a in alumnes, 1..2};
	number aux_i;
	number aux_j;
	do aux_i=alumnes;
		do aux_j=alumnes;
			if (X[aux_i,aux_j].sol > 0) then do; Parelles[aux_i, 1] = aux_j; Parelles[aux_i, 2] = comp[aux_i,aux_j];end;
		end;
	end;
	print Parelles;

quit;
