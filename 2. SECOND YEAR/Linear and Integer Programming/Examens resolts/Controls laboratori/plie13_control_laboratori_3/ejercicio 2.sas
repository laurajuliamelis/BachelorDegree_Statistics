proc optmodel;

	/* PARAMETRES GENERALS */
	number n_alumnes = 18;
	number min_comp = 5;

	/*  INSTANCIA */
	set<number> alumnes = 1..n_alumnes;
	number comp{a in alumnes, b in alumnes} = [
0 3 5 4 6 7 5 0 5 4 7 6 8 4 6 3 2 7
2 0 4 8 1 1 3 6 4 9 7 3 3 7 9 6 7 5
6 4 0 7 1 4 1 2 6 9 8 3 4 7 2 4 6 2
3 5 3 0 6 6 4 5 4 1 4 7 9 8 10 6 3 9
9 4 1 9 0 4 4 2 2 3 5 7 8 8 6 5 3 4
3 7 0 3 3 0 4 2 4 6 1 1 4 6 4 8 1 6
2 3 5 4 3 9 0 6 5 6 2 3 1 8 2 2 3 7
0 8 3 8 6 6 6 0 3 6 6 6 10 0 6 3 2 5
3 6 5 5 1 1 6 8 0 7 10 2 5 8 7 8 6 5
5 3 3 6 7 5 7 4 3 0 5 8 4 2 6 2 6 5
6 3 6 5 6 3 6 0 4 5 0 9 7 7 5 6 8 6
1 7 6 2 3 5 4 4 7 3 5 0 6 9 1 3 10 3
4 7 5 1 4 2 4 10 5 7 6 3 0 4 7 1 2 6
6 10 3 4 7 0 3 4 4 0 5 2 3 0 3 5 6 2
6 4 4 4 2 2 3 8 7 8 5 0 2 4 0 5 4 3
5 2 9 5 7 0 2 3 3 4 4 4 9 9 5 0 8 4
9 4 1 9 3 4 6 3 4 5 4 6 5 3 3 2 0 7
8 5 1 6 5 7 4 8 5 2 1 3 5 6 5 7 4 0
];
	var X{alumnes, alumnes} binary ;

	max COMPATIBILITAT = sum{a in alumnes, b in alumnes} comp[a,b]*X[a,b];

	con ROWS{a in alumnes}: sum{b in alumnes} X[a,b] = 1;
	con COLS{b in alumnes}: sum{a in alumnes} X[a,b] = 1;
	con SYMM{a in alumnes, b in alumnes}: X[a,b] = X[b,a]; 
	con MIN_C{a in alumnes, b in alumnes}: min_comp*X[a,b] <= comp[a,b];

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
