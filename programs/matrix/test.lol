// Initialize a 2x2 matrix

// Matrix t = Matrix([5.0]); //[1,1]
// Matrix t = Matrix([1.0,2.0]); //[1,2]
Matrix t1 = Matrix([1.0, 2.0, 3.0], [2.0, 3.0, 4.0]);
//Matrix t2 = Matrix([2.0, 3.0, 4.0], [4.0, 5.0, 6.0]);

/*
t = Matrix([1.0],[2.0]); //(2,1)
t = Matrix([1.0, 2.0],[3.0, 4.0]); //(2 * 2)
t = Matrix([1.0, 2.0,1.0],[3.0, 4.0, 3.0]); //(2 * 3)
t = Matrix([1.0, 2.0,1.0],[3.0, 4.0, 3.0],[4.0,5.0, 4.0],[6.0,7.0,6.0]); //(4 * 3)
*/

//t1.dive(t2);
//t1.addc(2.0);

printm(t1);
//matrix_swap_rows(t1, 0, 1);
//mswapr(t1, 0, 1);
//printm(t1);

Matrix t3 = mgetc(t1, 1);
//Matrix t3 = mtrans(t1);
//Matrix t3 = mgetsub(t1, 0, 0, 0, 0);
//t3 = mtrans(t3);
printm(t3);

// t[0, 0] = 7;
//t.set(0, 1, 8.0);
//t.set(0, 0, 5.0);

//println(str_of_float(t[0, 1]));
//println(str_of_float(t.get(0, 0)));
//println(str_of_float(t.get(1, 0)));
