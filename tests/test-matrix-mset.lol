Matrix t = Matrix([1.0, 2.0,1.0],[3.0, 4.0, 3.0],[4.0,5.0, 4.0],[6.0,7.0,6.0]);
mset(t, 0, 1, 8.0);
println(str_of_float(t[0, 1]));
