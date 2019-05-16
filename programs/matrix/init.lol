// Initialize a 2x2 matrix
Matrix<2,2> t;
t = Matrix([[1.0, 2.0],[3.0, 4.0]]);

t[0, 0] = 7;
t.set(0, 1, 8);

println(str_of_float(t[0, 0]));
println(str_of_float(t.get(0, 0)));
