// test matrices
Matrix t = Matrix([1.0,2.0]);

/*
t = Matrix([1.0],[2.0]); //(2,1)
t = Matrix([1.0, 2.0],[3.0, 4.0]); //(2 * 2)
t = Matrix([1.0, 2.0,1.0],[3.0, 4.0, 3.0]); //(2 * 3)
t = Matrix([1.0, 2.0,1.0],[3.0, 4.0, 3.0],[4.0,5.0, 4.0],[6.0,7.0,6.0]); //(4 * 3)
*/

/*
t[0, 0] = 7;
t.set(0, 1, 8);
*/
println(str_of_float(t[0, 0]));
//println(str_of_float(t.get(0, 0)));

// Test functions
func int addOne(int x){return x+1;}

func func(int:int) return_f (func(int:int) f){
  return f;
}

func(int:int) x = return_f(addOne);

/*
The following doesnt work:
func func return_f (func f){
  return f;
}

func x = return_f(addOne);
*/
