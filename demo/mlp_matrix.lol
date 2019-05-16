// fully-connected 1st layer
func Matrix fc1 (Matrix x){
  // hard coded weights and biases
  Matrix w1 = Matrix([1.0, 1.0], [1.0, 1.0]);
  Matrix b1 = Matrix([0.0,-1.0]); //<1,2>

  //println(str_of_int(b1.row()));
  //println(str_of_int(b1.col()));

  Matrix out = mmul(x,w1); // <1,2>
  out.add(b1);
  return out;
}

Matrix input = Matrix([0.0, 0.0]);

fc1(input);
