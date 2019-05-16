// relu activation function
func Matrix relu (Matrix x){
  for (int i =0; i< x.row(); i++){
    for (int j=0; j< x.col(); j++){
      if (x[i,j] < 0.0){
        x[i,j] = 0.0;
      }
    }
  }
  return x;
}

// fully-connected 1st layer
func Matrix fc1 (Matrix x){
  // hard coded weights and biases
  Matrix w1 = Matrix([1.0, 1.0], [1.0, 1.0]);
  Matrix b1 = Matrix([0.0,-1.0]); //<1,2>

  Matrix out = mmul(x,w1); // <1,2>
  madd(out,b1);
  return out;
}

// fully-connected 1st layer
func Matrix fc2 (Matrix x){
  // hard coded weights and biases
  Matrix w2 = Matrix([1.0],[-2.0]);
  Matrix b2 = Matrix([0.0]); //<1,1>

  Matrix out = mmul(x,w2); // <1,1>
  madd(out,b2);
  return out;
}


List<Matrix> input = [Matrix([0.0, 0.0]),Matrix([0.0, 1.0]),Matrix([1.0, 0.0]),Matrix([1.0, 1.0])];

for (int i = 0; i<input.length(); i++){
  Matrix x = relu(fc1(input[i]));
  x = relu(fc2(x));
  printm(x);
}
