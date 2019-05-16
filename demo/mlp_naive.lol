// activation function
func List<float> relu (List<float> x, int size){
  for (int i =0; i< size; i++){
    if(x[i] < 0.0){
      x[i] = 0.0;
    }
  }
  return x;
}

// fully-connected 1st layer
func List<float> fc1 (List<float> x, int size){
  // hard coded weights and biases
  List<List<float>> w1 = [[1.0, 1.0], [1.0, 1.0]];
  List<float> b1 = [0.0,-1.0];

  List<float> out = [0.0,0.0];

  // dot product
  for (int i=0; i< x.length(); i++){
    for (int j=0; j< size; j++){
      out[j] += w1[i][j] * x[i];
    }
  }

  // add bias
  for (int i=0; i< size; i++){
    out[i] += b1[i];
  }
  return out;
}

// fully-connected 2nd layer
func List<float> fc2 (List<float> x, int size){
  // hard coded weights and biases
  List<List<float>> w2 = [[1.0],[-2.0]];
  List<float> b2 = [0.0];

  List<float> out = [0.0];

  // dot product
  for (int i=0; i< x.length(); i++){
    for (int j=0; j< size; j++){
      out[j] += w2[i][j] * x[i];
    }
  }

  // add bias
  for (int i=0; i< size; i++){
    out[i] += b2[i];
  }
  return out;
}

List<List<float>> input = [[0.0, 0.0],[0.0, 1.0],[1.0, 0.0],[1.0, 1.0]];

// list of function to be passed sequentially
List<func(List<float>,int:List<float>)> sequential = [];
sequential.append(fc1);
sequential.append(relu);
sequential.append(fc2);
sequential.append(relu);

// helper for the correct dimension
List<int> dim = [2,2,1,1];

// loop through each input
for (int i = 0; i<input.length();i++){
  List<float> x = input[i];
  // go through sequence and apply function
  for (int j =0; j< sequential.length(); j++){
    x = sequential[j](x,dim[j]);
  }
  println(str_of_float(x[0]));
}
// expects 0,1,1,0
