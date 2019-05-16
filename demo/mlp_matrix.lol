// activation function
func Matrix relu (Matrix x){
  for (int i =0; i< size; i++){
    if(x[i] < 0.0){
      x[i] = 0.0;
    }
  }
  return x;
}
