int x = 0;

func int f(int y){
  return x+y;
}

func int g(int z){
  int x = 1;
  return f(z);
}

print(str_of_int(g(1)));
