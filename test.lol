/*
func int add (int i){
  return i + 1;
}

func int apply_2(int i, func f){
  return f(f (i));
}

println (str_of_int(apply_2(3,add)));
*/

int y = 10;

func int addOne (int x){
  return x+1;
}

println(str_of_int(addOne(1)));
