func string foo() {
  for (int i = 5; i < 10; i = i+1) {
    if (i == 5){
      return "hello " + str_of_int(i);
    }
  }
}

println(foo());
