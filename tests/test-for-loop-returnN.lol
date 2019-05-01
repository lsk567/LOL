func string foo() {
  int i;
  for (i = 5; i < 10; i = i+1) {
    return "hello " + str_of_int(i);
  }
}

println(foo());