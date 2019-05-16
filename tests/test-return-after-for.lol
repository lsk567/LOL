func string foo() {
  for (int i = 5; i < 10; i = i+1) {
      int x = 5;
      x = 10;
  }
  return "Hello World";
}

println(foo());
