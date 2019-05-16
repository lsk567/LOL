int q = 7;
if (q == 7) {
    int q = 9;
    println(str_of_int(q));
} else {
    println("no");
}
int i;
for (i = 0; i < 10; i = i+1) {
    int q = 9;
	q = q + 1;
    println(str_of_int(q));
}

func int foo (int a) {
    int q = 21;
    return a;
}

int a = foo(q);

println(str_of_int(a));
println(str_of_int(q));
