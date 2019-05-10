func func(int,int:int) apply_f(func(int:int) f) {
    return func int (int i, int j) {
        return f(i) + f(j);
    };
}

func int double(int x) {
    return x * 2;
}

func(int,int:int) sum_of_double = apply_f(double);

println(str_of_int(sum_of_double(3, 5))); 

