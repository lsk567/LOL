func func(float,float:float) apply_f(func(float:float) f) {
    return func float (float i, float j) {
        return f(i) + f(j);
    };
}

func float double(float x) {
    return x * 2.0;
}

func(float,float:float) sum_of_double = apply_f(double);

println(str_of_float(sum_of_double(3.4, 5.3))); 

