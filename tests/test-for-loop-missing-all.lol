func string infinite(int c) {
    for ( ; ; ) {
        c = c + 1;
        if (c > 100) {
            return "done!";
        }
    }
    
    return "what??";
}

string s = infinite(2);
println(s);
