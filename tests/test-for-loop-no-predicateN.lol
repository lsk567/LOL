func int hi() {
    for (int k = 1; ; k = k + 7) {
        if (k > 100) {
            return 100;
        }
    }
    return 1;
}

println(str_of_int(hi()));
