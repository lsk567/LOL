bool h = true;
bool i = true;
bool j = false;

bool t1 = h AND i;
bool t2 = h AND j;

if (t1) {
	println("T AND T is T");
} else {
	println("T AND T is F");
}

if (t2) {
	println("T AND F is T");
} else {
	println("T AND F is F");
}


