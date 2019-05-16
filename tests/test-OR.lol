bool h = true;
bool i = true;
bool j = false;

bool t1 = h OR i;
bool t2 = h OR j;

if (t1) {
	println("T OR T is T");
} else {
	println("T OR T is F");
}

if (t2) {
	println("T OR F is T");
} else {
	println("T OR F is F");
}


