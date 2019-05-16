func void printEigenValues(List<float> result) {
	println("Eigen value(s): ");
	for (int i = 0; i < result.length(); i++) {
		println(str_of_float(result[i]));
	}
}

func bool validateMatrix(Matrix mat) {
	if(mat.col() == 2 AND mat.row() == 2){
		return true;
	}
	println("The input Matrix has a wrong dimension.");
	return false;
}

func float calculateBForEquation(Matrix matrix) {
	return (matrix.get(0,0) + matrix.get(1,1)) * -1.0;
}
func float calculateCForEquation(Matrix matrix) {
    return matrix.get(0,0) * matrix.get(1,1) - matrix.get(0,1) * matrix.get(1,0);
}

func List<float> solveQuadraticEquation(float determinant, float b) {
	if (determinant > 0.0) {
		return [((b * -1.0) + determinant) / 2.0,((b * -1.0) - determinant) / 2.0];
	}
	if (determinant == 0.0) {
		return [((b * -1.0) + determinant) / 2.0];
	}
	List<float> ret3 = [];
	return ret3;
}

func float solveDeterminant(float b, float c) {
	return (b^2.0 - 4.0 * c) ^ 0.5;
}

func void getEigenValues(Matrix matrix) {
	if (validateMatrix(matrix)){
	float b = calculateBForEquation(matrix);
	float determinant = solveDeterminant(b, calculateCForEquation(matrix));
	List<float> result = solveQuadraticEquation(determinant, b);
	printEigenValues(result);
	}
}
Matrix mat = Matrix([94.45,-4645.4], [3.47,655.69]);
printm(mat);
getEigenValues(mat);

