#include <stdio.h>

int main() {
    int numeros[] = {1, 2, 54, 28, 11, 96};
    int longitud = sizeof numeros / sizeof numeros[0];
    int sumatoria = 0; //Aqui vamos a almacenar cada valor
    for (int x = 0; x < longitud; x++) {
        int numeroActual = numeros[x];
        // Sumar el numero actual a la sumatoria
        sumatoria = sumatoria + numeroActual;
        // Tambien se puede sumar de forma mas simple:
        sumatoria += numeroActual;
    }
    printf("La sumatoria es: %d", sumatoria);
    return 0;
}
