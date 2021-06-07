#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <omp.h>

double parallel_reduction(double *x, long n, int nt){

	// (1) pensar en paralelismo manual (openmp)
	double *sumas = (double*)malloc(sizeof(double)*nt);

	#pragma omp parallel shared(sumas)

	{
		int tid = omp_get_thread_num();
		int chunk = (n + nt - 1)/nt;
		int start = tid*chunk;
		int end = tid*chunk + chunk;
		//printf("soy thread %i trabajo desde %i hasta %i\n", tid, start, end);
		// fase (1), la parte n/p
		double lsum = 0.0f;
		for(int i=start; i<end && i<n; ++i){
			lsum += x[i];
		}
		sumas[tid] = lsum;
		// esperamos que todos pongan su valor en sumas
		#pragma omp barrier

		// fase (2), reducir arreglo sumas[]
		int l = nt/2;
		// nt = 4
		// l = 2
		// tid: [0,1,2,3]

		// por que no se hace suma secuencial?
		// for i = 1 to n_t: red += sum[i]
		while(l > 0){
			if(tid < l){
				sumas[tid] = sumas[tid] + sumas[tid+l];
				// tid = 0
				// tid = 1
				// sumas[0] = sumas[0] + sumas[0+2 = 2]
				// sumas[1] = sumas[1] + sumas[1+2 = 3]

				// tid = 0
				// sumas[0] = sumas[0] + sumas[0+1 = 1]
			}
			l = l/2;
			// l = 2/1 = 1
			// l = 1/2 = 0

			#pragma omp barrier
		}
	}
	return sumas[0];
}

int main(int argc, char** argv){
    if(argc != 3){
	    fprintf(stderr, "run as ./prog n nt\n");
	    exit(EXIT_FAILURE);
    }
    int N = atoi(argv[1]);
    int nt = atoi(argv[2]);
    omp_set_num_threads(nt);
    double sum = 0.0, *x;
    x = (double*)malloc(sizeof(double)*N);
    for(int i = 0; i < N; ++i){
        x[i] = (double)rand()/(double)RAND_MAX;
    }
    double t1 = omp_get_wtime();
    for(int i = 0; i < N; ++i){
        sum += x[i];
    }
    double t2 = omp_get_wtime();
    double tseq = t2-t1;

    t1 = omp_get_wtime();
    double psum = parallel_reduction(x, N, nt);
    t2 = omp_get_wtime();
    double tpar = t2-t1;
    free(x);
    printf("sum = %f (%f secs)\npsum = %f (%f secs)\nDONE\n", sum, tseq, psum, tpar);
}