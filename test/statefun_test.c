#include <stdio.h>
#include "../main/statefun.c"

main(int argc, char *argv[]) {

  //                    x     y     t
  double myapprox[] = {10.0, 20.0, 1.0,
		       15.0, 17.5, 2.5,
		       22.5, 16.0, 3.0,
		       27.0, 23.0, 5.0};

  unsigned span = 2;

  double t_start = 0.0;
  double t_step  = 1.0;
  unsigned n_time = 7;

  double answer[n_time * span];

  unsigned i;

  // simple case: constant approximator; use only first state of myapprox
  estimate_from_approx(myapprox, span, span, t_start, t_step, n_time, -99999, answer);

  puts ("Constant case\ntime     x          y");
  for (i = 0; i < n_time; ++i) 
    printf ("%3.0f %10.3f %10.3f\n", t_start + i * t_step, answer[i * span], answer[i * span + 1]);

  // general case: non-constant approximator
  estimate_from_approx(myapprox, sizeof(myapprox) / sizeof(double), span, t_start, t_step, n_time, -99999, answer);

  puts ("\n\nGeneral case\ntime     x          y");
  for (i = 0; i < n_time; ++i) 
    printf ("%3.0f %10.3f %10.3f\n", t_start + i * t_step, answer[i * span], answer[i * span + 1]);
}

/* output should be:

Constant case
time     x          y
  0     10.000     20.000
  1     10.000     20.000
  2     10.000     20.000
  3     10.000     20.000
  4     10.000     20.000
  5     10.000     20.000
  6     10.000     20.000


General case
time     x          y
  0 -99999.000 -99999.000
  1     10.000     20.000
  2     13.333     18.333
  3     22.500     16.000
  4     24.750     19.500
  5     27.000     23.000
  6 -99999.000 -99999.000

*/
