/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#include <math.h>
#ifdef WIN32
#define isnan(x) _isnan(x)
#define isinf(x) (!_finite(x))
#endif
int isNAN_float_c(float *x){
  return isnan(*x);
}
int isNAN_double_c(double *x){
  return isnan(*x);
}
int isINF_float_c(float *x){
  return isinf(*x);
}
int isINF_double_c(double *x){
  return isinf(*x);
}
