#pragma
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
