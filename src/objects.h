#ifndef OBJECTS_H
#define OBJECTS_H
#include "matrix.h"

// s s s r r r m m m
Matrix *sphere_t(double *args);
Matrix *box_t(double *args);
Matrix *color_for_object(Matrix *obj, double *c1,
			 double *c2, double *c3);
#endif
