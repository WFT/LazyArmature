#ifndef TRANSFORM_H
#define TRANSFORM_H
#include <math.h>
#include "matrix.h"

#define TO_RAD(deg) (deg * M_PI / 180)

Matrix *identity_mat();

Matrix *move_mat(double x, double y, double z);

Matrix *scale_mat(double x, double y, double z);

// takes some radians
Matrix *rotate_x_mat(double rad);
Matrix *rotate_y_mat(double rad);
Matrix *rotate_z_mat(double rad);

Matrix *rotate_xyz_point_mat(double xrad, double yrad, double zrad,
			     double xpos, double ypos, double zpos);

// the old object will be free'd
Matrix *apply_transform(Matrix *transform, Matrix *obj);
Matrix *apply_transform_free(Matrix *transform, Matrix *obj);
// **obj is NULL terminated
void apply_transform_many(Matrix *transform, Matrix **obj);
void apply_transform_many_free(Matrix *transform, Matrix **obj);
#endif
