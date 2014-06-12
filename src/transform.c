#include "transform.h"

Matrix * identity_mat() {
  Matrix *ret = mat_construct(4, 4);
  int i;
  for(i = 0; i < 4; i++)
    mat_set_cell(ret, i, i, 1);
  return ret;
}

Matrix * move_mat(double x, double y, double z) {
  Matrix *ret = identity_mat();
  mat_set_cell(ret, 3, 0, x);
  mat_set_cell(ret, 3, 1, y);
  mat_set_cell(ret, 3, 2, z);
  return ret;
}

Matrix * scale_mat(double x, double y, double z) {
  Matrix *ret = identity_mat();
  mat_set_cell(ret, 0, 0, x);
  mat_set_cell(ret, 1, 1, y);
  mat_set_cell(ret, 2, 2, z);
  return ret;
}

Matrix * rotate_x_mat(double rad) {
  Matrix *ret = identity_mat();
  mat_set_cell(ret, 1, 1, cos(rad));
  mat_set_cell(ret, 1, 2, sin(rad));
  mat_set_cell(ret, 2, 1, -sin(rad));
  mat_set_cell(ret, 2, 2, cos(rad));
  return ret;
}

Matrix * rotate_y_mat(double rad) {
  Matrix *ret = identity_mat();
  mat_set_cell(ret, 0, 0, cos(rad));
  mat_set_cell(ret, 2, 0, sin(rad));
  mat_set_cell(ret, 0, 2, -sin(rad));
  mat_set_cell(ret, 2, 2, cos(rad));
  return ret;
}

Matrix * rotate_z_mat(double rad) {
  Matrix *ret = identity_mat();
  mat_set_cell(ret, 0, 0, cos(rad));
  mat_set_cell(ret, 0, 1, sin(rad));
  mat_set_cell(ret, 1, 0, -sin(rad));
  mat_set_cell(ret, 1, 1, cos(rad));
  return ret;
}

Matrix *rotate_xyz_point_mat(double xrad, double yrad, double zrad,
			     double xpos, double ypos, double zpos) {
  Matrix *m1 = move_mat(-xpos, -ypos, -zpos);
  Matrix *m2 = move_mat(xpos, ypos, zpos);
  Matrix *t = apply_transform_free(rotate_x_mat(TO_RAD(xrad)), m1);
  t = apply_transform_free(rotate_y_mat(TO_RAD(yrad)), t);
  t = apply_transform_free(rotate_z_mat(TO_RAD(zrad)), t);
  t = apply_transform_free(m2, t);
  return t;
}

Matrix *apply_transform(Matrix *transform, Matrix *obj) {
  Matrix *ret = mat_multiply(transform, obj);
  mat_destruct(obj);
  return ret;
}

Matrix *apply_transform_free(Matrix *transform, Matrix *obj) {
  Matrix *ret = mat_multiply(transform, obj);
  mat_destruct(obj);
  mat_destruct(transform);
  return ret;
}

void apply_transform_many(Matrix *transform, Matrix **obj) {
  int p = 0;
  Matrix *ret;
  while (obj[p]) {
    ret = mat_multiply(transform, obj[p]);
    mat_destruct(obj[p]);
    obj[p] = ret;
    p++;
  }
}

void apply_transform_many_free(Matrix *transform, Matrix **obj) {
  int p = 0;
  Matrix *ret;
  while (obj[p]) {
    ret = mat_multiply(transform, obj[p]);
    mat_destruct(obj[p]);
    obj[p] = ret;
    p++;
  }
  mat_destruct(transform);
}
