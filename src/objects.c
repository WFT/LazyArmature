#include <stdlib.h>
#include <stdio.h>
#include "transform.h"
#include "objects.h"

Matrix *otransform(double *args);
void addtriangle(Matrix *mat, double *p1, double *p2, double *p3);

Matrix *sphere_t(double *args) {
  int nVertices = 25;
  double lrad = (M_PI)/ nVertices;
  Matrix *roty = rotate_y_mat(lrad);
  Matrix *sphere = mat_construct(0, 4);
  
  Matrix *arc = mat_construct(0,4);
  double coors[4] = {0, 0, 0, 1};
  int i;
  // generate one arc of points, along z = 0
  for (i = 0; i < nVertices; i++) {
    coors[0] = sin(i * lrad);
    coors[1] = cos(i * lrad);
    mat_add_column(arc, coors);
  }
  Matrix *arcp = mat_multiply(roty, arc);
  Matrix *weave = mat_construct(0,4);
  double col[4];
  // turn the previous arcs into a series of triangles
  for (i = 0; i < nVertices - 1; i++) {
    mat_get_column(arc, i, col);
    mat_add_column(weave, col);
    mat_get_column(arc, i+1, col);
    mat_add_column(weave, col);
    mat_get_column(arcp, i, col);
    mat_add_column(weave, col);
    //arcp, i
    mat_add_column(weave, col);
    mat_get_column(arc, i+1, col);
    mat_add_column(weave, col);
    mat_get_column(arcp, i+1, col);
    mat_add_column(weave, col);
  }
  // end triangles!
  double end[4] = {0, -1, 0, 1};
  mat_get_column(arc, nVertices - 1, col);
  mat_add_column(weave, col);
  mat_add_column(weave, end);
  mat_get_column(arcp, nVertices - 1, col);
  mat_add_column(weave, col);
  mat_add_column(weave, col);
  mat_add_column(weave, end);
  mat_add_column(weave, end);
  // spin the woven matrix and zip it up
  Matrix *tfrmd;
  for (i = 0; i < nVertices * 2; i++) {
    tfrmd = mat_multiply(roty, weave);
    mat_extend(sphere, weave);
    mat_destruct(weave);
    weave = tfrmd;
  }
  Matrix *t  = otransform(args);
  Matrix *ret = mat_multiply(t, sphere);
  mat_destruct(roty);
  mat_destruct(arcp);
  mat_destruct(arc);
  mat_destruct(weave);
  mat_destruct(sphere);
  mat_destruct(t);
  return ret;
}

Matrix *box_t(double *args) {
  Matrix *cube = mat_construct(0, 4);
  double tlf[4] = {-1, 1, 1, 1};	// top left front
  double tlb[4] = {-1, 1, -1, 1};	// top left back
  double trf[4] = {1, 1, 1, 1};	// top right front
  double trb[4] = {1, 1, -1, 1};	// top right back
  double blf[4] = {-1, -1, 1, 1};	// back left front
  double blb[4] = {-1, -1, -1, 1};	// back left back
  double brf[4] = {1, -1, 1, 1};	// back right front
  double brb[4] = {1, -1, -1, 1};	// back right back
  // top face
  addtriangle(cube, tlf, trb, tlb);
  addtriangle(cube, tlf, trf, trb);
  // bottom face
  addtriangle(cube, blf, brb, brf);
  addtriangle(cube, blf, blb, brb);
  // back face
  addtriangle(cube, blb, trb, brb);
  addtriangle(cube, blb, tlb, trb);
  // front face
  addtriangle(cube, blf, brf, trf);
  addtriangle(cube, blf, trf, tlf);
  // right face
  addtriangle(cube, brf, brb, trb);
  addtriangle(cube, brf, trb, trf);
  // left face
  addtriangle(cube, blf, tlf, blb);
  addtriangle(cube, blb, tlf, tlb);
  Matrix *t  = otransform(args);
  Matrix *ret = mat_multiply(t, cube);
  mat_destruct(cube);
  mat_destruct(t);
  return ret;
}

Matrix *color_for_object(Matrix *obj, double *c1,
			 double *c2, double *c3) {
  Matrix *c = mat_construct(obj->cols, 3);
  int i;
  for (i = 0; i < obj->cols; i++) {
    switch (i%3) {
    case 0:
      mat_set_column(c, i, c1);
      break;
    case 1:
      mat_set_column(c, i, c2);
      break;
    case 2:
      mat_set_column(c, i, c3);
      break;
    }
  }
  return c;
}

// turn the arguments into a transformation matrix
// using the standard Sx Sy Sz Rx Ry Rz Mx My Mz
Matrix *otransform(double *args) {
  Matrix *t = scale_mat(args[0], args[1], args[2]);
  t = apply_transform_free(rotate_x_mat(TO_RAD(args[3])), t);
  t = apply_transform_free(rotate_y_mat(TO_RAD(args[4])), t);
  t = apply_transform_free(rotate_z_mat(TO_RAD(args[5])), t);
  t = apply_transform_free(move_mat(args[6], args[7], args[8]), t);
  return t;
}

// add three points to a matrix
void addtriangle(Matrix *mat, double *p1, double *p2, double *p3) {
  mat_add_column(mat, p1);
  mat_add_column(mat, p2);
  mat_add_column(mat, p3);
}
