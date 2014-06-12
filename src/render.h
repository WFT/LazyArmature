#ifndef RENDER_H
#define RENDER_H
#include <SDL2/SDL.h>
#include "matrix.h"

// ambient light colors
uint32_t ambient_red;
uint32_t ambient_green;
uint32_t ambient_blue;

void set_ambient_light(int r, int g, int b);

// renders a single perspective -- access through render functions below
void renderperspective(Matrix *faces, double *eye, Matrix *colors);

// rendering functions for screen
void rendercyclops(Matrix *faces, double *eye, Matrix *colors);
Matrix *spinmat(int x, int y, int z);
void renderseries(Matrix **faces, double *eye, Matrix **colors);
#endif
