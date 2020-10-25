#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CLAMP(X) X > 0 ? 1 : (X < 0 ? -1 : 0)
struct axis{
  long long p[4];
  long long v[4];
};

void step_axis(struct axis* a){
  for(int i = 0; i < 4; i++)
    for(int j = 0; j < 4; j++)
      a->v[i] += CLAMP(a->p[j] - a->p[i]);
  for(int i = 0; i < 4; i++)
    a->p[i] += a->v[i];
}
int axis_eq(struct axis* a1, struct axis* a2){
  int ret = 0;
  for(int i = 0; i < 4; i++){
    ret += (a1->p[i] == a2->p[i] ? 0 : 1);
    ret += (a1->v[i] == a2->v[i] ? 0 : 1);
  }
  return ret;
}
long long axis_period(struct axis* a){
  long long ret = 0;
  struct axis* c = malloc(sizeof(struct axis));
  memcpy(c, a, sizeof(struct axis));
  step_axis(c); ret++;
  while(axis_eq(c, a) != 0){
    step_axis(c); ret++;
  }
  return ret;
}

/* void print_axis(struct axis* a){ */
/*   for(int i = 0; i < 4; i++) */
/*     printf("%lld ", a->p[i]); */
/*   printf(": "); */
/*   for(int i = 0; i < 4; i++) */
/*     printf("%lld ", a->v[i]); */
/*   printf("\n"); */
/* } */
int main()
{
  struct axis x = {{17, -2, 7, 1}, {0, 0, 0, 0}};
  struct axis y = {{5, -8, -6, -10}, {0, 0, 0, 0}};
  struct axis z = {{1, 8, 14, 4}, {0, 0, 0, 0}};
  long long xp = axis_period(&x);
  long long yp = axis_period(&y);
  long long zp = axis_period(&z);
  printf("%lld %lld %lld\n", xp, yp, zp);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc -ggdb -o day12 day12.c" */
/* End: */
