#include<stdio.h>

#include<caml/mlvalues.h>
#include<caml/memory.h>
#include<caml/callback.h>
#include<caml/fail.h>
#include<caml/alloc.h>
#include<caml/misc.h>

value biocaml_pwm_scan(value mat, value seq, value caml_tol) {
  CAMLparam3(mat, seq, caml_tol);
  CAMLlocal3(r,tmp,hit);
  int n = Wosize_val(seq);
  int m = Wosize_val(mat);
  double tol = Double_val(caml_tol);
  int i,j;
  
  r = Val_int(0); // empty list
  for(i = n - m; i >= 0; i--) {
    float score = 0.;
    for(j = 0; j < m; j++) {
      score += Double_field(Field(mat, j), Int_val(Field(seq,i+j)));
    }
    if(score > tol) {
      tmp = r;
      hit = caml_alloc(2,0);
      Store_field(hit,0,Val_int(i));
      Store_field(hit,1,caml_copy_double(score));
      r = caml_alloc(2,0);
      Store_field(r,0,hit);
      Store_field(r,1,tmp);
    }
  }
  CAMLreturn (r);
}
