// RUN: %clang_cc1 -fsyntax-only -ferror-limit 100000 -fopenmp -verify %s
// This test was initially supposed for '#pragma simd' and was
// changed to apply to '#pragma omp simd', which is similar.

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp simd'}} */
#pragma omp simd

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp simd'}} */
#pragma omp simd foo

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp simd'}} */
#pragma omp simd safelen(4)

void test_no_clause()
{
  int i;
  #pragma omp simd
  for (i = 0; i < 16; ++i) ;
}

void test_invalid_clause()
{
  int i;
  /* expected-warning@+1 {{extra tokens at the end of '#pragma omp simd' are ignored}} */
  #pragma omp simd foo bar
  for (i = 0; i < 16; ++i) ;
}

void test_non_identifiers()
{
  int i, x;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  #pragma omp simd;
  for (i = 0; i < 16; ++i) ;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  #pragma omp simd private(x);
  for (i = 0; i < 16; ++i) ;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  #pragma omp simd , private(x);
  for (i = 0; i < 16; ++i) ;
}

void test_safelen()
{
  int i;
  /* expected-error@+1 {{expected '('}}  expected-error@+1 {{expected expression}}*/
  #pragma omp simd safelen
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd safelen()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}*/
  #pragma omp simd safelen(,
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+2 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  /* expected-error@+1 {{expected expression}}  expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}*/
  #pragma omp simd safelen(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected '('}} */
  #pragma omp simd safelen 4)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4,
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+3 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4,)
  for (i = 0; i < 16; ++i) ;
  /* xxpected-error@+1 {{expected expression}} */
  #pragma omp simd safelen(4)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4 4)
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+3 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4,,4)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd safelen(4)
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+3 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  /* expected-error@+2 {{expected ')'}} */
  /* expected-note@+1 {{to match this '('}} */
  #pragma omp simd safelen(4,8)
  for (i = 0; i < 16; ++i) ;
}

void test_linear()
{
  int i;
  /* expected-error@+1 {{expected expression}} expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}} */
  #pragma omp simd linear(
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected expression}} expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}*/
  #pragma omp simd linear(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd linear(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd linear()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd linear(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp simd linear(0)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{use of undeclared identifier 'x'}} */
  #pragma omp simd linear(x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{use of undeclared identifier 'x'}} */
  /* expected-error@+1 {{use of undeclared identifier 'y'}} */
  #pragma omp simd linear(x, y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+3 {{use of undeclared identifier 'x'}} */
  /* expected-error@+2 {{use of undeclared identifier 'y'}} */
  /* expected-error@+1 {{use of undeclared identifier 'z'}} */
  #pragma omp simd linear(x, y, z)
  for (i = 0; i < 16; ++i) ;

  int x, y;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd linear(x:)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd linear(x:,)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd linear(x:1)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd linear(x:2*2)
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+2 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp simd linear(x:1,y)
  for (i = 0; i < 16; ++i) ;
  // expected-warning@+2 {{extra tokens at the end of '#pragma omp simd' are ignored}}
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp simd linear(x:1,y,z:1)
  for (i = 0; i < 16; ++i) ;

  // expected-note@+2 {{defined as linear}}
  // expected-error@+1 {{linear variable cannot be linear}}
  #pragma omp simd linear(x) linear(x)
  for (i = 0; i < 16; ++i) ;

  // expected-note@+2 {{defined as private}}
  // expected-error@+1 {{private variable cannot be linear}}
  #pragma omp simd private(x) linear(x)
  for (i = 0; i < 16; ++i) ;

  // expected-note@+2 {{defined as linear}}
  // expected-error@+1 {{linear variable cannot be private}}
  #pragma omp simd linear(x) private(x)
  for (i = 0; i < 16; ++i) ;

  // expected-note@+2 {{defined as linear}}
  // expected-error@+1 {{linear variable cannot be lastprivate}}
  #pragma omp simd linear(x) lastprivate(x)
  for (i = 0; i < 16; ++i) ;

  // expected-note@+2 {{defined as lastprivate}}
  // expected-error@+1 {{lastprivate variable cannot be linear}}
  #pragma omp simd lastprivate(x) linear(x) 
  for (i = 0; i < 16; ++i) ;
}

void test_private()
{
  int i;
  /* expected-error@+2 {{expected expression}} */
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp simd private(
  for (i = 0; i < 16; ++i) ;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd private(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd private(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd private()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd private(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp simd private(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp simd private(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd private(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd private(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_firstprivate()
{
  int i;
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{unexpected OpenMP clause 'firstprivate' in directive '#pragma omp simd'}} */
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd firstprivate(
  for (i = 0; i < 16; ++i) ;
}

void test_lastprivate()
{
  int i;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd lastprivate(
  for (i = 0; i < 16; ++i) ;

  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd lastprivate(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd lastprivate(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd lastprivate()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd lastprivate(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp simd lastprivate(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp simd lastprivate(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd lastprivate(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd lastprivate(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_reduction()
{
  int i, x, y;
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp simd reduction(
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp simd reduction()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp simd reduction(x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected identifier}} */
  #pragma omp simd reduction(:x)
  for (i = 0; i < 16; ++i) ;
  // expected-error@+4 {{expected ')'}} expected-note@+4 {{to match this '('}}
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd reduction(,
  for (i = 0; i < 16; ++i) ;
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp simd reduction(+
  for (i = 0; i < 16; ++i) ;

  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  //
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd reduction(+:
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd reduction(+:)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd reduction(+:,y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd reduction(+:x,+:y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd reduction(%:x)
  for (i = 0; i < 16; ++i) ;

  #pragma omp simd reduction(+:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(*:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(-:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(|:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(^:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(&&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(||:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(max:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp simd reduction(min:x)
  for (i = 0; i < 16; ++i) ;
  struct X { int x; };
  struct X X;
  // TODO: Is the following error correct?
  // expected-error@+1 {{expected variable name}}
  #pragma omp simd reduction(+:X.x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp simd reduction(+:x+x)
  for (i = 0; i < 16; ++i) ;
}

void test_aligned()
{
  int i;
  /* expected-error@+2 {{expected expression}} */
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp simd aligned(
  for (i = 0; i < 16; ++i) ;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd aligned(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp simd aligned(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd aligned()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp simd aligned(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp simd aligned(0)
  for (i = 0; i < 16; ++i) ;

  int *x, y, z[25];
  #pragma omp simd aligned(x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}} */
  #pragma omp simd aligned(x, y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}} */
  #pragma omp simd aligned(x, y, z)
  for (i = 0; i < 16; ++i) ;

  #pragma omp simd aligned(x:4)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}} */
  #pragma omp simd aligned(x, y:8)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}} */
  #pragma omp simd aligned(x, y, z:10+6)
  for (i = 0; i < 16; ++i) ;
  // expected-error@+2 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}}
  // expected-error@+1 {{expression is not an integer constant expression}}
  #pragma omp simd aligned(x, y, z:x)
  for (i = 0; i < 16; ++i) ;
  // expected-note@+2 {{defined as aligned}}
  // expected-error@+1 {{aligned variable cannot be aligned}}
  #pragma omp simd aligned(x:16) aligned(z,x:16)
  for (i = 0; i < 16; ++i) ;
}

void test_multiple_clauses()
{
  int i;
  float x = 0, y = 0, z = 0;
  #pragma omp simd safelen(4) reduction(+:x, y) reduction(-:z) // OK
  for (i = 0; i < 16; ++i);

  // expected-error@+1 {{private variable cannot be lastprivate}} expected-note@+1 {{defined as private}}
  #pragma omp simd private(x), lastprivate(x)
  for (i = 0; i < 16; ++i);

  #pragma omp simd safelen(4) reduction(+:x, y), reduction(-:z)
  for (i = 0; i < 16; ++i);

  #pragma omp simd reduction(+:x, y) reduction(-:z)
  for (i = 0; i < 16; ++i);
}

void test_for()
{
  // expected-error@+3 {{expected '(' after 'for'}}
  // expected-error@+2 2{{use of undeclared identifier 'i'}}
  #pragma omp simd
  for int i = 0; i < 16; i++);

  // expected-error@+3 {{expected ')'}}
  // expected-note@+2 {{to match this '('}}
  #pragma omp simd
  for (int i = 0; i < 16; i++;

  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp simd
  for (int i = 0 i < 16; i++);

  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp simd
  for (int i = 0; i < 16 i++);

  // expected-error@+2 2 {{expected ';' in 'for' statement specifier}}
  #pragma omp simd
  for (int i = 0 i < 16 i++);

  int i = 0;
  // expected-error@+2 {{initialization of for-loop does not have canonical form}}
  #pragma omp simd
  for (; i < 16; ++i);

  // expected-error@+2 {{condition of for-loop does not have canonical form}}
  #pragma omp simd
  for (int i = 0; ; ++i);

  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp simd
  for (int i = 0; i < 16; );

  // expected-error@+3 {{condition of for-loop does not have canonical form}}
  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp simd
  for (int i = 0; ;);

}
