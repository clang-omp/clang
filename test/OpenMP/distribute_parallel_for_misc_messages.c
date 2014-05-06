// RUN: %clang_cc1 -fsyntax-only -ferror-limit 100000 -fopenmp -verify %s

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp distribute parallel for'}} */
#pragma omp distribute parallel for

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp distribute parallel for'}} */
#pragma omp distribute parallel for foo

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp distribute parallel for'}} */
#pragma omp distribute parallel for collapse

void test_no_clause()
{
  int i;
  #pragma omp distribute parallel for
  for (i = 0; i < 16; ++i) ;
}

void test_invalid_clause()
{
  int i;
  /* expected-warning@+1 {{extra tokens at the end of '#pragma omp distribute parallel for' are ignored}} */
  #pragma omp distribute parallel for foo bar
  for (i = 0; i < 16; ++i) ;
}

void test_non_identifiers()
{
  int i, x;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp distribute parallel for' are ignored}}
  #pragma omp distribute parallel for;
  for (i = 0; i < 16; ++i) ;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp distribute parallel for' are ignored}}
  #pragma omp distribute parallel for private(x);
  for (i = 0; i < 16; ++i) ;

  // expected-warning@+1 {{extra tokens at the end of '#pragma omp distribute parallel for' are ignored}}
  #pragma omp distribute parallel for , private(x);
  for (i = 0; i < 16; ++i) ;
}

void test_private()
{
  int i;
  /* expected-error@+2 {{expected expression}} */
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp distribute parallel for private(
  for (i = 0; i < 16; ++i) ;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp distribute parallel for private(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp distribute parallel for private(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for private()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for private(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp distribute parallel for private(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp distribute parallel for private(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for private(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for private(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_firstprivate()
{
  // TODO: tests on this.
  int i;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for firstprivate(
  for (i = 0; i < 16; ++i) ;
}

void test_lastprivate()
{
  int i;
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for lastprivate(
  for (i = 0; i < 16; ++i) ;

  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp distribute parallel for lastprivate(,
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp distribute parallel for lastprivate(,)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for lastprivate()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for lastprivate(int)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp distribute parallel for lastprivate(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp distribute parallel for lastprivate(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for lastprivate(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for lastprivate(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_reduction()
{
  int i, x, y;
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp distribute parallel for reduction(
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp distribute parallel for reduction()
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp distribute parallel for reduction(x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected identifier}} */
  #pragma omp distribute parallel for reduction(:x)
  for (i = 0; i < 16; ++i) ;
  // expected-error@+4 {{expected ')'}} expected-note@+4 {{to match this '('}}
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp distribute parallel for reduction(,
  for (i = 0; i < 16; ++i) ;
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp distribute parallel for reduction(+
  for (i = 0; i < 16; ++i) ;

  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  //
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for reduction(+:
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for reduction(+:)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for reduction(+:,y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for reduction(+:x,+:y)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 {{expected expression}} */
  #pragma omp distribute parallel for reduction(%:x)
  for (i = 0; i < 16; ++i) ;

  #pragma omp distribute parallel for reduction(+:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(*:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(-:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(|:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(^:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(&&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(||:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(max:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp distribute parallel for reduction(min:x)
  for (i = 0; i < 16; ++i) ;
  struct X { int x; };
  struct X X;
  // TODO: Is the following error correct?
  // expected-error@+1 {{expected variable name}}
  #pragma omp distribute parallel for reduction(+:X.x)
  for (i = 0; i < 16; ++i) ;
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp distribute parallel for reduction(+:x+x)
  for (i = 0; i < 16; ++i) ;
}

void test_multiple_clauses()
{
  int i;
  float x = 0, y = 0, z = 0;
  #pragma omp distribute parallel for reduction(+:x, y) reduction(-:z) // OK
  for (i = 0; i < 16; ++i);

  // expected-error@+1 {{private variable cannot be lastprivate}} expected-note@+1 {{defined as private}}
  #pragma omp distribute parallel for private(x), lastprivate(x)
  for (i = 0; i < 16; ++i);

  #pragma omp distribute parallel for reduction(+:x, y), reduction(-:z)
  for (i = 0; i < 16; ++i);

  #pragma omp distribute parallel for reduction(+:x, y) reduction(-:z)
  for (i = 0; i < 16; ++i);
}

void test_for()
{
  // expected-error@+3 {{expected '(' after 'for'}}
  // expected-error@+2 2{{use of undeclared identifier 'i'}}
  #pragma omp distribute parallel for
  for int i = 0; i < 16; i++);

  // expected-error@+3 {{expected ')'}}
  // expected-note@+2 {{to match this '('}}
  #pragma omp distribute parallel for
  for (int i = 0; i < 16; i++;

  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp distribute parallel for
  for (int i = 0 i < 16; i++);

  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp distribute parallel for
  for (int i = 0; i < 16 i++);

  // expected-error@+2 2 {{expected ';' in 'for' statement specifier}}
  #pragma omp distribute parallel for
  for (int i = 0 i < 16 i++);

  int i = 0;
  // expected-error@+2 {{initialization of for-loop does not have canonical form}}
  #pragma omp distribute parallel for
  for (; i < 16; ++i);

  // expected-error@+2 {{condition of for-loop does not have canonical form}}
  #pragma omp distribute parallel for
  for (int i = 0; ; ++i);

  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp distribute parallel for
  for (int i = 0; i < 16; );

  // expected-error@+3 {{condition of for-loop does not have canonical form}}
  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp distribute parallel for
  for (int i = 0; ;);

}
