// RUN: %clang_cc1 -fsyntax-only -ferror-limit 100000 -fopenmp -verify %s

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp teams distribute parallel for'}} */
#pragma omp teams distribute parallel for

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp teams distribute parallel for'}} */
#pragma omp teams distribute parallel for foo

/* expected-error@+1 {{unexpected OpenMP directive '#pragma omp teams distribute parallel for'}} */
#pragma omp teams distribute parallel for collapse

void test_no_clause()
{
  int i;
  #pragma omp target
  #pragma omp teams distribute parallel for
  for (i = 0; i < 16; ++i) ;
}

void test_invalid_clause()
{
  int i;
  #pragma omp target
  /* expected-warning@+1 {{extra tokens at the end of '#pragma omp teams distribute parallel for' are ignored}} */
  #pragma omp teams distribute parallel for foo bar
  for (i = 0; i < 16; ++i) ;
}

void test_non_identifiers()
{
  int i, x;

  #pragma omp target
  // expected-warning@+1 {{extra tokens at the end of '#pragma omp teams distribute parallel for' are ignored}}
  #pragma omp teams distribute parallel for;
  for (i = 0; i < 16; ++i) ;

  #pragma omp target
  // expected-warning@+1 {{extra tokens at the end of '#pragma omp teams distribute parallel for' are ignored}}
  #pragma omp teams distribute parallel for private(x);
  for (i = 0; i < 16; ++i) ;

  #pragma omp target
  // expected-warning@+1 {{extra tokens at the end of '#pragma omp teams distribute parallel for' are ignored}}
  #pragma omp teams distribute parallel for , private(x);
  for (i = 0; i < 16; ++i) ;
}

void test_private()
{
  int i;
  #pragma omp target
  /* expected-error@+2 {{expected expression}} */
  // expected-error@+1 {{expected ')'}} expected-note@+1 {{to match this '('}}
  #pragma omp teams distribute parallel for private(
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp teams distribute parallel for private(,
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp teams distribute parallel for private(,)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for private()
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for private(int)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp teams distribute parallel for private(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp target
  #pragma omp teams distribute parallel for private(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for private(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for private(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_firstprivate()
{
  // TODO: tests on this.
  int i;
  #pragma omp target
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for firstprivate(
  for (i = 0; i < 16; ++i) ;
}

void test_lastprivate()
{
  int i;
  #pragma omp target
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for lastprivate(
  for (i = 0; i < 16; ++i) ;

  #pragma omp target
  // expected-error@+2 {{expected ')'}} expected-note@+2 {{to match this '('}}
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp teams distribute parallel for lastprivate(,
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp teams distribute parallel for lastprivate(,)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for lastprivate()
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for lastprivate(int)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp teams distribute parallel for lastprivate(0)
  for (i = 0; i < 16; ++i) ;

  int x, y, z;
  #pragma omp target
  #pragma omp teams distribute parallel for lastprivate(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for lastprivate(x, y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for lastprivate(x, y, z)
  for (i = 0; i < 16; ++i) ;
}

void test_reduction()
{
  int i, x, y;
  #pragma omp target
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp teams distribute parallel for reduction(
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+2 {{expected identifier}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp teams distribute parallel for reduction()
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp teams distribute parallel for reduction(x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected identifier}} */
  #pragma omp teams distribute parallel for reduction(:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  // expected-error@+4 {{expected ')'}} expected-note@+4 {{to match this '('}}
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 2 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(,
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  /* expected-error@+2 {{expected expression}} */
  /* expected-error@+1 {{expected ':' in 'reduction' clause}} */
  #pragma omp teams distribute parallel for reduction(+
  for (i = 0; i < 16; ++i) ;

  #pragma omp target
  // expected-error@+3 {{expected ')'}} expected-note@+3 {{to match this '('}}
  //
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(+:
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(+:)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(+:,y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(+:x,+:y)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+3 {{expected identifier}} */
  /* expected-error@+2 {{expected ':' in 'reduction' clause}} */
  /* expected-error@+1 {{expected expression}} */
  #pragma omp teams distribute parallel for reduction(%:x)
  for (i = 0; i < 16; ++i) ;

  #pragma omp target
  #pragma omp teams distribute parallel for reduction(+:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(*:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(-:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(|:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(^:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(&&:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(||:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(max:x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(min:x)
  for (i = 0; i < 16; ++i) ;
  struct X { int x; };
  struct X X;
  #pragma omp target
  // TODO: Is the following error correct?
  // expected-error@+1 {{expected variable name}}
  #pragma omp teams distribute parallel for reduction(+:X.x)
  for (i = 0; i < 16; ++i) ;
  #pragma omp target
  /* expected-error@+1 {{expected variable name}} */
  #pragma omp teams distribute parallel for reduction(+:x+x)
  for (i = 0; i < 16; ++i) ;
}

void test_multiple_clauses()
{
  int i;
  float x = 0, y = 0, z = 0;
  #pragma omp target
  #pragma omp teams distribute parallel for reduction(+:x, y) reduction(-:z) // OK
  for (i = 0; i < 16; ++i);

  #pragma omp target
  // expected-error@+1 {{private variable cannot be lastprivate}} expected-note@+1 {{defined as private}}
  #pragma omp teams distribute parallel for private(x), lastprivate(x)
  for (i = 0; i < 16; ++i);

  #pragma omp target
  #pragma omp teams distribute parallel for reduction(+:x, y), reduction(-:z)
  for (i = 0; i < 16; ++i);

  #pragma omp target
  #pragma omp teams distribute parallel for reduction(+:x, y) reduction(-:z)
  for (i = 0; i < 16; ++i);
}

void test_for()
{
  #pragma omp target
  // expected-error@+3 {{expected '(' after 'for'}}
  // expected-error@+2 2{{use of undeclared identifier 'i'}}
  #pragma omp teams distribute parallel for
  for int i = 0; i < 16; i++);

  #pragma omp target
  // expected-error@+3 {{expected ')'}}
  // expected-note@+2 {{to match this '('}}
  #pragma omp teams distribute parallel for
  for (int i = 0; i < 16; i++;

  #pragma omp target
  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp teams distribute parallel for
  for (int i = 0 i < 16; i++);

  #pragma omp target
  // expected-error@+2 {{expected ';' in 'for' statement specifier}}
  #pragma omp teams distribute parallel for
  for (int i = 0; i < 16 i++);

  #pragma omp target
  // expected-error@+2 2 {{expected ';' in 'for' statement specifier}}
  #pragma omp teams distribute parallel for
  for (int i = 0 i < 16 i++);

  int i = 0;
  #pragma omp target
  // expected-error@+2 {{initialization of for-loop does not have canonical form}}
  #pragma omp teams distribute parallel for
  for (; i < 16; ++i);

  #pragma omp target
  // expected-error@+2 {{condition of for-loop does not have canonical form}}
  #pragma omp teams distribute parallel for
  for (int i = 0; ; ++i);

  #pragma omp target
  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp teams distribute parallel for
  for (int i = 0; i < 16; );

  #pragma omp target
  // expected-error@+3 {{condition of for-loop does not have canonical form}}
  // expected-error@+2 {{increment of for-loop does not have canonical form}}
  #pragma omp teams distribute parallel for
  for (int i = 0; ;);

}
