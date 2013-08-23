// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

namespace std {

struct random_access_iterator_tag { };

template <class Iter>
struct iterator_traits {
  typedef typename Iter::difference_type difference_type; // expected-error {{no type named 'difference_type' in 'Iter'}} expected-error {{no type named 'difference_type' in 'Iter1'}}
  typedef typename Iter::iterator_category iterator_category; // expected-error {{no type named 'iterator_category' in 'Iter'}} expected-error {{no type named 'iterator_category' in 'Iter1'}} expected-error {{no type named 'iterator_category' in 'Iter2'}}
};

template <class Iter>
typename iterator_traits<Iter>::difference_type distance(Iter first, Iter last) {
  return first - last;
}
}

class Iter {
  public:
    Iter() { }
    Iter(const Iter &) { }
    Iter operator ++() { return *this; }
    Iter operator --() { return *this; }
    bool operator <(Iter a) { return true; }
    bool operator >=(Iter a) { return false; }
};
int operator -(Iter a, Iter b) { return 0; }
class Iter1 {
  public:
    Iter1() { }
    Iter1(const Iter1 &) { }
    Iter1 operator ++() { return *this; }
    Iter1 operator --() { return *this; }
    bool operator <(Iter1 a) { return true; }
    bool operator >=(Iter1 a) { return false; }
};
class Iter2 {
  public:
    Iter2() { }
    Iter2(const Iter2 &) { }
    Iter2 operator ++() { return *this; }
    Iter2 operator --() { return *this; }
    bool operator <(Iter2 a) { return true; }
    bool operator >=(Iter2 a) { return false; }
    typedef int difference_type;
};
int operator -(Iter2 a, Iter2 b) { return 0; }
class Iter3 {
  public:
    Iter3() { }
    Iter3(const Iter3 &) { }
    Iter3 operator ++() { return *this; }
    Iter3 operator --() { return *this; }
    bool operator <(Iter3 a) { return true; }
    bool operator >=(Iter3 a) { return false; }
    typedef int difference_type;
    typedef int iterator_category;
};
int operator -(Iter3 a, Iter3 b) { return 0; }
class Iter4 {
  public:
    Iter4() { }
    Iter4(const Iter4 &) { }
    Iter4 operator ++() { return *this; }
    Iter4 operator --() { return *this; }
    bool operator <(Iter4 a) { return true; }
    bool operator >=(Iter4 a) { return false; }
    Iter4 operator+=(int) const {return Iter4();}
    Iter4 operator-=(int) const {return Iter4();}
    typedef int difference_type;
    typedef std::random_access_iterator_tag iterator_category;
};
int operator -(Iter4 a, Iter4 b) { return 0; }

int t;
#pragma omp threadprivate(t)

int main() {
  #pragma omp for
  for (int i = 0; i < 10; i++)
    ++i;
  #pragma omp for
  for (t = 0; t < 10; t++)
    ++t;
  #pragma omp for
  for (int i; i < 10; i++) // expected-error {{initialization of for-loop does not have canonical form}}
    ++i;
  #pragma omp for
  for (float i = 0; i < 10.0f; i++) // expected-error {{variable must be of integer or random access iterator type}}
    ++i;
  #pragma omp for
  for (int i = 0; i != 10; i++) // expected-error {{condition of for-loop does not have canonical form}}
    ++i;
  #pragma omp for
  for (int i = 0; i < 10; i |= 2) // expected-error {{increment of for-loop does not have canonical form}}
    ++i;
  int i;
  #pragma omp for
  for (i = 0; i < 10; i++)
    ++i;
  #pragma omp for
  for (i--; i < 10; i++) // expected-error {{initialization of for-loop does not have canonical form}}
    ++i;
  #pragma omp for
  for (i = 0; i != 10; i++) // expected-error {{condition of for-loop does not have canonical form}}
    ++i;
  #pragma omp for
  for (i = 0; i < 10; i ^= 2) // expected-error {{increment of for-loop does not have canonical form}}
    ++i;
  Iter begin, end;
  #pragma omp for
  for (Iter I = begin; I >= end; ++I) // expected-error {{increment expression must cause 'I' to decrease on each iteration of the loop}}
    ++I;
  #pragma omp for
  for (Iter I = end; I < begin; --I) // expected-error {{increment expression must cause 'I' to increase on each iteration of the loop}}
    ++I;
  #pragma omp for
  for (Iter I = begin; I < end; ++I) // expected-note {{in instantiation of template class 'std::iterator_traits<Iter>' requested here}} expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  #pragma omp for
  for (Iter I = end; I >= begin; --I) // expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  Iter1 begin1;
  #pragma omp for
  for (Iter1 I = begin1; I < begin1; ++I) // expected-note {{in instantiation of template class 'std::iterator_traits<Iter1>' requested here}}  expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  #pragma omp for
  for (Iter1 I = begin1; I >= begin1; --I) // expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  Iter2 begin2;
  #pragma omp for
  for (Iter2 I = begin2; I < begin2; ++I) // expected-note {{in instantiation of template class 'std::iterator_traits<Iter2>' requested here}}  expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  #pragma omp for
  for (Iter2 I = begin2; I >= begin2; --I) // expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  Iter3 begin3;
  #pragma omp for
  for (Iter3 I = begin3; I < begin3; ++I) // expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  #pragma omp for
  for (Iter3 I = begin3; I >= begin3; --I) // expected-error {{iteration variable is not of a random access iterator type}}
    ++I;
  Iter4 begin4;
  #pragma omp for
  for (Iter4 I = begin4; I < begin4; ++I)
    ++I;
  #pragma omp for
  for (Iter4 I = begin4; I >= begin4; --I)
    ++I;
  goto label; // expected-error {{use of undeclared label 'label'}}
  #pragma omp for
  for (int i = 0; i < 100; ++i) {
    label: ++i;
  }
  #pragma omp for collapse(1)
  for (Iter4 I = begin4; I >= begin4; --I)
    ++I;
  #pragma omp for collapse(3)
  for (Iter4 I = begin4; I >= begin4; --I)
  for (Iter4 I1 = begin4; I1 >= begin4; --I1)
  for (Iter4 I2 = begin4; I2 >= begin4; --I2)
    ++I;
  #pragma omp for collapse(0) // expected-error {{expression is not a positive integer value}}
  for (Iter4 I = begin4; I >= begin4; --I)
    ++I;
  #pragma omp for
  for (Iter4 I = begin4; I >= begin4; --I)
    #pragma omp for // expected-error {{region cannot be closely nested inside a worksharing region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp single
  for (Iter4 I = begin4; I >= begin4; --I)
    #pragma omp for // expected-error {{region cannot be closely nested inside a worksharing region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp sections
  for (Iter4 I = begin4; I >= begin4; --I)
    #pragma omp for // expected-error {{region cannot be closely nested inside a worksharing region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp master
  for (Iter4 I = begin4; I >= begin4; --I)
    #pragma omp for // expected-error {{region cannot be closely nested inside a master region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp critical
  for (Iter4 I = begin4; I >= begin4; --I)
    #pragma omp for // expected-error {{region cannot be closely nested inside a critical region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp for ordered
  for (Iter4 I = begin4; I >= begin4; --I)
  #pragma omp ordered
    #pragma omp for // expected-error {{region cannot be closely nested inside an ordered region}}
    for (Iter4 J = begin4; J >= begin4; --J)
      ++I;
  #pragma omp for collapse(3)
  for (Iter4 I = begin4; I >= begin4; --I)
    ++I; // expected-error {{only for-loops are allowed for '#pragma omp for'}}
  ++begin4; 
}
