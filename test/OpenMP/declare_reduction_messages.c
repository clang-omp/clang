// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int temp;

#pragma omp declare reduction // expected-error {{expected '(' after 'declare reduction'}}
#pragma omp declare reduction { // expected-error {{expected '(' after 'declare reduction'}} expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction ( // expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction (# // expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction (/ // expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction (+ // expected-error {{expected ':'}}
#pragma omp declare reduction (for // expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction (if : // expected-error {{expected identifier or one of the following operators: '+', '-', '*', '&', '|', '^', '&&' and '||'}}
#pragma omp declare reduction (oper : // expected-error {{expected a type}}
#pragma omp declare reduction (oper ; // expected-error {{expected ':'}} expected-error {{expected a type}}
#pragma omp declare reduction (fun : int // expected-error {{expected ':'}} expected-error {{expected expression}}
#pragma omp declare reduction (+ : const int : // expected-error {{a type name cannot be qualified with 'const', 'volatile' or 'restrict'}}
#pragma omp declare reduction (- : volatile int : // expected-error {{a type name cannot be qualified with 'const', 'volatile' or 'restrict'}}
#pragma omp declare reduction (* : int ; // expected-error {{expected ','}} expected-error {{expected a type}}
#pragma omp declare reduction (& : double char : // expected-error {{cannot combine with previous 'double' declaration specifier}} expected-error {{expected expression}}
#pragma omp declare reduction (^ : double, char, : // expected-error {{expected a type}} expected-error {{expected expression}}
#pragma omp declare reduction (&& : int, S : // expected-error {{unknown type name 'S'}} expected-error {{expected expression}}
#pragma omp declare reduction (|| : int, double : temp += omp_in) // expected-error 2 {{variable 'temp' is not allowed in combiner expression for '#pragma omp declare reduction', only 'omp_in' or 'omp_out' are allowed}}
#pragma omp declare reduction (| : char, float : omp_out += temp ) // expected-error 2 {{variable 'temp' is not allowed in combiner expression for '#pragma omp declare reduction', only 'omp_in' or 'omp_out' are allowed}}
#pragma omp declare reduction (fun : long : omp_out += omp_in ) { // expected-warning {{extra tokens at the end of '#pragma omp declare reduction' are ignored}} expected-note {{previous declaration is here}}
#pragma omp declare reduction (fun : unsigned : omp_out += temp)) // expected-error {{variable 'temp' is not allowed in combiner expression for '#pragma omp declare reduction', only 'omp_in' or 'omp_out' are allowed}}
#pragma omp declare reduction (fun : long (*)(void): omp_out += omp_in) // expected-error {{a type name cannot be a function type}}
#pragma omp declare reduction (fun : long [3]: omp_out += omp_in) // expected-error {{a type name cannot be an array type}}
#pragma omp declare reduction (fun23 : long, int, long: omp_out += omp_in) // expected-error {{previous declaration with type 'long' is found}} expected-note {{previous declaration is here}}

#pragma omp declare reduction (fun : long : omp_out += omp_in ) // expected-error {{previous declaration with type 'long' is found}}
#pragma omp declare reduction (fun1 : long : omp_out += omp_in ) initializer // expected-error {{expected '(' after 'initializer'}} expected-error {{expected expression}}
#pragma omp declare reduction (fun2 : long : omp_out += omp_in ) initializer { // expected-error {{expected '(' after 'initializer'}} expected-error {{expected expression}}
#pragma omp declare reduction (fun3 : long : omp_out += omp_in ) initializer [ // expected-error {{expected '(' after 'initializer'}} expected-error {{expected expression}}
#pragma omp declare reduction (fun4 : long : omp_out += omp_in ) initializer() // expected-error {{expected expression}}
#pragma omp declare reduction (fun5 : long : omp_out += omp_in ) initializer(temp) // expected-error {{variable 'temp' is not allowed in initializer expression for '#pragma omp declare reduction', only 'omp_priv' or 'omp_orig' are allowed}}
#pragma omp declare reduction (fun6 : long : omp_out += omp_in ) initializer(omp_orig // expected-error {{expected function call}} expected-error {{expected ')'}} expected-note {{to match this '('}}
#pragma omp declare reduction (fun7 : long : omp_out += omp_in ) initializer(omp_priv 12) // expected-error {{expected '=' after 'omp_priv'}}
#pragma omp declare reduction (fun8 : long : omp_out += omp_in ) initializer(omp_priv=23) // expected-note {{previous declaration is here}}
#pragma omp declare reduction (fun8 : long : omp_out += omp_in ) initializer(omp_priv=23)) // expected-warning {{extra tokens at the end of '#pragma omp declare reduction' are ignored}} expected-error {{previous declaration with type 'long' is found}} 
#pragma omp declare reduction (fun9 : long : omp_out += omp_in ) initializer(omp_priv=) // expected-error {{expected expression}}

int fun(int arg) {
#pragma omp declare reduction (red : int : omp_out++)
  {
#pragma omp declare reduction (red : int : omp_out++) // expected-note {{previous declaration is here}}
#pragma omp declare reduction (red : int : omp_out++) // expected-error {{previous declaration with type 'int' is found}}
  }
  return arg;
}

