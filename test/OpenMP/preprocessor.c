// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -E -o preprocessor.i %s 2>&1
// RUN:   FileCheck -check-prefix=CHKH -input-file=preprocessor.i %s
// RUN:   FileCheck -check-prefix=CHKT -input-file=preprocessor.i.tgt-nvptx64sm_35-nvidia-cuda %s

// This is to make sure the pragma name is not expanded!
#define omp (0xDEADBEEF)
#define this_should_be_expanded(x) map(tofrom:x)

#define N 2
#define M 1

int foo(int *a, int *b)
{
  //CHKH: omp target map(a[0:2]) map(tofrom:b[0:2*1])
  //CHKT: omp target map(a[0:2]) map(tofrom:b[0:2*1])
  #pragma omp target map(a[0:N]) this_should_be_expanded(b[0:2*M]) 
  {
    ++a[0];
    ++a[1];
    --b[0];
    --b[1];
  }
  
  return 0;
}


