///
/// Perform several driver tests for OpenMP offloading
///

/// Check whether an invalid OpenMP target is specified:
// RUN:   %clang -### -fopenmp -omptargets=aaa-bbb-ccc-ddd %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-INVALID-TARGET %s
// CHK-INVALID-TARGET: error: OpenMP target is invalid: 'aaa-bbb-ccc-ddd'

/// Check error for empty -omptargets
// RUN:   %clang -### -fopenmp -omptargets=  %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-EMPTY-OMPTARGETS %s
// CHK-EMPTY-OMPTARGETS: warning: joined argument expects additional value: '-omptargets='

/// Check whether we are using a target whose toolchain was not prepared to
/// to support offloading:
// RUN:   %clang -### -fopenmp -omptargets=x86_64-apple-darwin %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-NO-SUPPORT %s
// CHK-NO-SUPPORT: error: Toolchain for target 'x86_64-apple-darwin' is not supporting OpenMP offloading.

/// Target independent check of the commands passed to each tool when using
/// valid OpenMP targets
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-COMMANDS %s
//
// Host commands
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-main-file-path" "[[SRC:[^ ]+]].c"
// CHK-COMMANDS: "-E"
// CHK-COMMANDS: "-o" "[[HOSTPP:.+]].i"
// CHK-COMMANDS: "-x" "c" "[[SRC]].c"
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-main-file-path" "[[SRC:[^ ]+]].c"
// CHK-COMMANDS: "-S"
// CHK-COMMANDS: "-o" "[[HOSTASM:.+]].s"
// CHK-COMMANDS: "-x" "cpp-output" "[[HOSTPP]].i"
// CHK-COMMANDS: as" "-a64" "-mppc64" "-many" "-o" "[[HOSTOBJ:.+]].o" "[[HOSTASM]].s"

// Target1 commands (nvptx uses host preprocessor definitions)
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-main-file-path" "[[SRC]].c" "-triple" "powerpc64--linux" "-E"
// CHK-COMMANDS: "-o" "[[T1PP:.+]].i" "-x" "c" "[[SRC]].c"
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-main-file-path" "[[SRC]].c" "-triple" "nvptx64-nvidia-cuda" "-S"
// CHK-COMMANDS: "-target-cpu" "sm_20"
// CHK-COMMANDS: "-o" "[[T1ASM:.+]].s" "-x" "cpp-output" "[[T1PP]].i"
// CHK-COMMANDS: ptxas" "-o" "[[T1OBJ:.+]].o" "-c" "-arch" "sm_20" "[[T1ASM]].s"
// CHK-COMMANDS: cp" "[[T1OBJ]].o" "[[T1CBIN:.+]].cubin"
// CHK-COMMANDS: nvlink" "-o" "[[T1LIB:.+]].so" "-arch" "sm_20" {{.*}}"[[T1CBIN]].cubin"

// Target2 command
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-main-file-path" "[[SRC]].c" "-triple" "powerpc64-ibm-linux-gnu" "-E"
// CHK-COMMANDS: "-o" "[[T2PP:.+]].i" "-x" "c" "[[SRC]].c"
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-main-file-path" "[[SRC]].c" "-triple" "powerpc64-ibm-linux-gnu" "-S"
// CHK-COMMANDS: "-target-cpu" "ppc64"
// CHK-COMMANDS: "-o" "[[T2ASM:.+]].s" "-x" "cpp-output" "[[T2PP]].i"
// CHK-COMMANDS: as" "-a64" "-mppc64" "-many" "-o" "[[T2OBJ:.+]].o" "[[T2ASM]].s"
// CHK-COMMANDS: ld" "--eh-frame-hdr" "-m" "elf64ppc" "-shared" "-o" "[[T2LIB:.+]].so" {{.*}} "[[T2OBJ]].o"

// Final linking command
// CHK-COMMANDS: ld" {{.*}} "-o" "a.out"  {{.*}}  "[[HOSTOBJ]].o" "-liomp5" "-lomptarget" {{.*}} "-T" "[[LKSCRIPT:.+]].lk"

/// Check frontend require main file name
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-triple" "powerpc64-ibm-linux-gnu" %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MAINFILE %s
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-triple" "nvptx64-nvidia-cuda" %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MAINFILE %s
// CHK-MAINFILE: error: Main-file path is required to generate code for OpenMP target regions. Use -omp-main-file-path 'path'.

/// Check frontend module ID error
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-triple" "powerpc64-ibm-linux-gnu" "-omp-main-file-path" "abcd.efgh" %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MODULEID %s
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-triple" "nvptx64-nvidia-cuda" %s "-omp-main-file-path" "abcd.efgh" 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MODULEID %s
// CHK-MODULEID: error: Unable to generate module ID from input file 'abcd.efgh' for OpenMP target code generation. Make sure the file exists in the file system.

/// Check the subtarget detection
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=nvptx64sm_35-nvidia-cuda %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-SUBTARGET %s
// CHK-SUBTARGET: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=nvptx64sm_35-nvidia-cuda" "-omp-target-mode" "-omp-main-file-path" {{.*}} "-triple" "nvptx64sm_35-nvidia-cuda" "-S"
// CHK-SUBTARGET: "-target-cpu" "sm_35"
// CHK-SUBTARGET: "-o" "[[ASM:.+]].s"
// CHK-SUBTARGET: "-x" "cpp-output" "[[PP:.+]].i"

/// Check the automatic detection of target files
// RUN:   %clang -fopenmp -target powerpc64-linux -omptargets=nvptx64sm_35-nvidia-cuda %s -S 2>&1
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=nvptx64sm_35-nvidia-cuda target_driver.s -Weverything 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-TARGET_REMARK %s
// CHK-TARGET_REMARK: remark: Identified OpenMP target file 'target_driver.s.tgt-nvptx64sm_35-nvidia-cuda' to be used in the 'nvptx64sm_35-nvidia-cuda' toolchain.

/// Check separate compilation
// RUN:   echo ' ' > %t.1.s 
// RUN:   echo ' ' > %t.1.s.tgt-nvptx64sm_35-nvidia-cuda
// RUN:   echo ' ' > %t.2.o
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=nvptx64sm_35-nvidia-cuda %t.1.s %t.2.o 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-SEP-COMPILATION %s
// CHK-SEP-COMPILATION: as" "-a64" "-mppc64" "-many" "-o" "[[HOSTOBJ:.+]].o" "[[HOSTASM:.+]].s"
// CHK-SEP-COMPILATION: ptxas" "-o" "[[TGTOBJ:.+]].o" "-c" "-arch" "sm_35" "[[HOSTASM]].s.tgt-nvptx64sm_35-nvidia-cuda"
// CHK-SEP-COMPILATION: {{.*}}" "[[TGTOBJ]].o" "[[TGTCUBIN:.+]].cubin"
// CHK-SEP-COMPILATION: nvlink" "-o" "[[TGTSO:.+]].so"  {{.*}}"[[TGTCUBIN]].cubin"
// CHK-SEP-COMPILATION: ld" {{.*}}"[[HOSTOBJ]].o" "[[HOSTOBJ2:.+]].o" {{.*}}"-T" "[[LKS:.+]].lk"
