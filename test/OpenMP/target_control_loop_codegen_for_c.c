///
/// Perform several control loop code generation tests
/// (code within target region for nvptx)
///

///##############################################
///
/// Empty target region (control loop skeleton)
///
///##############################################

#ifdef TT1
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-linux \
// RUN:   -DTT1 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK1 -input-file=target_control_loop_codegen_for_c.ll.tgt-nvptx64sm_35-nvidia-linux %s

// CK1: @masterLabel = common addrspace(3) global i32 0
// CK1: @othersLabel = common addrspace(3) global i32 0

int foo() {

#pragma omp target
  {
  }

  return 0;
}

// CK1: %[[TID:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK1-NEXT: %[[PMST:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID]], 0
// CK1-NEXT: br i1 %[[PMST]], label %[[INTLB:[a-zA-Z0-9_\.]+]], label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK1: [[INTLB]]:                                     ; preds = %[[ENTLB:[a-zA-Z0-9_\.]+]]
// CK1-NEXT: store i32 1, i32 addrspace(3)* @masterLabel
// CK1-NEXT: store i32 0, i32 addrspace(3)* @othersLabel
// CK1-NEXT: br label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK1: [[FTILB]]:
// CK1-NEXT: call void @llvm.nvvm.barrier0()
// CK1-NEXT: %[[NXTLB:[a-zA-Z0-9_\.]+]] = alloca i32
// CK1-NEXT: %[[NXTLBCST:[a-zA-Z0-9_\.]+]] = addrspacecast i32* %[[NXTLB]] to i32 addrspace(5)*
// CK1-NEXT: %[[FINED:[a-zA-Z0-9_\.]+]] = alloca i1
// CK1-NEXT: %[[FINEDCST:[a-zA-Z0-9_\.]+]] = addrspacecast i1* %[[FINED]] to i1 addrspace(5)*
// CK1-NEXT: store i1 false, i1 addrspace(5)* %[[FINEDCST]]
// CK1-NEXT: br label %[[STRTCTL:[a-zA-Z0-9_\.]+]]

// CK1: [[STRTCTL]]:
// CK1-NEXT: call void @llvm.nvvm.barrier0()
// CK1-NEXT:  %[[TID2:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK1-NEXT: %[[PMST2:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID2]], 0
// CK1-NEXT: br i1 %[[PMST2]], label %[[MSTNXTLB:[a-zA-Z0-9_\.]+]], label %[[OTHSNXTLB:[a-zA-Z0-9_\.]+]]

// CK1: [[MSTNXTLB]]:
// CK1-NEXT: %[[LDMSTLB:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @masterLabel
// CK1-NEXT: store i32 %[[LDMSTLB]], i32 addrspace(5)* %[[NXTLBCST]]
// CK1-NEXT: br label %[[SWITCH:[a-zA-Z0-9_\.]+]]

// CK1: [[OTHSNXTLB]]:
// CK1-NEXT: %[[LDOTHSLBL:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @othersLabel
// CK1-NEXT: store i32 %[[LDOTHSLBL]], i32 addrspace(5)* %[[NXTLBCST]]
// CK1-NEXT: br label %[[SWITCH]]

// CK1: [[SWITCH]]:
// CK1-NEXT: %[[NXTLBLD:[a-zA-Z0-9_\.]+]] = load i32 addrspace(5)* %[[NXTLBCST]]
// CK1-NEXT: switch i32 %[[NXTLBLD]], label %[[CHKFINISHLP:[a-zA-Z0-9_\.]+]] [
// CK1-NEXT: i32 0, label %[[IDLE:[a-zA-Z0-9_\.]+]]
// CK1-NEXT: i32 1, label %[[SQSTRT:[a-zA-Z0-9_\.]+]]
// CK1-NEXT: i32 2, label %[[FINISHBB:[a-zA-Z0-9_\.]+]]
// CK1-NEXT: ]

// CK1: [[SQSTRT]]:
// CK1-NEXT: call void @__kmpc_kernel_init()
// CK1-NEXT: store i32 2, i32 addrspace(3)* @masterLabel
// CK1-NEXT: store i32 2, i32 addrspace(3)* @othersLabel
// CK1-NEXT: br label %[[CHKFINISHLP]]

// CK1: [[IDLE]]:
// CK1-NEXT: br label %[[CHKFINISHLP]]

// CK1: [[FINISHBB]]:
// CK1-NEXT: store i1 true, i1 addrspace(5)* %[[FINEDCST]]
// CK1-NEXT: br label %[[CHKFINISHLP]]

// CK1: [[CHKFINISHLP]]:
// CK1-NEXT: %[[FINIVRLD:[a-zA-Z0-9_\.]+]] = load i1 addrspace(5)* %[[FINEDCST]]
// CK1-NEXT: %[[ISFINI:[a-zA-Z0-9_\.]+]] = icmp eq i1 %[[FINIVRLD]], true
// CK1-NEXT: br i1 %[[ISFINI]], label %[[ENDCTRL:[a-zA-Z0-9_\.]+]], label %[[STRTCTL]]

// CK1: [[ENDCTRL]]:
// CK1-NEXT:  ret void

#endif

///##############################################
///
/// Only one declaration and initialization
///
///##############################################

#ifdef TT2
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-linux \
// RUN:   -DTT2 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK2 -input-file=target_control_loop_codegen_for_c.ll.tgt-nvptx64sm_35-nvidia-linux %s

// CK2: @masterLabel = common addrspace(3) global i32 0
// CK2: @othersLabel = common addrspace(3) global i32 0

int foo() {

#pragma omp target
  {
    int a = 1;
  }

  return 0;
}

// CK2: %[[TID:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK2-NEXT: %[[PMST:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID]], 0
// CK2-NEXT: br i1 %[[PMST]], label %[[INTLB:[a-zA-Z0-9_\.]+]], label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK2: [[INTLB]]:                                     ; preds = %[[ENTLB:[a-zA-Z0-9_\.]+]]
// CK2-NEXT: store i32 1, i32 addrspace(3)* @masterLabel
// CK2-NEXT: store i32 0, i32 addrspace(3)* @othersLabel
// CK2-NEXT: br label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK2: [[FTILB]]:
// CK2-NEXT: call void @llvm.nvvm.barrier0()
// CK2-NEXT: %[[NXTLB:[a-zA-Z0-9_\.]+]] = alloca i32
// CK2-NEXT: %[[NXTLBCST:[a-zA-Z0-9_\.]+]] = addrspacecast i32* %[[NXTLB]] to i32 addrspace(5)*
// CK2-NEXT: %[[FINED:[a-zA-Z0-9_\.]+]] = alloca i1
// CK2-NEXT: %[[FINEDCST:[a-zA-Z0-9_\.]+]] = addrspacecast i1* %[[FINED]] to i1 addrspace(5)*
// CK2-NEXT: store i1 false, i1 addrspace(5)* %[[FINEDCST]]
// CK2-NEXT: br label %[[STRTCTL:[a-zA-Z0-9_\.]+]]

// CK2: [[STRTCTL]]:
// CK2-NEXT: call void @llvm.nvvm.barrier0()
// CK2-NEXT:  %[[TID2:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK2-NEXT: %[[PMST2:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID2]], 0
// CK2-NEXT: br i1 %[[PMST2]], label %[[MSTNXTLB:[a-zA-Z0-9_\.]+]], label %[[OTHSNXTLB:[a-zA-Z0-9_\.]+]]

// CK2: [[MSTNXTLB]]:
// CK2-NEXT: %[[LDMSTLB:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @masterLabel
// CK2-NEXT: store i32 %[[LDMSTLB]], i32 addrspace(5)* %[[NXTLBCST]]
// CK2-NEXT: br label %[[SWITCH:[a-zA-Z0-9_\.]+]]

// CK2: [[OTHSNXTLB]]:
// CK2-NEXT: %[[LDOTHSLBL:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @othersLabel
// CK2-NEXT: store i32 %[[LDOTHSLBL]], i32 addrspace(5)* %[[NXTLBCST]]
// CK2-NEXT: br label %[[SWITCH]]

// CK2: [[SWITCH]]:
// CK2-NEXT: %[[NXTLBLD:[a-zA-Z0-9_\.]+]] = load i32 addrspace(5)* %[[NXTLBCST]]
// CK2-NEXT: switch i32 %[[NXTLBLD]], label %[[CHKFINISHLP:[a-zA-Z0-9_\.]+]] [
// CK2-NEXT: i32 0, label %[[IDLE:[a-zA-Z0-9_\.]+]]
// CK2-NEXT: i32 1, label %[[SQSTRT:[a-zA-Z0-9_\.]+]]
// CK2-NEXT: i32 2, label %[[FINISHBB:[a-zA-Z0-9_\.]+]]
// CK2-NEXT: ]

// CK2: [[SQSTRT]]:
// CK2-NEXT: call void @__kmpc_kernel_init()
// CK2-NEXT: store i32 1, i32* %[[LCLVAR:[a-zA-Z0-9_\.]+]], align 4
// CK2-NEXT: store i32 2, i32 addrspace(3)* @masterLabel
// CK2-NEXT: store i32 2, i32 addrspace(3)* @othersLabel
// CK2-NEXT: br label %[[CHKFINISHLP]]

// CK2: [[IDLE]]:
// CK2-NEXT: br label %[[CHKFINISHLP]]

// CK2: [[FINISHBB]]:
// CK2-NEXT: store i1 true, i1 addrspace(5)* %[[FINEDCST]]
// CK2-NEXT: br label %[[CHKFINISHLP]]

// CK2: [[CHKFINISHLP]]:
// CK2-NEXT: %[[FINIVRLD:[a-zA-Z0-9_\.]+]] = load i1 addrspace(5)* %[[FINEDCST]]
// CK2-NEXT: %[[ISFINI:[a-zA-Z0-9_\.]+]] = icmp eq i1 %[[FINIVRLD]], true
// CK2-NEXT: br i1 %[[ISFINI]], label %[[ENDCTRL:[a-zA-Z0-9_\.]+]], label %[[STRTCTL]]

// CK2: [[ENDCTRL]]:
// CK2-NEXT:  ret void

#endif

///##############################################
///
/// Parallel region
///
///##############################################

#ifdef TT3
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-linux \
// RUN:   -DTT3 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK3 -input-file=target_control_loop_codegen_for_c.ll.tgt-nvptx64sm_35-nvidia-linux %s

// CK3: @masterLabel = common addrspace(3) global i32 0
// CK3: @othersLabel = common addrspace(3) global i32 0
int foo() {
  int b[1024];

  for (int i = 0 ; i < 1024 ; i++)
    b[i] = i;
  
#pragma omp target
  {
    int a = 1;
    
#pragma omp parallel for
    for (int i = 0 ; i < 1024 ; i++)
      b[i] += a;
  }
  
  return b[0];
}

// CK3: %[[TID:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT: %[[PMST:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID]], 0
// CK3-NEXT: br i1 %[[PMST]], label %[[INTLB:[a-zA-Z0-9_\.]+]], label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK3: [[INTLB]]:                                     ; preds = %[[ENTLB:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: store i32 1, i32 addrspace(3)* @masterLabel
// CK3-NEXT: store i32 0, i32 addrspace(3)* @othersLabel
// CK3-NEXT: br label %[[FTILB:[a-zA-Z0-9_\.]+]]

// CK3: [[FTILB]]:
// CK3-NEXT: call void @llvm.nvvm.barrier0()
// CK3-NEXT: %[[NXTLB:[a-zA-Z0-9_\.]+]] = alloca i32
// CK3-NEXT: %[[NXTLBCST:[a-zA-Z0-9_\.]+]] = addrspacecast i32* %[[NXTLB]] to i32 addrspace(5)*
// CK3-NEXT: %[[FINED:[a-zA-Z0-9_\.]+]] = alloca i1
// CK3-NEXT: %[[FINEDCST:[a-zA-Z0-9_\.]+]] = addrspacecast i1* %[[FINED]] to i1 addrspace(5)*
// CK3-NEXT: store i1 false, i1 addrspace(5)* %[[FINEDCST]]
// CK3-NEXT: br label %[[STRTCTL:[a-zA-Z0-9_\.]+]]

// CK3: [[STRTCTL]]:
// CK3-NEXT: call void @llvm.nvvm.barrier0()
// CK3-NEXT:  %[[TID2:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT: %[[PMST2:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID2]], 0
// CK3-NEXT: br i1 %[[PMST2]], label %[[MSTNXTLB:[a-zA-Z0-9_\.]+]], label %[[OTHSNXTLB:[a-zA-Z0-9_\.]+]]

// CK3: [[MSTNXTLB]]:
// CK3-NEXT: %[[LDMSTLB:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @masterLabel
// CK3-NEXT: store i32 %[[LDMSTLB]], i32 addrspace(5)* %[[NXTLBCST]]
// CK3-NEXT: br label %[[SWITCH:[a-zA-Z0-9_\.]+]]

// CK3: [[OTHSNXTLB]]:
// CK3-NEXT: %[[LDOTHSLBL:[a-zA-Z0-9_\.]+]] = load i32 addrspace(3)* @othersLabel
// CK3-NEXT: store i32 %[[LDOTHSLBL]], i32 addrspace(5)* %[[NXTLBCST]]
// CK3-NEXT: br label %[[SWITCH]]

// CK3: [[SWITCH]]:
// CK3-NEXT: %[[NXTLBLD:[a-zA-Z0-9_\.]+]] = load i32 addrspace(5)* %[[NXTLBCST]]
// CK3-NEXT: switch i32 %[[NXTLBLD]], label %[[CHKFINISHLP:[a-zA-Z0-9_\.]+]] [
// CK3-NEXT: i32 0, label %[[IDLE:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: i32 1, label %[[SQSTRT:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: i32 2, label %[[FINISHBB:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: i32 3, label %[[PARBB:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: i32 4, label %[[SEQBB:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: ]

// CK3: [[SQSTRT]]:
// CK3-NEXT: call void @__kmpc_kernel_init()
// CK3-NEXT: store i32 1, i32* {{.*}}, align 4
// CK3-NEXT: {{.*}} = call i32 @__kmpc_kernel_prepare_parallel()
// CK3-NEXT: store i32 3, i32 addrspace(3)* @masterLabel
// CK3-NEXT: store i32 3, i32 addrspace(3)* @othersLabel
// CK3-NEXT: br label %[[CHKFINISHLP]]

// CK3: [[IDLE]]:
// CK3-NEXT: br label %[[CHKFINISHLP]]

// CK3: [[FINISHBB]]:
// CK3-NEXT: store i1 true, i1 addrspace(5)* %[[FINEDCST]]
// CK3-NEXT: br label %[[CHKFINISHLP]]

// CK3: [[CHKFINISHLP]]:
// CK3-NEXT: %[[FINIVRLD:[a-zA-Z0-9_\.]+]] = load i1 addrspace(5)* %[[FINEDCST]]
// CK3-NEXT: %[[ISFINI:[a-zA-Z0-9_\.]+]] = icmp eq i1 %[[FINIVRLD]], true
// CK3-NEXT: br i1 %[[ISFINI]], label %[[ENDCTRL:[a-zA-Z0-9_\.]+]], label %[[STRTCTL]]

// CK3: [[ENDCTRL]]:
// CK3-NEXT:  ret void

// CK3: [[PARBB]]:
// CK3-NEXT: call void @__kmpc_kernel_parallel()
// CK3-NEXT:  %[[NTHS:[a-zA-Z0-9_\.]+]] = call i32 @omp_get_num_threads()
// CK3-NEXT:  %[[TID3:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT:  %[[ISEXC:[a-zA-Z0-9_\.]+]] = icmp uge i32 %[[TID3]], %[[NTHS]]
// CK3-NEXT:  br i1 %[[ISEXC]], label %[[ISEXCBB:[a-zA-Z0-9_\.]+]], label %[[NOTEXCBB:[a-zA-Z0-9_\.]+]]

// CK3: [[ISEXCBB]]:
// CK3-NEXT: br label %[[CHKFINISHLP]]

// CK3: [[NOTEXCBB]]:
// CK3-NEXT: %[[BID:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
// CK3-NEXT: %[[BSZE:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
// CK3-NEXT: %[[TID3:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT: %[[BIDXBSZE:[a-zA-Z0-9_\.]+]] = mul i32 %[[BID]], %[[BSZE]]
// CK3-NEXT: %[[GID:[a-zA-Z0-9_\.]+]] = add i32 %[[BIDXBSZE]], %[[TID3]]

// CK3: store i32 0, i32* %[[LB:[a-zA-Z0-9_\.]+]]
// CK3-NEXT: store i32 1023, i32* %[[UB:[a-zA-Z0-9_\.]+]]
// CK3:  call void @__kmpc_for_static_init_4({ i32, i32, i32, i32, i8* }* {{.*}}, i32 %[[GID]], i32 34, i32* {{.*}}, i32* %[[LB]], i32* %[[UB]], i32* {{.*}}, i32 1, i32 0)
// CK3-NEXT: br label {{.*}}

// CK3: %[[BID2:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
// CK3-NEXT: %[[BSZE2:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
// CK3-NEXT: %[[TID7:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT: %[[BIDXBSZE2:[a-zA-Z0-9_\.]+]] = mul i32 %[[BID2]], %[[BSZE2]]
// CK3-NEXT: %[[GID2:[a-zA-Z0-9_\.]+]] = add i32 %[[BIDXBSZE2]], %[[TID7]]
// CK3-NEXT: call void @__kmpc_for_static_fini({ i32, i32, i32, i32, i8* }* {{.*}}, i32 %[[GID2]])
// CK3-NEXT: call void @llvm.nvvm.barrier0()
// CK3-NEXT: br label %[[PRECENDBB:[a-zA-Z0-9_\.]+]]

// CK3: {{:?}}[[PRECENDBB]]{{:?}}
// CK3-NEXT: call void @__kmpc_kernel_end_parallel()
// CK3-NEXT: %[[TID8:[a-zA-Z0-9_\.]+]] = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
// CK3-NEXT: %[[AMMAST9:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[TID8]], 0
// CK3-NEXT: br i1 %[[AMMAST9]], label %[[NXTLABBB:[a-zA-Z0-9_\.]+]], label %[[FTNLBB:[a-zA-Z0-9_\.]+]]

// CK3: [[SEQBB]]:
// CK3-NEXT: store i32 2, i32 addrspace(3)* @masterLabel
// CK3-NEXT: store i32 2, i32 addrspace(3)* @othersLabel
// CK3-NEXT: br label %[[CHKFINISHLP]]

// CK3: [[NXTLABBB]]:
// CK3-NEXT: store i32 4, i32 addrspace(3)* @masterLabel
// CK3-NEXT: store i32 0, i32 addrspace(3)* @othersLabel
// CK3-NEXT: br label %[[FTNLBB]]

// CK3: [[FTNLBB]]: 
// CK3-NEXT: br label %[[CHKFINISHLP]]
#endif
