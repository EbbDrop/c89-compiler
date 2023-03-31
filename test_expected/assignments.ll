; module = main
source_filename = "assignments.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: int a
    %1 = alloca i32
    ; allocation of: float b
    %2 = alloca float
    ; allocation of: float c
    %3 = alloca float
    ;;; int a = 4;
    store i32 4, ptr %1
    ;;; float b = 3;
    %4 = sitofp i32 3 to float
    store float %4, ptr %2
    ;;; float c = a + b;
    %5 = load i32, ptr %1
    %6 = sitofp i32 %5 to float
    %7 = load float, ptr %2
    %8 = fadd float %6, %7
    store float %8, ptr %3
    ;;; b = c - a;
    %9 = load float, ptr %3
    %10 = load i32, ptr %1
    %11 = sitofp i32 %10 to float
    %12 = fsub float %9, %11
    store float %12, ptr %2
    ;;; printf(b);
    %13 = load float, ptr %2
    %14 = fpext float %13 to double
    %15 = call i32 (ptr, ...) @printf(ptr @.printf_float, double %14)
    ;;; a = (int)(c = 8);
    %16 = sitofp i32 8 to float
    store float %16, ptr %3
    %17 = fptosi float %16 to i32
    store i32 %17, ptr %1
    ;;; printf(a);
    %18 = load i32, ptr %1
    %19 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %18)
    ;;; printf(c);
    %20 = load float, ptr %3
    %21 = fpext float %20 to double
    %22 = call i32 (ptr, ...) @printf(ptr @.printf_float, double %21)
    ret void
}

