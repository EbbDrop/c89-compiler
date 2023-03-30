; module = main
source_filename = "casts.c"

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
    ; allocation of: char c
    %3 = alloca i8
    ;;; int a = (int)4.3;
    %4 = fptosi double 4.3 to i32
    store i32 %4, ptr %1
    ;;; float b = (float)a + 0.7;
    %5 = load i32, ptr %1
    %6 = sitofp i32 %5 to float
    %7 = fpext float %6 to double
    %8 = fadd double %7, 0.7
    %9 = fptrunc double %8 to float
    store float %9, ptr %2
    ;;; char c = (char)((float)a + b);
    %10 = load i32, ptr %1
    %11 = sitofp i32 %10 to float
    %12 = load float, ptr %2
    %13 = fadd float %11, %12
    %14 = fptoui float %13 to i8
    store i8 %14, ptr %3
    ret void
}

