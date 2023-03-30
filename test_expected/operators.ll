; module = main
source_filename = "operators.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ;;; 5 + 3;
    %1 = add i32 5, 3
    ;;; 5 - 3;
    %2 = sub i32 5, 3
    ;;; 5 * 3;
    %3 = mul i32 5, 3
    ;;; 5 / 3;
    %4 = sdiv i32 5, 3
    ;;; + 3;
    ;;; - 3;
    %5 = sub i32 0, 3
    ;;; 4 % 3;
    %6 = srem i32 4, 3
    ;;; 4 % 9;
    %7 = srem i32 4, 9
    ;;; 5.2 + 3.8;
    %8 = fadd double 5.2, 3.8
    ;;; 5.2 - 3.8;
    %9 = fsub double 5.2, 3.8
    ;;; 5.2 * 3.8;
    %10 = fmul double 5.2, 3.8
    ;;; 5.2 / 3.8;
    %11 = fdiv double 5.2, 3.8
    ;;; + 3.8;
    ;;; - 3.8;
    %12 = fneg double 3.8
    ret void
}

