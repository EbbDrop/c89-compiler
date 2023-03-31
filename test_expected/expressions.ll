; module = main
source_filename = "expressions.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ;;; 5*(3/10 + 9/10);
    %1 = sdiv i32 3, 10
    %2 = sdiv i32 9, 10
    %3 = add i32 %1, %2
    %4 = mul i32 5, %3
    ;;; 6*2/( 2+1 * 2/3 +6) +8 * (8/4);
    %5 = mul i32 6, 2
    %6 = mul i32 1, 2
    %7 = sdiv i32 %6, 3
    %8 = add i32 2, %7
    %9 = add i32 %8, 6
    %10 = sdiv i32 %5, %9
    %11 = sdiv i32 8, 4
    %12 = mul i32 8, %11
    %13 = add i32 %10, %12
    ;;; (1
    ;;; +
    ;;; 1);
    %14 = add i32 1, 1
    ret void
}

