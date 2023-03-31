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
    ;;; !0;
    %8 = icmp eq i32 0, 0
    %9 = zext i1 %8 to i32
    ;;; !5;
    %10 = icmp eq i32 0, 5
    %11 = zext i1 %10 to i32
    ;;; 5.2 + 3.8;
    %12 = fadd double 5.2, 3.8
    ;;; 5.2 - 3.8;
    %13 = fsub double 5.2, 3.8
    ;;; 5.2 * 3.8;
    %14 = fmul double 5.2, 3.8
    ;;; 5.2 / 3.8;
    %15 = fdiv double 5.2, 3.8
    ;;; + 3.8;
    ;;; - 3.8;
    %16 = fneg double 3.8
    ;;; !0.0;
    %17 = fcmp oeq double 0, 0
    %18 = zext i1 %17 to i32
    ;;; !5.2;
    %19 = fcmp oeq double 0, 5.2
    %20 = zext i1 %19 to i32
    ret void
}

