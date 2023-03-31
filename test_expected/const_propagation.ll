; module = main
source_filename = "const_propagation.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: int a
    %1 = alloca i32
    ; allocation of: int b
    %2 = alloca i32
    ; allocation of: int c
    %3 = alloca i32
    ;;; const int a = 4;
    store i32 4, ptr %1
    ;;; const int b = a + 2;
    %4 = load i32, ptr %1
    %5 = add i32 %4, 2
    store i32 %5, ptr %2
    ;;; const int c = ((b * 23) / 2) % 10;
    %6 = load i32, ptr %2
    %7 = mul i32 %6, 23
    %8 = sdiv i32 %7, 2
    %9 = srem i32 %8, 10
    store i32 %9, ptr %3
    ret void
}

