; module = main
source_filename = "inc_dec.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: int a
    %1 = alloca i32
    ;;; int a = 3;
    store i32 3, ptr %1
    ;;; printf(a++);
    %2 = load i32, ptr %1
    %3 = add i32 1, %2
    store i32 %3, ptr %1
    %4 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %2)
    ;;; printf(a++);
    %5 = load i32, ptr %1
    %6 = add i32 1, %5
    store i32 %6, ptr %1
    %7 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %5)
    ;;; printf(a--);
    %8 = load i32, ptr %1
    %9 = sub i32 %8, 1
    store i32 %9, ptr %1
    %10 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %8)
    ;;; printf(a--);
    %11 = load i32, ptr %1
    %12 = sub i32 %11, 1
    store i32 %12, ptr %1
    %13 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %11)
    ;;; printf(++a);
    %14 = load i32, ptr %1
    %15 = add i32 1, %14
    store i32 %15, ptr %1
    %16 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %15)
    ;;; printf(++a);
    %17 = load i32, ptr %1
    %18 = add i32 1, %17
    store i32 %18, ptr %1
    %19 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %18)
    ;;; printf(--a);
    %20 = load i32, ptr %1
    %21 = sub i32 %20, 1
    store i32 %21, ptr %1
    %22 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %21)
    ;;; printf(--a);
    %23 = load i32, ptr %1
    %24 = sub i32 %23, 1
    store i32 %24, ptr %1
    %25 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %24)
    ret void
}

