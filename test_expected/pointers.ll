; module = main
source_filename = "pointers.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: int a
    %1 = alloca i32
    ; allocation of: int * ptr
    %2 = alloca ptr
    ; allocation of: int * * ptrptr
    %3 = alloca ptr
    ;;; int a = 4;
    store i32 4, ptr %1
    ;;; int *ptr = &a;
    store ptr %1, ptr %2
    ;;; int **ptrptr = &ptr;
    store ptr %2, ptr %3
    ;;; **ptrptr = 8;
    %4 = load ptr, ptr %3
    %5 = load ptr, ptr %4
    store i32 8, ptr %5
    ;;; printf(a);
    %6 = load i32, ptr %1
    %7 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %6)
    ret void
}

