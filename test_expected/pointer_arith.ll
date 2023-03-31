; module = main
source_filename = "pointer_arith.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: int i
    %1 = alloca i32
    ; allocation of: int * ptri
    %2 = alloca ptr
    ; allocation of: char c
    %3 = alloca i8
    ; allocation of: char * ptrc
    %4 = alloca ptr
    ;;; int i = 4;
    store i32 4, ptr %1
    ;;; int* ptri = &i;
    store ptr %1, ptr %2
    ;;; printf(ptri);
    %5 = load ptr, ptr %2
    %6 = call i32 (ptr, ...) @printf(ptr @.printf_ptr, ptr %5)
    ; moves pointer 4 places (int is 4 bytes)
    ;;; printf(ptri + 1);
    %7 = load ptr, ptr %2
    %8 = sext i32 1 to i64
    %9 = getelementptr i32, ptr %7, i64 %8
    %10 = call i32 (ptr, ...) @printf(ptr @.printf_ptr, ptr %9)
    ;;; char c = 'a';
    store i8 97, ptr %3
    ;;; char* ptrc = &c;
    store ptr %3, ptr %4
    ;;; printf(ptrc);
    %11 = load ptr, ptr %4
    %12 = call i32 (ptr, ...) @printf(ptr @.printf_ptr, ptr %11)
    ; moves pointer one place (char is one byte)
    ;;; printf(ptrc + 1);
    %13 = load ptr, ptr %4
    %14 = sext i32 1 to i64
    %15 = getelementptr i8, ptr %13, i64 %14
    %16 = call i32 (ptr, ...) @printf(ptr @.printf_ptr, ptr %15)
    ret void
}

