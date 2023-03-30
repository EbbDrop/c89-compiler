; module = main
source_filename = "comments.c"

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
    ; Single line coment
    ;;; int a = 4;
    store i32 4, ptr %1
    ;
    ; * Multi line
    ; 
    ;;; int b = 5;
    store i32 5, ptr %2
    ret void
}

