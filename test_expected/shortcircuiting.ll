; module = main
source_filename = "shortcircuiting.c"

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
    ;;; printf(a);
    %2 = load i32, ptr %1
    %3 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %2)
    ;;; (a = 0) && (a = 255);
    store i32 0, ptr %1
    %4 = icmp ne i32 0, 0
    br i1 %4, label %5, label %7
5:
    store i32 255, ptr %1
    %6 = icmp ne i32 0, 255
    br label %7
7:
    %8 = phi i1 [ false, %0 ], [ %6, %5 ]
    %9 = zext i1 %8 to i32
    ; should print 0
    ;;; printf(a);
    %10 = load i32, ptr %1
    %11 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %10)
    ;;; (a = 2) || (a = 255);
    store i32 2, ptr %1
    %12 = icmp ne i32 0, 2
    br i1 %12, label %15, label %13
13:
    store i32 255, ptr %1
    %14 = icmp ne i32 0, 255
    br label %15
15:
    %16 = phi i1 [ true, %7 ], [ %14, %13 ]
    %17 = zext i1 %16 to i32
    ; should print 2
    ;;; printf(a);
    %18 = load i32, ptr %1
    %19 = call i32 (ptr, ...) @printf(ptr @.printf_int, i32 %18)
    ret void
}

