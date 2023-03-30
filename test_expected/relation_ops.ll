; module = main
source_filename = "relation_ops.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ;;; 1 > 3;
    %1 = icmp sgt i32 1, 3
    %2 = zext i1 %1 to i32
    ;;; 3 > 1;
    %3 = icmp sgt i32 3, 1
    %4 = zext i1 %3 to i32
    ;;; 1 < 3;
    %5 = icmp slt i32 1, 3
    %6 = zext i1 %5 to i32
    ;;; 3 < 1;
    %7 = icmp slt i32 3, 1
    %8 = zext i1 %7 to i32
    ;;; 1 <= 3;
    %9 = icmp sle i32 1, 3
    %10 = zext i1 %9 to i32
    ;;; 3 <= 3;
    %11 = icmp sle i32 3, 3
    %12 = zext i1 %11 to i32
    ;;; 3 <= 1;
    %13 = icmp sle i32 3, 1
    %14 = zext i1 %13 to i32
    ;;; 1 >= 3;
    %15 = icmp sge i32 1, 3
    %16 = zext i1 %15 to i32
    ;;; 3 >= 3;
    %17 = icmp sge i32 3, 3
    %18 = zext i1 %17 to i32
    ;;; 3 >= 1;
    %19 = icmp sge i32 3, 1
    %20 = zext i1 %19 to i32
    ;;; 2 == 2;
    %21 = icmp eq i32 2, 2
    %22 = zext i1 %21 to i32
    ;;; 2 == 4;
    %23 = icmp eq i32 2, 4
    %24 = zext i1 %23 to i32
    ;;; 2 != 3;
    %25 = icmp ne i32 2, 3
    %26 = zext i1 %25 to i32
    ;;; 3 != 3;
    %27 = icmp ne i32 3, 3
    %28 = zext i1 %27 to i32
    ret void
}

