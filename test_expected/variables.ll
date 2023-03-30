; module = main
source_filename = "variables.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define void @main() {
0:
    ; allocation of: float foo
    %1 = alloca float
    ; allocation of: int bar
    %2 = alloca i32
    ; allocation of: char UPPER
    %3 = alloca i8
    ; allocation of: float c_foo
    %4 = alloca float
    ; allocation of: int c_bar
    %5 = alloca i32
    ; allocation of: char C_UPPER
    %6 = alloca i8
    ;;; float foo = 3.4;
    %7 = fptrunc double 3.4 to float
    store float %7, ptr %1
    ;;; int bar = 3;
    store i32 3, ptr %2
    ;;; char UPPER = 'b';
    store i8 98, ptr %3
    ;;; const float c_foo = 5.;
    %8 = fptrunc double 5 to float
    store float %8, ptr %4
    ;;; const int c_bar = 0xff;
    store i32 255, ptr %5
    ;;; const char C_UPPER = '\x20';
    store i8 32, ptr %6
    ret void
}

