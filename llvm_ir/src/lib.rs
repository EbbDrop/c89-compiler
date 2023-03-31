mod function;
mod global_var;
mod instruction;
mod module;
mod value;

pub mod id;
pub mod ty;

pub use function::*;
pub use global_var::*;
pub use instruction::*;
pub use module::*;
pub use value::*;

#[derive(Debug, Clone)]
pub struct Error;

impl From<()> for Error {
    fn from(_: ()) -> Self {
        Self
    }
}

impl From<Error> for () {
    fn from(_: Error) -> Self {}
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn usage_example() {
        let mut module = Module::new(id::name("main"), "main.c".to_owned());
        let sum_function = {
            let mut builder = FunctionDefinitionBuilder::new(
                module.add_global_identifier(id::name("sum")).unwrap(),
                ty::I16.into(),
            );
            let param_a = builder.add_param(ty::I16.into());
            let param_b = builder.add_param(ty::I16.into());
            let mut builder = builder.start_body().start_block();
            let result = builder
                .add_yielding_instruction(
                    RawYieldingInstruction::BinaryOp {
                        op: BinaryOp::Add,
                        op1: param_a,
                        op2: param_b,
                    }
                    .try_into()
                    .unwrap(),
                )
                .unwrap();
            builder
                .terminate_block(RawTerminatorInstruction::Return(result).try_into().unwrap())
                .unwrap()
                .build()
                .unwrap()
        };
        let main_function = {
            let builder = FunctionDefinitionBuilder::new(
                module.add_global_identifier(id::name("main")).unwrap(),
                ty::Void.into(),
            )
            .start_body()
            .start_block_with_label(
                module
                    .add_global_identifier(id::name("entry"))
                    .unwrap()
                    .into(),
            )
            .unwrap();
            builder
                .terminate_block(RawTerminatorInstruction::ReturnVoid.try_into().unwrap())
                .unwrap()
                .build()
                .unwrap()
        };
        module.define_function(sum_function);
        module.define_function(main_function);
        let actual = format!("{module}");
        let _expected_implicit = r#"; module = main
source_filename = "main.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define i16 @sum(i16, i16) {
    add i16 %0, %1
    ret i16 %3
}

define void @main() {
entry:
    ret void
}
"#
        .to_owned();
        let expected_explicit = r#"; module = main
source_filename = "main.c"

@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)

define i16 @sum(i16 %0, i16 %1) {
2:
    %3 = add i16 %0, %1
    ret i16 %3
}

define void @main() {
entry:
    ret void
}
"#
        .to_owned();
        assert_eq!(actual, expected_explicit);
    }
}
