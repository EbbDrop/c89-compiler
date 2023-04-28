use llvm_ir::*;

#[test]
pub fn it_works() {
    let mut module = Module::new("main".to_owned());
    module.set_source_filename("this\nis\na\ntest   !".to_owned());

    module.define_global_var(
        GlobalVarDefinition::new_constant(constant::Array::new_char_array_from_str(
            "This is a test!\n Foobar~~\0",
        ))
        .with_linkage(Linkage::Private),
    );

    let i32_ty = module.define_type(ty::I32);

    let mut function = FunctionDeclaration::new(i32_ty.clone().into());

    let param_a = function.add_param(i32_ty.clone());
    let param_b = function.add_param(i32_ty.clone());

    let mut function = function.to_definition_builder();

    let bb1 = function.declare_block();
    let bb2 = function.declare_block();

    function
        .terminate_and_start_declared_block(instruction::Branch { dest: bb1.clone() }, bb1.clone())
        .unwrap();

    let cond = function
        .add_instruction(instruction::compare::Int {
            operator: instruction::IcmpCond::Eq,
            operand1: param_a,
            operand2: param_b,
        })
        .unwrap();

    function
        .terminate_and_start_declared_block(
            instruction::BranchConditional {
                cond,
                dest_true: bb1,
                dest_false: bb2.clone(),
            },
            bb2,
        )
        .unwrap();

    module
        .define_function_named("main".into(), function.build())
        .unwrap();

    module
        .declare_function_named(
            "12 24 _this is atest.\n!".into(),
            FunctionDeclaration::new(ReturnType::Void),
        )
        .unwrap();

    assert_eq!(
        format!(
            "{}",
            module.display(FmtOpts {
                display_unnamed_ids: false,
            })
        ),
        r#"; module = main
source_filename = "this\0Ais\0Aa\0Atest   !"

%0 = type i32

@0 = private constant [26 x i8] c"This is a test!\0A Foobar~~\00"

declare void @"12 24 _this is atest.\0A!" ()

define %0 @main (%0, %0) {
    br label %3
    icmp eq %0 %0, %1
    br i1 %4, label %3, label %5
    ret %0 zeroinitializer
}
"#
    );
}
