use super::*;

#[test]
fn displays_register_names_correctly() {
    assert_eq!("$zero", format!("{:#}", Reg::ZERO));

    assert_eq!("$at", format!("{:#}", Reg::AT));

    assert_eq!("$v0", format!("{:#}", Reg::V0));
    assert_eq!("$v1", format!("{:#}", Reg::V1));

    assert_eq!("$a0", format!("{:#}", Reg::A0));
    assert_eq!("$a1", format!("{:#}", Reg::A1));
    assert_eq!("$a2", format!("{:#}", Reg::A2));
    assert_eq!("$a3", format!("{:#}", Reg::A3));

    assert_eq!("$t0", format!("{:#}", Reg::T0));
    assert_eq!("$t1", format!("{:#}", Reg::T1));
    assert_eq!("$t2", format!("{:#}", Reg::T2));
    assert_eq!("$t3", format!("{:#}", Reg::T3));
    assert_eq!("$t4", format!("{:#}", Reg::T4));
    assert_eq!("$t5", format!("{:#}", Reg::T5));
    assert_eq!("$t6", format!("{:#}", Reg::T6));
    assert_eq!("$t7", format!("{:#}", Reg::T7));
    assert_eq!("$t8", format!("{:#}", Reg::T8));
    assert_eq!("$t9", format!("{:#}", Reg::T9));

    assert_eq!("$s0", format!("{:#}", Reg::S0));
    assert_eq!("$s1", format!("{:#}", Reg::S1));
    assert_eq!("$s2", format!("{:#}", Reg::S2));
    assert_eq!("$s3", format!("{:#}", Reg::S3));
    assert_eq!("$s4", format!("{:#}", Reg::S4));
    assert_eq!("$s5", format!("{:#}", Reg::S5));
    assert_eq!("$s6", format!("{:#}", Reg::S6));
    assert_eq!("$s7", format!("{:#}", Reg::S7));
    assert_eq!("$fp", format!("{:#}", Reg::S8));

    assert_eq!("$k0", format!("{:#}", Reg::K0));
    assert_eq!("$k1", format!("{:#}", Reg::K1));

    assert_eq!("$gp", format!("{:#}", Reg::GP));
    assert_eq!("$sp", format!("{:#}", Reg::SP));
    assert_eq!("$fp", format!("{:#}", Reg::FP));
    assert_eq!("$ra", format!("{:#}", Reg::RA));

    assert_eq!("$0", format!("{}", Reg::R(0)));
    assert_eq!("$1", format!("{}", Reg::R(1)));
    assert_eq!("$2", format!("{}", Reg::R(2)));
    assert_eq!("$3", format!("{}", Reg::R(3)));
    assert_eq!("$4", format!("{}", Reg::R(4)));
    assert_eq!("$5", format!("{}", Reg::R(5)));
    assert_eq!("$6", format!("{}", Reg::R(6)));
    assert_eq!("$7", format!("{}", Reg::R(7)));
    assert_eq!("$8", format!("{}", Reg::R(8)));
    assert_eq!("$9", format!("{}", Reg::R(9)));
    assert_eq!("$10", format!("{}", Reg::R(10)));
    assert_eq!("$11", format!("{}", Reg::R(11)));
    assert_eq!("$12", format!("{}", Reg::R(12)));
    assert_eq!("$13", format!("{}", Reg::R(13)));
    assert_eq!("$14", format!("{}", Reg::R(14)));
    assert_eq!("$15", format!("{}", Reg::R(15)));
    assert_eq!("$16", format!("{}", Reg::R(16)));
    assert_eq!("$17", format!("{}", Reg::R(17)));
    assert_eq!("$18", format!("{}", Reg::R(18)));
    assert_eq!("$19", format!("{}", Reg::R(19)));
    assert_eq!("$20", format!("{}", Reg::R(20)));
    assert_eq!("$21", format!("{}", Reg::R(21)));
    assert_eq!("$22", format!("{}", Reg::R(22)));
    assert_eq!("$23", format!("{}", Reg::R(23)));
    assert_eq!("$24", format!("{}", Reg::R(24)));
    assert_eq!("$25", format!("{}", Reg::R(25)));
    assert_eq!("$26", format!("{}", Reg::R(26)));
    assert_eq!("$27", format!("{}", Reg::R(27)));
    assert_eq!("$28", format!("{}", Reg::R(28)));
    assert_eq!("$29", format!("{}", Reg::R(29)));
    assert_eq!("$30", format!("{}", Reg::R(30)));
    assert_eq!("$31", format!("{}", Reg::R(31)));
}

#[test]
fn displays_fpu_registers_correctly() {
    assert_eq!("$f0", format!("{}", FReg::F(0)));
    assert_eq!("$f1", format!("{}", FReg::F(1)));
    assert_eq!("$f2", format!("{}", FReg::F(2)));
    assert_eq!("$f3", format!("{}", FReg::F(3)));
    assert_eq!("$f4", format!("{}", FReg::F(4)));
    assert_eq!("$f5", format!("{}", FReg::F(5)));
    assert_eq!("$f6", format!("{}", FReg::F(6)));
    assert_eq!("$f7", format!("{}", FReg::F(7)));
    assert_eq!("$f8", format!("{}", FReg::F(8)));
    assert_eq!("$f9", format!("{}", FReg::F(9)));
    assert_eq!("$f10", format!("{}", FReg::F(10)));
    assert_eq!("$f11", format!("{}", FReg::F(11)));
    assert_eq!("$f12", format!("{}", FReg::F(12)));
    assert_eq!("$f13", format!("{}", FReg::F(13)));
    assert_eq!("$f14", format!("{}", FReg::F(14)));
    assert_eq!("$f15", format!("{}", FReg::F(15)));
    assert_eq!("$f16", format!("{}", FReg::F(16)));
    assert_eq!("$f17", format!("{}", FReg::F(17)));
    assert_eq!("$f18", format!("{}", FReg::F(18)));
    assert_eq!("$f19", format!("{}", FReg::F(19)));
    assert_eq!("$f20", format!("{}", FReg::F(20)));
    assert_eq!("$f21", format!("{}", FReg::F(21)));
    assert_eq!("$f22", format!("{}", FReg::F(22)));
    assert_eq!("$f23", format!("{}", FReg::F(23)));
    assert_eq!("$f24", format!("{}", FReg::F(24)));
    assert_eq!("$f25", format!("{}", FReg::F(25)));
    assert_eq!("$f26", format!("{}", FReg::F(26)));
    assert_eq!("$f27", format!("{}", FReg::F(27)));
    assert_eq!("$f28", format!("{}", FReg::F(28)));
    assert_eq!("$f29", format!("{}", FReg::F(29)));
    assert_eq!("$f30", format!("{}", FReg::F(30)));
    assert_eq!("$f31", format!("{}", FReg::F(31)));
}

#[test]
fn displays_virtual_registers() {
    assert_eq!("@0", format!("{}", Reg::Virtual(0)));
    assert_eq!("@1", format!("{}", Reg::Virtual(1)));
    assert_eq!("@15", format!("{}", Reg::Virtual(15)));
    assert_eq!("@3658", format!("{}", Reg::Virtual(3658)));
}

#[test]
fn displays_virtual_fpu_registers() {
    assert_eq!("@fs0", format!("{}", FReg::VirtualSingle(0)));
    assert_eq!("@fs1", format!("{}", FReg::VirtualSingle(1)));
    assert_eq!("@fs32", format!("{}", FReg::VirtualSingle(32)));
    assert_eq!("@fs211", format!("{}", FReg::VirtualSingle(211)));

    assert_eq!("@fd0", format!("{}", FReg::VirtualDouble(0)));
    assert_eq!("@fd1", format!("{}", FReg::VirtualDouble(1)));
    assert_eq!("@fd32", format!("{}", FReg::VirtualDouble(32)));
    assert_eq!("@fd211", format!("{}", FReg::VirtualDouble(211)));
}

#[test]
#[should_panic = "$32"]
fn displaying_nonexistent_registers_panics() {
    format!("{}", Reg::R(32));
}

#[test]
#[should_panic = "$f32"]
fn displaying_nonexistent_fpu_registers_panics() {
    format!("{}", FReg::F(32));
}
