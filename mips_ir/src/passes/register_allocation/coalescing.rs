use crate::AnyReg;

/// `dest` and `src` must have the same tyupe
fn move_(&mut self, dest: AnyReg, src: AnyReg) {
    match dest {
        AnyReg::R(reg) => crate::instr::pseudo::move_(reg, src.try_into().unwrap()),
        AnyReg::F(freg) => self.instructions.push(crate::instr::move_(
            freg.ffmt(),
            freg,
            src.try_into().unwrap(),
        )),
    }
}
