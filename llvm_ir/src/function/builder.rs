use super::{FunctionDeclaration, FunctionDefinition, LocalIdHandle, ReturnType};
use crate::convert::{Into_, TryInto_};
use crate::instruction::{
    Instruction, IntoValidated, MaybeYielding, MaybeYieldingInstruction, Terminator,
    VoidInstruction, Yielding, YieldingInstruction,
};
use crate::{instruction, value};
use crate::{BasicBlock, Name};

pub struct FunctionDefinitionBuilder {
    function: FunctionDefinition,
    block_handle: Option<LocalIdHandle>,
    block_comment: Option<String>,
    pending_instructions: Vec<instruction::Instruction>,
    pending_comments: Vec<(usize, String)>,
    declared_block_handles: Vec<LocalIdHandle>,
}

impl FunctionDefinitionBuilder {
    pub(super) fn new(declaration: FunctionDeclaration) -> Self {
        Self {
            function: FunctionDefinition::new(declaration),
            block_handle: None,
            block_comment: None,
            pending_instructions: Vec::new(),
            pending_comments: Vec::new(),
            declared_block_handles: Vec::new(),
        }
    }

    pub fn block_label(&self) -> Option<value::constant::Label> {
        self.block_handle
            .map(value::constant::Label::with_literal_type)
    }

    /// If a label has already been set for the current block, it will be returned. This label
    /// might be named.
    /// Otherwise, a new unnamed label is created.
    pub fn get_or_set_block_label(&mut self) -> value::constant::Label {
        match self.block_handle {
            Some(bh) => value::constant::Label::with_literal_type(bh),
            None => {
                let bh = self.function.declaration.create_unnamed_id_handle();
                self.block_handle = Some(bh);
                value::constant::Label::with_literal_type(bh)
            }
        }
    }

    /// If a label has already been set for the current block, it will be returned. This label
    /// might be unnamed.
    /// Otherwise, the given name is used to create a new named label.
    pub fn get_or_set_block_label_named(
        &mut self,
        name: Name,
    ) -> Result<value::constant::Label, String> {
        match self.block_handle {
            Some(bh) => Ok(value::constant::Label::with_literal_type(bh)),
            None => {
                let bh = self.function.declaration.create_named_id_handle(name)?;
                self.block_handle = Some(bh);
                Ok(value::constant::Label::with_literal_type(bh))
            }
        }
    }

    pub fn block_comment(&self) -> &Option<String> {
        &self.block_comment
    }

    pub fn block_comment_mut(&mut self) -> &mut Option<String> {
        &mut self.block_comment
    }

    pub fn declare_block(&mut self) -> value::constant::Label {
        let handle = self.function.declaration.create_unnamed_id_handle();
        self.declared_block_handles.push(handle);
        value::constant::Label::with_literal_type(handle)
    }

    pub fn declare_block_named(&mut self, name: Name) -> Result<value::constant::Label, String> {
        let handle = self.function.declaration.create_named_id_handle(name)?;
        self.declared_block_handles.push(handle);
        Ok(value::constant::Label::with_literal_type(handle))
    }

    // will panic if the label was not from this builder
    pub fn start_block(&mut self, label: value::constant::Label) {
        if !self.is_block_terminated() {
            self.terminate_block(self.default_terminator()).unwrap();
        }
        self.declared_block_handles.remove(
            self.declared_block_handles
                .iter()
                .position(|handle| *handle == label.handle)
                .unwrap(),
        );
        self.block_handle = Some(label.handle);
    }

    pub fn add_comment(&mut self, comment: String) {
        self.pending_comments
            .push((self.pending_instructions.len(), comment));
    }

    pub fn add_instruction<T: IntoValidated<YieldingInstruction> + Yielding>(
        &mut self,
        instruction: T,
    ) -> Result<value::Register<T::YieldTy>, String> {
        let id = self.function.declaration.create_unnamed_id_handle();
        self.add_instruction_yielding_to_id(id, instruction)
    }

    pub fn add_instruction_named<T: IntoValidated<YieldingInstruction> + Yielding>(
        &mut self,
        name: Name,
        instruction: T,
    ) -> Result<value::Register<T::YieldTy>, String> {
        let id = self
            .function
            .declaration
            .create_named_id_handle(name)
            .map_err(|name| format!("local name `{name:?}` shouldn't already exist"))?;
        self.add_instruction_yielding_to_id(id, instruction)
    }

    fn add_instruction_yielding_to_id<T: IntoValidated<YieldingInstruction> + Yielding>(
        &mut self,
        id: LocalIdHandle,
        instruction: T,
    ) -> Result<value::Register<T::YieldTy>, String> {
        let register = value::Register {
            ty: instruction.yield_ty(),
            handle: id,
        };
        let instruction = instruction.try_into_()?;
        if matches!(instruction, YieldingInstruction::ValidatedPhi(_))
            && self.pending_instructions.iter().any(|instr| {
                !matches!(
                    instr,
                    Instruction::Yielding(_, YieldingInstruction::ValidatedPhi(_))
                )
            })
        {
            return Err("phi instructions can only appear at the beginning of a basic block")?;
        }
        self.pending_instructions
            .push(Instruction::Yielding(register.clone().into_(), instruction));
        Ok(register)
    }

    pub fn add_maybe_yielding_instruction<
        T: IntoValidated<MaybeYieldingInstruction> + MaybeYielding,
    >(
        &mut self,
        instruction: T,
    ) -> Result<Option<value::Register<T::YieldTy>>, String> {
        let result = match instruction.yield_ty() {
            Some(ty) => {
                let id = self.function.declaration.create_unnamed_id_handle();
                let register = value::Register { ty, handle: id };
                Some(register)
            }
            None => None,
        };
        self.pending_instructions.push(Instruction::MaybeYielding(
            result.as_ref().cloned().map(Into_::into_),
            instruction.into_validated()?.into(),
        ));
        Ok(result)
    }

    pub fn add_maybe_yielding_instruction_named<
        T: IntoValidated<MaybeYieldingInstruction> + MaybeYielding,
    >(
        &mut self,
        name: Name,
        instruction: T,
    ) -> Result<Option<value::Register<T::YieldTy>>, String> {
        let result = match instruction.yield_ty() {
            Some(ty) => {
                let id = self.function.declaration.create_named_id_handle(name)?;
                let register = value::Register { ty, handle: id };
                Some(register)
            }
            None => None,
        };
        self.pending_instructions.push(Instruction::MaybeYielding(
            result.as_ref().cloned().map(Into_::into_),
            instruction.into_validated()?.into(),
        ));
        Ok(result)
    }

    pub fn add_void_instruction<T: IntoValidated<VoidInstruction>>(
        &mut self,
        instruction: T,
    ) -> Result<(), String> {
        self.pending_instructions
            .push(Instruction::Void(instruction.into_validated()?.into()));
        Ok(())
    }

    pub fn terminate_block<T: IntoValidated<Terminator>>(
        &mut self,
        terminator: T,
    ) -> Result<(), String> {
        let mut bb = BasicBlock {
            label: value::constant::Label::with_literal_type(
                self.block_handle
                    .take()
                    .unwrap_or_else(|| self.function.declaration.create_unnamed_id_handle()),
            ),
            comment: self.block_comment.take(),
            instructions: Vec::new(),
            comments: Vec::new(),
            terminator: terminator.into_validated()?.into(),
        };
        bb.instructions.append(&mut self.pending_instructions);
        bb.comments.append(&mut self.pending_comments);
        self.function.push_block(bb);
        Ok(())
    }

    /// Convenience method to terminate with an unconditional branch instruction.
    pub fn terminate_block_with_branch_to(
        &mut self,
        dest: impl value::LabelValue,
    ) -> Result<(), String> {
        self.terminate_block(instruction::Branch { dest })
    }

    /// Returns `true` if the current block (if any) doesn't have any instructions yet.
    pub fn is_block_empty(&self) -> bool {
        self.pending_instructions.is_empty()
    }

    /// Returns `true` if the last block was terminated and meanwhile no new block was started.
    /// Note that calling [`get_or_set_block_label`] also starts a new block if there's currently
    /// no active block.
    /// Comments are ignored for this purpose.
    pub fn is_block_terminated(&self) -> bool {
        self.block_handle.is_none() && self.pending_instructions.is_empty()
    }

    pub fn try_build(self) -> Result<FunctionDefinition, String> {
        if self.block_handle.is_none()
            && self.pending_instructions.is_empty()
            && self.declared_block_handles.is_empty()
            && !self.function.body.is_empty()
        {
            Ok(self.function)
        } else {
            Err("cannot build function definiton: function body contains unfinished basic blocks")?
        }
    }

    // This will add a `ret zeroinitializer` instruction to every unfinished block.
    // This will discard comments that are the only element in an unfinished block.
    pub fn build(mut self) -> FunctionDefinition {
        let default_terminator = self.default_terminator();
        if !self.is_block_terminated() {
            self.terminate_block(default_terminator.clone()).unwrap();
        }
        {
            let mut remaining_handles = Vec::new();
            remaining_handles.append(&mut self.declared_block_handles);
            for handle in remaining_handles {
                self.block_handle = Some(handle);
                self.terminate_block(default_terminator.clone()).unwrap();
            }
        }
        if self.function.body.is_empty() {
            self.terminate_block(default_terminator).unwrap();
        }
        self.function
    }

    fn default_terminator(&self) -> instruction::Terminator {
        match &self.function.return_type {
            ReturnType::Void => instruction::ReturnVoid.try_into_().unwrap(),
            ReturnType::Element(ety) => instruction::Return::<value::Element>(
                value::constant::ZeroInitializer(ety.clone()).into(),
            )
            .try_into_()
            .unwrap(),
        }
    }
}
