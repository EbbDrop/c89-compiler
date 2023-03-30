use crate::id::{
    GlobalName, GlobalNameId, Id, Label, LabelIdAny, LabelIdResolved, LabelIdUnresolved,
    LabelPlaceholder, LabelRef, LocalId, LocalNameId, Name, UnnamedId,
};
use crate::ty::{self, Type};
use crate::{
    Instruction, IntoTyped, RawTerminatorInstruction, RawYieldingInstruction,
    TerminatorInstruction, TypedValue, YieldingInstruction, YieldlessInstruction,
};
use std::collections::HashSet;
use std::fmt;
use std::ops::Deref;

////////////////////////////////////////////////////////////////////////////////
// FunctionDefinitionBuilder
////////////////////////////////////////////////////////////////////////////////

pub struct FunctionDefinitionBuilder {
    name: GlobalNameId,
    ret_ty: Type,
    params: Vec<Param>,
    named_symbols: HashSet<LocalNameId>,
    counter: usize,
}

impl FunctionDefinitionBuilder {
    pub fn new(name: GlobalName, ret_ty: Type) -> Self {
        Self {
            name: name.0,
            ret_ty,
            params: Vec::new(),
            named_symbols: HashSet::new(),
            counter: 0,
        }
    }

    pub fn add_param(&mut self, ty: Type) -> TypedValue {
        let id = self.next_unnamed_id();
        self.params.push(Param::new(ty, id.clone().into()));
        id.into_typed(ty)
    }

    pub fn add_param_named(&mut self, ty: Type, name: Name) -> crate::Result<TypedValue> {
        let name = LocalNameId::new(name);
        let inserted = self.named_symbols.insert(name.clone());
        if !inserted {
            return Err(crate::Error);
        }
        self.params.push(Param::new(ty, name.clone().into()));
        Ok(name.into_typed(ty))
    }

    pub fn build_with_empty_body(self) -> FunctionDefinition {
        FunctionDefinition {
            name: self.name,
            ret_ty: self.ret_ty,
            params: self.params,
            blocks: Vec::new(),
        }
    }

    pub fn start_body(self) -> FunctionBodyBuilder {
        FunctionBodyBuilder {
            fbuilder: BasicBlockBuilder {
                function: FunctionDefinition {
                    name: self.name,
                    ret_ty: self.ret_ty,
                    params: self.params,
                    blocks: Vec::new(),
                },
                named_symbols: self.named_symbols,
                crnt_block_label: None,
                crnt_block_body: Vec::new(),
                crnt_block_comments: Vec::new(),
                placeholder_resolutions: Vec::new(),
                counter: self.counter,
            },
        }
    }

    fn next_unnamed_id(&mut self) -> UnnamedId {
        let n = self.counter;
        self.counter += 1;
        UnnamedId::new(n)
    }
}

////////////////////////////////////////////////////////////////////////////////
// BasicBlockBuilder
////////////////////////////////////////////////////////////////////////////////

// A `BasicBlockBuilder` will only be accessible from the outside if `crnt_block_label.is_some()`.
pub struct BasicBlockBuilder {
    function: FunctionDefinition,
    named_symbols: HashSet<LocalNameId>,
    crnt_block_label: Option<LabelIdResolved>,
    crnt_block_body: Vec<Instruction>,
    /// Vec of (index, comment) pairs, where the index denote the location in the body where the
    /// comment should be inserted. This can be equal to `crnt_block_body.len()`, in which case the
    /// comment should be inserted after the last instruction in the body (but before the terminator
    /// instruction). The indices are guaranteed to be increasing (i.e. in ascending order).
    crnt_block_comments: Vec<(usize, String)>,
    /// Maps a `LabelPlaceholder` index to its resolved local `UnnamedId`.
    placeholder_resolutions: Vec<Option<UnnamedId>>,
    counter: usize,
}

impl BasicBlockBuilder {
    /// Returns a [`LabelRef`] to the current block.
    pub fn current_block_label(&self) -> LabelRef {
        LabelRef(self.crnt_block_label.clone().unwrap().into())
    }

    pub fn create_label_placeholder(&mut self) -> LabelPlaceholder {
        let n = self.placeholder_resolutions.len();
        self.placeholder_resolutions.push(None);
        LabelPlaceholder(n)
    }

    pub fn add_comment(&mut self, comment: String) {
        self.crnt_block_comments
            .push((self.crnt_block_body.len(), comment));
    }

    pub fn add_instruction_yielding_to_global(
        &mut self,
        result: GlobalName,
        instruction: YieldingInstruction,
    ) -> crate::Result<TypedValue> {
        let id = result.0.into();
        self.add_instruction_yielding_to_id(id, instruction)
    }

    pub fn add_instruction_yielding_to_local(
        &mut self,
        result: Name,
        instruction: YieldingInstruction,
    ) -> crate::Result<TypedValue> {
        self.add_local_name(result)
            .and_then(|id| self.add_instruction_yielding_to_id(id.into(), instruction))
    }

    pub fn add_yielding_instruction(
        &mut self,
        instruction: YieldingInstruction,
    ) -> crate::Result<TypedValue> {
        let id = self.next_unnamed_id();
        self.add_instruction_yielding_to_id(id.into(), instruction)
    }

    fn add_instruction_yielding_to_id(
        &mut self,
        result: Id,
        instruction: YieldingInstruction,
    ) -> crate::Result<TypedValue> {
        // Phi instructions can only appear at the beginning of a basic block.
        if let RawYieldingInstruction::Phi { .. } = instruction.deref() {
            if let Some(instr) = self.crnt_block_body.last() {
                match instr {
                    Instruction::Yielding(_, instr) => match instr.deref() {
                        RawYieldingInstruction::Phi { .. } => {}
                        _ => return Err(crate::Error),
                    },
                    _ => return Err(crate::Error),
                }
            }
        }
        let ty = instruction.result_ty();
        self.crnt_block_body
            .push(Instruction::Yielding(result.clone(), instruction));
        Ok(result.into_typed(ty))
    }

    pub fn add_yieldless_instruction(&mut self, instruction: YieldlessInstruction) {
        self.crnt_block_body
            .push(Instruction::Yieldless(instruction));
    }

    pub fn terminate_block(
        mut self,
        terminator: TerminatorInstruction,
    ) -> crate::Result<FunctionBodyBuilder> {
        let is_valid_terminator = match terminator.deref() {
            RawTerminatorInstruction::ReturnVoid => self.function.ret_ty == ty::Void.into(),
            RawTerminatorInstruction::Return(tyval) => self.function.ret_ty == tyval.ty,
            RawTerminatorInstruction::BranchUnconditional(_)
            | RawTerminatorInstruction::BranchConditional { .. } => true,
        };
        if !is_valid_terminator {
            return Err(crate::Error);
        }
        let mut block = BasicBlock {
            label: self.crnt_block_label.take().unwrap(),
            body: Vec::new(),
            terminator,
            comments: Vec::new(),
        };
        block.body.append(&mut self.crnt_block_body);
        block.comments.append(&mut self.crnt_block_comments);
        self.function.blocks.push(block);
        Ok(FunctionBodyBuilder { fbuilder: self })
    }

    fn next_unnamed_id(&mut self) -> UnnamedId {
        let n = self.counter;
        self.counter += 1;
        UnnamedId::new(n)
    }

    fn add_local_name(&mut self, name: Name) -> crate::Result<LocalNameId> {
        let id = LocalNameId::new(name);
        let inserted = self.named_symbols.insert(id.clone());
        match inserted {
            true => Ok(id),
            false => Err(crate::Error),
        }
    }

    fn resolve_placeholders(&mut self) {
        let resolve_label = |label: &mut LabelRef| {
            if let LabelIdAny::Placeholder(LabelPlaceholder(p)) = label.0 {
                if let Some(id) = self.placeholder_resolutions.get(p).unwrap() {
                    label.0 = LabelIdAny::Unnamed(id.clone());
                }
            }
        };
        for block in &mut self.function.blocks {
            for instruction in &mut block.body {
                instruction.for_each_label(resolve_label);
            }
            block.terminator.for_each_label(resolve_label);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// FunctionBodyBuilder
////////////////////////////////////////////////////////////////////////////////

pub struct FunctionBodyBuilder {
    fbuilder: BasicBlockBuilder,
}

impl FunctionBodyBuilder {
    pub fn start_block(mut self) -> BasicBlockBuilder {
        let id = self.fbuilder.next_unnamed_id();
        self.fbuilder.crnt_block_label = Some(id.into());
        self.fbuilder
    }

    /// # Errors
    ///
    /// Returns `Err(())` in case the label is a placeholder that has previoulsy been used to start
    /// a block in this function.
    pub fn start_block_with_label(mut self, label: Label) -> crate::Result<BasicBlockBuilder> {
        let resolved_label: LabelIdResolved = match label.0 {
            LabelIdUnresolved::GlobalName(id) => id.into(),
            LabelIdUnresolved::Placeholder(LabelPlaceholder(placeholder)) => {
                match self
                    .fbuilder
                    .placeholder_resolutions
                    .get(placeholder)
                    .unwrap()
                {
                    Some(_) => {
                        // If this point is reached, the placeholder has previously been used to
                        // start another block in this function. Re-using this placeholder is not
                        // allowed.
                        return Err(crate::Error);
                    }
                    None => {
                        let id = self.fbuilder.next_unnamed_id();
                        self.fbuilder.placeholder_resolutions[placeholder] = Some(id.clone());
                        self.fbuilder.resolve_placeholders();
                        id.into()
                    }
                }
            }
        };
        self.fbuilder.crnt_block_label = Some(resolved_label);
        Ok(self.fbuilder)
    }

    /// Builds a [`FunctionDefinition`].
    ///
    /// # Errors
    ///
    /// Returns `Err(())` in the following cases:
    ///
    /// - Not all placeholders were resolved. This happens when instructions (s.a. branches) were
    ///   added that reference a label for an unexisting block, and meanwhile no block with that
    ///   label has been added.
    //
    // This scenario is no longer a possibility:
    //
    // - The current block is not terminated. This is a bit more strict than necessary; in reality
    //   it would only be erroneous if there's a placeholder in one of the previous blocks that was
    //   resolved by starting this unterminated block. By not terminating this block, the block
    //   won't be a port of the built function definition, and the symbol to which the placeholder
    //   was resolved won't exist.
    pub fn build(self) -> crate::Result<FunctionDefinition> {
        // Check all placeholders are resolved.
        match self
            .fbuilder
            .placeholder_resolutions
            .iter()
            .all(Option::is_some)
        {
            true => Ok(self.fbuilder.function),
            false => Err(crate::Error),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// FunctionDefinition
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct FunctionDefinition {
    name: GlobalNameId,
    ret_ty: Type,
    params: Vec<Param>,
    blocks: Vec<BasicBlock>,
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name, ret_ty, .. } = self;
        write!(f, "define {ret_ty} @{name}(")?;
        {
            let mut iter = self.params.iter();
            if let Some(param) = iter.next() {
                write!(f, "{param}")?;
            }
            for param in iter {
                write!(f, ", {param}")?;
            }
        }
        f.write_str(") {\n")?;
        for block in &self.blocks {
            writeln!(f, "{block}")?;
        }
        f.write_str("}")
    }
}

////////////////////////////////////////////////////////////////////////////////
// Param
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct Param {
    name: LocalId,
    ty: Type,
}

impl Param {
    fn new(ty: Type, name: LocalId) -> Self {
        Self { ty, name }
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}

////////////////////////////////////////////////////////////////////////////////
// BasicBlock
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct BasicBlock {
    label: LabelIdResolved,
    body: Vec<Instruction>,
    terminator: TerminatorInstruction,
    // See BasicBlockBuilder.comments
    comments: Vec<(usize, String)>,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.label {
            LabelIdResolved::GlobalName(global_id) => writeln!(f, "{global_id}:")?,
            LabelIdResolved::Unnamed(n) => writeln!(f, "{n}:")?,
        }
        let mut comments_iter = self.comments.iter();
        let mut next_comment = comments_iter.next();
        for (i, instr) in self.body.iter().enumerate() {
            while let Some((j, comment)) = next_comment {
                if i == *j {
                    for line in comment.lines() {
                        writeln!(f, "    ;{line}")?;
                    }
                    next_comment = comments_iter.next();
                } else {
                    break;
                }
            }
            writeln!(f, "    {instr}")?;
        }
        write!(f, "    {}", self.terminator)
    }
}
