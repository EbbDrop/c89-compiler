mod expr_generator;
mod function_generator;
mod mips_value;
mod util;

use crate::ir::{self, ctype};
use function_generator::FunctionGenerator;
use mips_ir as mir;
use std::{collections::HashMap, unreachable};

pub struct Generator<'i, 's> {
    root: mir::Root,
    ir: &'i ir::Root,
    source: &'s str,
    float_constants: HashMap<u32, mir::Label>,
    double_constants: HashMap<u64, mir::Label>,
    string_constants: HashMap<Vec<u8>, mir::Label>,
}

impl<'i, 's> Generator<'i, 's> {
    pub fn new(ir: &'i ir::Root, source: &'s str) -> Self {
        Self {
            root: mir::Root::new(),
            ir,
            source,
            float_constants: HashMap::new(),
            double_constants: HashMap::new(),
            string_constants: HashMap::new(),
        }
    }

    pub fn generate(mut self) -> mir::Root {
        self.add_global_vars();
        self.add_functions();
        self.root
    }

    fn add_global_vars(&mut self) {
        let mut all_data = Vec::new();
        for (ident, global_var) in &self.ir.vars {
            // TODO: add comments
            all_data.push(self.compile_ir_global_var(ident, global_var));
        }
        // TODO: use a better space-optimizing algorithm here, instead of ordering by reverse align.
        all_data.sort_unstable_by_key(|d| d.align());
        for data in all_data.into_iter().rev() {
            self.root.add_data(data);
        }
    }

    fn add_functions(&mut self) {
        for (ident, function) in &self.ir.functions {
            if function.is_declaration() {
                self.root.create_external_label(ident);
                continue;
            }
            let function = FunctionGenerator::new(self, ident, function).generate();
            let label = function.label().clone();
            self.root.add_function(function);
            if ident == "main" {
                self.root.export_label(label);
            }
        }
    }

    fn add_float_constant(&mut self, value: f32) -> &mir::Label {
        // NOTE: Rust's and MIPS's representation of signalingness of NaN's differ, but this doesn't
        // matter since it is not possible to create NaN constants.
        let bits = value.to_bits();
        self.float_constants.entry(bits).or_insert_with(|| {
            let label = mir::Label::from(format!("$.const.f32.{bits}"));
            self.root.add_data(mir::GlobalData::new(
                label.clone(),
                mir::DataDirective::Float(value),
            ));
            label
        })
    }

    fn add_double_constant(&mut self, value: f64) -> &mir::Label {
        // NOTE: Rust's and MIPS's representation of signalingness of NaN's differ, but this doesn't
        // matter since it is not possible to create NaN constants.
        let bits = value.to_bits();
        self.double_constants.entry(bits).or_insert_with(|| {
            let label = mir::Label::from(format!("$.const.f64.{bits}"));
            self.root.add_data(mir::GlobalData::new(
                label.clone(),
                mir::DataDirective::Double(value),
            ));
            label
        })
    }

    /// Does not add a null byte.
    fn add_string_constant(&mut self, data: &[u8]) -> &mir::Label {
        let uid = self.string_constants.len();
        self.string_constants
            .entry(data.to_vec())
            .or_insert_with_key(|data| {
                let label = mir::Label::from(format!("$.const.str.{uid}"));
                // SPIM has a bug where zero escapes imidiatly end the string, so have
                // to use bytes
                // Strings will always have atleast a nullbyte so this will never fail
                // IDEA: Add comment of original string
                let data = mir::DataDirective::Bytes(data.clone().try_into().unwrap());
                self.root
                    .add_data(mir::GlobalData::new(label.clone(), data));
                label
            })
    }

    fn compile_ir_global_var(
        &mut self,
        ident: &str,
        global_var: &ir::GlobalVarNode,
    ) -> mir::GlobalData {
        let props = util::ctype_props(&global_var.ty);
        let data = match &global_var.value {
            Some(value) => match value {
                ir::Constant::Integer(value) => match global_var.ty {
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(
                        ctype::Arithmetic::Char
                        | ctype::Arithmetic::SignedChar
                        | ctype::Arithmetic::UnsignedChar,
                    )) => mir::DataDirective::Byte(*value as u8),
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(
                        ctype::Arithmetic::SignedShortInt | ctype::Arithmetic::UnsignedShortInt,
                    )) => mir::DataDirective::Half(*value as u16),
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(
                        ctype::Arithmetic::SignedInt
                        | ctype::Arithmetic::UnsignedInt
                        | ctype::Arithmetic::SignedLongInt
                        | ctype::Arithmetic::UnsignedLongInt,
                    )) => mir::DataDirective::Word(*value as u32),
                    _ => unreachable!(),
                },
                ir::Constant::Float(value) => match global_var.ty {
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::Float)) => {
                        mir::DataDirective::Float(*value as f32)
                    }
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(
                        ctype::Arithmetic::Double | ctype::Arithmetic::LongDouble,
                    )) => mir::DataDirective::Double(*value),
                    _ => unreachable!(),
                },
                ir::Constant::String(value) => {
                    let label = self.add_string_constant(value);
                    mir::DataDirective::LabelWord(label.clone())
                }
            },
            None => mir::DataDirective::Space(props.size),
        };
        mir::GlobalData::new(ident.into(), data).with_align(Some(props.alignment))
    }
}
