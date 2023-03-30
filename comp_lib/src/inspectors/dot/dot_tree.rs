pub struct DotTree {
    name: String,
    children: Vec<(&'static str, DotTree)>,
}

impl DotTree {
    pub fn new(name: String, children: Vec<(&'static str, DotTree)>) -> Self {
        Self { name, children }
    }

    pub fn new_leaf(name: String) -> Self {
        Self {
            name,
            children: vec![],
        }
    }

    pub fn to_inner_dot(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        next_id: &mut usize,
    ) -> std::fmt::Result {
        let self_id = *next_id;
        writeln!(f, " \"{}\" [label=\"{}\"];", self_id, self.name)?;
        for c in &self.children {
            *next_id += 1;
            writeln!(
                f,
                " \"{}\" -> \"{}\" [label=\"{}\"];",
                self_id, *next_id, c.0
            )?;
            c.1.to_inner_dot(f, next_id)?
        }
        Ok(())
    }
}

impl std::fmt::Display for DotTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph AST {{")?;
        writeln!(f, " ordering=\"out\"")?;
        let mut id = 0;
        self.to_inner_dot(f, &mut id)?;
        writeln!(f, "}}")?;

        Ok(())
    }
}
