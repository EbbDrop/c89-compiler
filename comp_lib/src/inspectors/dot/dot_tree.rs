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

    pub fn to_inner_dot(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " \"{:p}\" [label=\"{}\"];", self, self.name)?;
        for c in &self.children {
            writeln!(f, " \"{:p}\" -> \"{:p}\" [label=\"{}\"];", self, &c.1, c.0)?;
            c.1.to_inner_dot(f)?
        }
        Ok(())
    }
}

impl std::fmt::Display for DotTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph AST {{")?;
        writeln!(f, " ordering=\"out\"")?;
        self.to_inner_dot(f)?;
        writeln!(f, "}}")?;

        Ok(())
    }
}
