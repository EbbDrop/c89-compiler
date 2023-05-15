#[derive(Debug, Clone)]
pub enum Target {
    X86_64,
    Mips,
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Target::X86_64 => "X86_64",
            Target::Mips => "MIPS",
        };
        write!(f, "{name}")
    }
}

#[derive(Debug, Clone)]
pub struct Settings {
    pub target: Target,
}
