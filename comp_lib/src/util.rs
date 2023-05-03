pub trait Ice {
    type Target;

    /// Unwraps `self` or panics with an ICE (Internal Compiler Error), optionally with a message
    /// based on error data contained in `self`.
    fn ice(self) -> Self::Target;
}

impl<T, M: std::fmt::Display> Ice for std::result::Result<T, M> {
    type Target = T;

    fn ice(self) -> T {
        self.map_err(|msg| format!("ICE: {msg}")).unwrap()
    }
}
