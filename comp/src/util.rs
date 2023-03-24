use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum PathOrStd {
    Path(PathBuf),
    StdStream,
}

impl From<&std::ffi::OsStr> for PathOrStd {
    fn from(value: &std::ffi::OsStr) -> Self {
        if value == "-" {
            Self::StdStream
        } else {
            Self::Path(value.into())
        }
    }
}
