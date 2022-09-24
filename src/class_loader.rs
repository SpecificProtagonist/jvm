use std::{io::Read, path::PathBuf};

pub trait ClassLoader: Send + Sync {
    /// Get the bytes of the classfile representing the class
    fn load(&self, name: &str) -> Option<Vec<u8>>;
}

/// Class loader that loads classes from a folder, falling back to other folders if
/// the class wasn't found
pub struct DefaultClassLoader {
    pub class_path: Vec<PathBuf>,
}

impl DefaultClassLoader {
    pub fn new_boxed<P: Into<PathBuf>>(paths: impl IntoIterator<Item = P>) -> Box<dyn ClassLoader> {
        Box::new(Self {
            class_path: paths.into_iter().map(Into::into).collect(),
        })
    }
}

impl ClassLoader for DefaultClassLoader {
    fn load(&self, name: &str) -> Option<Vec<u8>> {
        for path in &self.class_path {
            let mut path = path.clone();
            path.push(name);
            path.set_extension("class");

            if path.exists() {
                let mut bytes = Vec::new();
                let error = "Failed to read class. TODO: What error to throw when the class is found but could not be read?";
                std::fs::File::open(path)
                    .expect(error)
                    .read_to_end(&mut bytes)
                    .expect(error);
                return Some(bytes);
            }
        }
        None
    }
}
