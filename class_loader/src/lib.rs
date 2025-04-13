use std::{
    ffi::OsStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use vm::ClassLoader;
use zip::ZipArchive;

pub fn file_loaders<P: AsRef<Path>>(paths: impl IntoIterator<Item = P>) -> Box<dyn ClassLoader> {
    Box::new(ChainedClassLoader {
        loaders: paths
            .into_iter()
            .map(|path| {
                Box::new(FileClassLoader {
                    path: path.as_ref().into(),
                }) as Box<dyn ClassLoader>
            })
            .collect(),
    })
}

/// Tries several class loaders in order.
pub struct ChainedClassLoader {
    pub loaders: Vec<Box<dyn ClassLoader>>,
}

impl ClassLoader for ChainedClassLoader {
    fn load(&self, name: &str) -> Option<Vec<u8>> {
        self.loaders.iter().find_map(|loader| loader.load(name))
    }
}

/// Class loader that loads classes from a folder or jar.
/// Does not respect META_INF.
pub struct FileClassLoader {
    pub path: PathBuf,
}

impl ClassLoader for FileClassLoader {
    fn load(&self, name: &str) -> Option<Vec<u8>> {
        let name = name.replace('.', "/") + ".class";
        if self.path.extension() == Some(OsStr::new("jar")) {
            // TODO: Cache zip lookup
            let jar_file = File::open(&self.path).ok()?;
            let mut jar = ZipArchive::new(jar_file).ok()?;
            let mut class_file = jar.by_name(&name).ok()?;
            let mut bytes = Vec::new();
            class_file.read_to_end(&mut bytes).ok()?;
            Some(bytes)
        } else if self.path.is_dir() {
            let mut bytes = Vec::new();
            let mut file = std::fs::File::open(self.path.join(name)).ok()?;
            file.read_to_end(&mut bytes).ok()?;
            Some(bytes)
        } else {
            None
        }
    }
}
