use std::fs::remove_file;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn compile_assembly_code(assembly_code: &str, input_file: &Path) -> io::Result<PathBuf> {
    let stem = input_file.file_stem().unwrap();
    let parent_dir = input_file.parent().unwrap();
    let assembly_path = PathBuf::from(format!("{}/{}.asm", parent_dir.display(), stem.display()));
    std::fs::write(&assembly_path, format!("{}\n", assembly_code))?;

    // Derive paths from assembly file
    let object_path = PathBuf::from(format!("{}/{}.o", parent_dir.display(), stem.display()));
    let executable_path = PathBuf::from(format!("{}/{}", parent_dir.display(), stem.display()));

    // Compile and link
    compile_with_as(&assembly_path, &object_path)?;
    link_with_ld(&object_path, &executable_path)?;

    // Remove compiler artifacts
    let _ = remove_file(&assembly_path);
    let _ = remove_file(&object_path);

    Ok(executable_path)
}

fn compile_with_as(assembly_path: &Path, object_path: &Path) -> io::Result<()> {
    let status = Command::new("as")
        .args(["-g", "-o"])
        .arg(object_path)
        .arg(assembly_path)
        .status()?;

    if !status.success() {
        return Err(io::Error::other("Compiling failed"));
    }

    Ok(())
}

fn link_with_ld(object_path: &Path, executable_path: &Path) -> io::Result<()> {
    let status = Command::new("ld")
        .args(["-melf_x86_64", "-o"])
        .arg(executable_path)
        .arg(object_path)
        .status()?;

    if !status.success() {
        return Err(io::Error::other("Linking failed"));
    }

    Ok(())
}
