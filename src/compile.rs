use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn compile_assembly_code(asm_path: &Path) -> io::Result<()> {
    // Derive paths from assembly file
    let stem = asm_path
        .file_stem()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid assembly file name"))?;
    let obj_path = PathBuf::from(format!("{}.o", stem.to_string_lossy()));
    let exe_path = PathBuf::from(format!("{}", stem.to_string_lossy()));

    // Compile and link
    compile_with_as(asm_path, &obj_path)?;
    link_with_ld(&obj_path, &exe_path)?;

    Ok(())
}

fn compile_with_as(asm_path: &Path, obj_path: &Path) -> io::Result<()> {
    let status = Command::new("as")
        .args(["-g", "-o"])
        .arg(obj_path)
        .arg(asm_path)
        .status()?;

    if !status.success() {
        return Err(io::Error::other("Compiling failed"));
    }

    Ok(())
}

fn link_with_ld(obj_path: &Path, exe_path: &Path) -> io::Result<()> {
    let status = Command::new("ld")
        .args(["-melf_x86_64", "-o"])
        .arg(exe_path)
        .arg(obj_path)
        .status()?;

    if !status.success() {
        return Err(io::Error::other("Linking failed"));
    }

    Ok(())
}
