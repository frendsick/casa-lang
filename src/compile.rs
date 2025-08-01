use std::fs::{create_dir_all, remove_file};
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::cli::CasaArgs;

pub fn compile_assembly_code(
    assembly_code: &str,
    input_path: &Path,
    args: &CasaArgs,
) -> io::Result<PathBuf> {
    let artifact_dir = &args.artifact_dir;
    let stem = input_path.file_stem().unwrap();
    let parent_dir = input_path.parent().unwrap();

    // Clean artifacts if the artifact directory is not given
    let clean_artifacts = artifact_dir.is_none();
    let artifact_dir = artifact_dir
        .clone()
        .or(Some(parent_dir.to_path_buf()))
        .unwrap();

    // Make sure the build directories exist
    create_dir_all(&artifact_dir)?;
    create_dir_all(&parent_dir)?;

    let assembly_path = PathBuf::from(format!("{}/{}.asm", artifact_dir.display(), stem.display()));
    let object_path = PathBuf::from(format!("{}/{}.o", artifact_dir.display(), stem.display()));
    let executable_path = &args.output.clone().or(Some(parent_dir.join(stem))).unwrap();

    // Compile and link the assembly code
    std::fs::write(&assembly_path, format!("{}\n", assembly_code))?;
    compile_with_as(&assembly_path, &object_path)?;
    link_with_ld(&object_path, executable_path)?;

    // Remove compiler artifacts
    if clean_artifacts {
        let _ = remove_file(&assembly_path);
        let _ = remove_file(&object_path);
    }

    Ok(executable_path.canonicalize()?)
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
