use std::path::{Path, PathBuf};

use clap::{Args, Parser};

use crate::error::{CasaError, fatal_error_short};

/// CASA compiler
#[derive(Debug, Parser)]
pub enum CasaCli {
    Build(CasaArgs),
    Run(CasaArgs),
}

#[derive(Debug, Clone, Args)]
#[command(name = "casac")]
#[command(version, about)]
pub struct CasaArgs {
    /// Input file path
    pub input: String,
}

pub fn parse_args(cli: &CasaCli) -> CasaArgs {
    match cli {
        CasaCli::Build(args) | CasaCli::Run(args) => args.clone(),
    }
}

pub fn parse_input_path(args: &CasaArgs) -> PathBuf {
    let code_file_extensions = ["casa"];
    let code_file = match Path::new(&args.input).canonicalize() {
        Ok(file)
            if let Some(ext) = file.extension().and_then(|e| e.to_str())
                && code_file_extensions.contains(&ext) =>
        {
            file
        }
        Ok(_) => fatal_error_short(
            CasaError::UnknownFileExtension,
            &format!(
                "The file `{}` has unknown file extension. Expected any of the following:\n{}",
                args.input,
                code_file_extensions
                    .iter()
                    .map(|ext| format!(".{}", ext))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ),
        Err(error) => fatal_error_short(
            CasaError::FileNotFound,
            &format!("Could not read `{}`: {}`", &args.input, error),
        ),
    };
    code_file
}
