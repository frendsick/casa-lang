use std::path::PathBuf;

use clap::{Args, Parser};

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
    pub input: PathBuf,

    /// Output file path
    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    /// Leave compiler artifacts to this directory
    #[arg(long, value_name = "DIR")]
    pub artifact_dir: Option<PathBuf>,
}

pub fn parse_args(cli: &CasaCli) -> CasaArgs {
    match cli {
        CasaCli::Build(args) | CasaCli::Run(args) => args.clone(),
    }
}
