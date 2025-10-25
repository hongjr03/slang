use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

mod sourcegen;

fn main() {
    let build_output = build_cpp_lib();
    setup_linking(&build_output);
    setup_rerun_triggers();
    generate_rs();
}

struct BuildOutput {
    lib_dir: PathBuf,
    debug: bool,
}

fn build_cpp_lib() -> BuildOutput {
    let debug = cfg!(debug_assertions);

    // Configure CMake build
    let mut config = cmake::Config::new(".");
    config
        .env("CMAKE_BUILD_PARALLEL_LEVEL", "16")
        .define("SLANG_MASTER_PROJECT", "OFF")
        .define("SLANG_INCLUDE_TESTS", "OFF")
        .define("SLANG_INCLUDE_TOOLS", "OFF")
        .define("SLANG_INCLUDE_INSTALL", "OFF")
        .define("CMAKE_BUILD_TYPE", if debug { "Debug" } else { "Release" })
        .define("SLANG_BOOST_SINGLE_HEADER", "")
        .define("CMAKE_VERBOSE_MAKEFILE", "ON")
        .define("BUILD_SHARED_LIBS", "OFF");

    if !debug {
        config.define("CMAKE_INTERPROCEDURAL_OPTIMIZATION", "ON");
    }

    let out_dir = config.build();
    let build_dir = out_dir.join("build");
    let lib_dir = build_dir.join("lib");

    let include_paths = [
        build_dir.join("source"),
        build_dir.join("deps/fmt-src/include"),
        PathBuf::from("include"),
        PathBuf::from("source"),
        PathBuf::from("external"),
        PathBuf::from("bindings/rust"),
    ];

    let mut builder = cxx_build::bridges(["bindings/rust/ffi.rs", "bindings/rust/ffi/cxx_sv.rs"]);
    builder
        .includes(include_paths.iter().map(PathBuf::as_path))
        .std("c++20")
        .flag_if_supported("-stdlib=libstdc++")
        .flag_if_supported("-DSLANG_BOOST_SINGLE_HEADER");

    if debug {
        builder.flag_if_supported("-DDEBUG");
    }

    builder.compile("slang_binding");

    BuildOutput { lib_dir, debug }
}

fn setup_linking(build: &BuildOutput) {
    println!("cargo:rustc-link-search=native={}", build.lib_dir.display());
    if !link_built_library(&build.lib_dir, "svlang", build.debug) {
        panic!("missing slang library in CMake build output");
    }

    for dep in ["fmt", "mimalloc"] {
        if !link_built_library(&build.lib_dir, dep, build.debug) {
            if !link_via_pkg_config(dep) {
                panic!(
                    "failed to locate dependency library `{dep}`; install it or ensure CMake builds it"
                );
            }
        }
    }
}

fn setup_rerun_triggers() {
    for file in &[
        "bindings/rust/lib.rs",
        "bindings/rust/ffi.rs",
        "bindings/rust/ffi/cxx_sv.rs",
        "bindings/rust/sourcegen.rs",
    ] {
        println!("cargo:rerun-if-changed={}", file);
    }

    for file in &["bindings/rust/ffi/string_view.h", "bindings/rust/ffi/wrapper.h"] {
        println!("cargo:rerun-if-changed={}", file);
    }

    for file in &["scripts/syntax.txt", "scripts/tokenkinds.txt", "scripts/triviakinds.txt"] {
        println!("cargo:rerun-if-changed={}", file);
    }
}

fn generate_rs() {
    let (all_types, kind_map) = sourcegen::loader::load_types();
    sourcegen::generator::generate_syntax_kind(&kind_map);
    sourcegen::generator::generate_ast_file(&all_types, &kind_map);

    if let Ok(tokens) = sourcegen::loader::load_token_macros() {
        sourcegen::generator::generate_token_macro(tokens);
    }
}

fn link_built_library(lib_dir: &Path, base: &str, debug: bool) -> bool {
    if let Some((name, kind)) = resolve_library(lib_dir, base, debug) {
        match kind {
            LinkKind::Static => println!("cargo:rustc-link-lib=static={name}"),
            LinkKind::Dynamic => println!("cargo:rustc-link-lib={name}"),
        }
        true
    } else {
        false
    }
}

fn resolve_library(lib_dir: &Path, base: &str, debug: bool) -> Option<(String, LinkKind)> {
    let mut artifacts = collect_library_artifacts(lib_dir);
    if artifacts.is_empty() {
        return None;
    }

    for candidate in preferred_candidates(base, debug) {
        if let Some(artifact) = artifacts.iter().find(|artifact| artifact.name == candidate) {
            return Some((artifact.name.clone(), artifact.kind));
        }
    }

    artifacts.sort_by(|a, b| a.name.cmp(&b.name));
    artifacts
        .into_iter()
        .find(|artifact| artifact.name == base || artifact.name.starts_with(&format!("{base}-")))
        .map(|artifact| (artifact.name, artifact.kind))
}

fn collect_library_artifacts(lib_dir: &Path) -> Vec<LibraryArtifact> {
    let mut artifacts = Vec::new();
    if let Ok(entries) = fs::read_dir(lib_dir) {
        for entry in entries.flatten() {
            if let Some(artifact) = LibraryArtifact::from_path(&entry.path()) {
                artifacts.push(artifact);
            }
        }
    }
    artifacts
}

fn preferred_candidates(base: &str, debug: bool) -> Vec<String> {
    let mut names = Vec::new();
    let mut push_unique = |value: String| {
        if !names.iter().any(|existing| existing == &value) {
            names.push(value);
        }
    };

    if debug {
        push_unique(format!("{base}d"));
        push_unique(format!("{base}-debug"));
        push_unique(format!("{base}_debug"));
        push_unique(format!("{base}-static-debug"));
        push_unique(base.to_owned());
    } else {
        push_unique(base.to_owned());
        push_unique(format!("{base}-static"));
        push_unique(format!("{base}_static"));
        push_unique(format!("{base}-mt"));
        push_unique(format!("{base}-static-release"));
    }

    push_unique(format!("{base}-static"));
    push_unique(format!("{base}_static"));

    names
}

fn link_via_pkg_config(lib_name: &str) -> bool {
    let output = Command::new("pkg-config").args(["--libs", lib_name]).output().ok();

    let output = match output {
        Some(output) if output.status.success() => output,
        _ => return false,
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    for arg in stdout.split_whitespace() {
        if let Some(path) = arg.strip_prefix("-L") {
            println!("cargo:rustc-link-search=native={path}");
        } else if let Some(lib) = arg.strip_prefix("-l") {
            println!("cargo:rustc-link-lib={lib}");
        }
    }

    true
}

#[derive(Clone, Copy)]
enum LinkKind {
    Static,
    Dynamic,
}

struct LibraryArtifact {
    name: String,
    kind: LinkKind,
}

impl LibraryArtifact {
    fn from_path(path: &Path) -> Option<Self> {
        let file_name = path.file_name()?.to_str()?;
        if let Some((name, kind)) = parse_library_filename(file_name) {
            Some(Self { name: name.to_string(), kind })
        } else {
            None
        }
    }
}

fn parse_library_filename(file_name: &str) -> Option<(&str, LinkKind)> {
    if let Some(stripped) = file_name.strip_suffix(".dll.a") {
        return Some((strip_lib_prefix(stripped), LinkKind::Dynamic));
    }

    if let Some(stripped) = file_name.strip_suffix(".dll") {
        return Some((strip_lib_prefix(stripped), LinkKind::Dynamic));
    }

    if let Some((before, _)) = file_name.split_once(".so.") {
        return Some((strip_lib_prefix(before), LinkKind::Dynamic));
    }

    if let Some(stripped) = file_name.strip_suffix(".so") {
        return Some((strip_lib_prefix(stripped), LinkKind::Dynamic));
    }

    if let Some(stripped) = file_name.strip_suffix(".dylib") {
        return Some((strip_lib_prefix(stripped), LinkKind::Dynamic));
    }

    if let Some(stripped) = file_name.strip_suffix(".a") {
        return Some((strip_lib_prefix(stripped), LinkKind::Static));
    }

    if let Some(stripped) = file_name.strip_suffix(".lib") {
        return Some((strip_lib_prefix(stripped), LinkKind::Static));
    }

    None
}

fn strip_lib_prefix(name: &str) -> &str {
    name.strip_prefix("lib").unwrap_or(name)
}
