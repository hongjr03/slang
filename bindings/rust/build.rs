use std::{
    env,
    path::{Path, PathBuf},
};

mod sourcegen;

fn main() {
    let cxxbridge_dir = generate_cxx_bridge();
    let install_dir = build_cpp_lib(&cxxbridge_dir);
    setup_linking(&install_dir);
    setup_rerun_triggers();
    generate_rs();
}

fn generate_cxx_bridge() -> PathBuf {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR is not set"));
    let cxxbridge_dir = out_dir.join("cxxbridge");

    drop(cxx_build::bridges([
        "bindings/rust/ffi.rs",
        "bindings/rust/ffi/cxx_sv.rs",
    ]));

    cxxbridge_dir
}

fn build_cpp_lib(cxxbridge_dir: &Path) -> PathBuf {
    let debug = cfg!(debug_assertions);
    let msvc_release_abi = cfg!(target_env = "msvc");
    let cmake_profile = if msvc_release_abi {
        "Release"
    } else if debug {
        "Debug"
    } else {
        "Release"
    };

    // Configure CMake build
    let config = &mut cmake::Config::new(".");
    config
        .env("CMAKE_BUILD_PARALLEL_LEVEL", "16")
        .define("FETCHCONTENT_TRY_FIND_PACKAGE_MODE", "NEVER")
        .define("SLANG_MASTER_PROJECT", "OFF")
        .define("SLANG_INCLUDE_TESTS", "OFF")
        .define("SLANG_INCLUDE_TOOLS", "OFF")
        .define("SLANG_INCLUDE_INSTALL", "ON")
        .define("SLANG_INCLUDE_PYLIB", "OFF")
        .define("SLANG_INCLUDE_RUSTLIB", "ON")
        .define(
            "SLANG_RUST_CXXBRIDGE_DIR",
            cxxbridge_dir.to_string_lossy().as_ref(),
        )
        .define("CMAKE_MSVC_RUNTIME_LIBRARY", "MultiThreadedDLL")
        .profile(cmake_profile)
        .define("CMAKE_VERBOSE_MAKEFILE", "ON");

    if !debug {
        config.define("CMAKE_INTERPROCEDURAL_OPTIMIZATION", "ON");
    }

    config.build()
}

fn setup_linking(install_dir: &Path) {
    let lib_dir = install_dir.join("lib");
    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=static=slang_rust_bridge");
}

fn setup_rerun_triggers() {
    for file in &[
        "CMakeLists.txt",
        "bindings/CMakeLists.txt",
        "bindings/rust/CMakeLists.txt",
        "external/CMakeLists.txt",
        "source/CMakeLists.txt",
        "cmake/merge_static_libs.cmake",
        "bindings/rust/lib.rs",
        "bindings/rust/ffi.rs",
        "bindings/rust/ffi/cxx_sv.rs",
        "bindings/rust/sourcegen.rs",
    ] {
        println!("cargo:rerun-if-changed={}", file);
    }

    for file in &[
        "bindings/rust/ffi/string_view.h",
        "bindings/rust/ffi/wrapper.h",
        "bindings/rust/ffi/wrapper.cc",
    ] {
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
