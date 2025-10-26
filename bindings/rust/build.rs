use std::path::Path;

mod sourcegen;

fn main() {
    build_cpp_lib();
    setup_linking();
    setup_rerun_triggers();
    generate_rs();
}

fn build_cpp_lib() {
    let debug = cfg!(debug_assertions);

    // Configure CMake build
    let config = &mut cmake::Config::new(".");
    config
        .env("CMAKE_BUILD_PARALLEL_LEVEL", "16")
        .define("SLANG_MASTER_PROJECT", "OFF")
        .define("SLANG_INCLUDE_TESTS", "OFF")
        .define("SLANG_INCLUDE_TOOLS", "OFF")
        .define("SLANG_INCLUDE_INSTALL", "OFF")
        .define("CMAKE_BUILD_TYPE", if debug { "Debug" } else { "Release" })
        .define("SLANG_BOOST_SINGLE_HEADER", "")
        .define("CMAKE_VERBOSE_MAKEFILE", "ON");

    // On Windows, use release CRT even in debug builds to match Rust runtime
    if cfg!(target_os = "windows") && debug {
        config
            .define("CMAKE_MSVC_RUNTIME_LIBRARY", "MultiThreadedDLL")
            .cflag("/MD");
    }

    if !debug {
        // Disable IPO for now - it causes linker issues with FFI symbols
        // config.define("CMAKE_INTERPROCEDURAL_OPTIMIZATION", "ON");
    }

    let dst = config.build().join("build");

    let includes = [
        &format!("{}/source", dst.display()),
        &format!("{}/deps/fmt-src/include", dst.display()),
        "include",
        "source",
        "external",
        "bindings/rust",
    ];

    let mut builder = cxx_build::bridges(["bindings/rust/ffi.rs", "bindings/rust/ffi/cxx_sv.rs"]);
    builder
        .file("bindings/rust/ffi/wrapper.cpp")  // Add wrapper implementation
        .includes(includes.iter().map(Path::new))
        .std("c++20")
        .flag_if_supported("-stdlib=libstdc++")
        .flag_if_supported("-DSLANG_BOOST_SINGLE_HEADER")
        .flag_if_supported("/permissive-");

    // Configure MSVC runtime library to match slang's CMake build
    // On Windows in debug mode, use release CRT to avoid CRT mismatch with Rust runtime
    if cfg!(target_os = "windows") {
        builder.flag_if_supported("/MD");    // Always use release CRT DLL
        if debug {
            builder.flag_if_supported("-DDEBUG");
        }
    } else if debug {
        builder.flag_if_supported("-DDEBUG");
    }

    builder.compile("slang_binding");
}

fn setup_linking() {
    let debug = cfg!(debug_assertions);
    let dst = cmake::Config::new(".")
        .build()
        .join("build")
        .display()
        .to_string();

    println!("cargo:rustc-link-search=native={dst}/lib");
    println!("cargo:rustc-link-lib=static=svlang");

    // Link fmt
    if debug {
        println!("cargo:rustc-link-lib=static=fmtd");
    } else {
        println!("cargo:rustc-link-lib=static=fmt");
    }

    // Link mimalloc - Windows uses different naming
    if cfg!(target_os = "windows") {
        if debug {
            println!("cargo:rustc-link-lib=static=mimalloc-static-debug");
        } else {
            println!("cargo:rustc-link-lib=static=mimalloc-static");
        }
    } else {
        // Try pkg-config first on Unix
        let pkg_config_success = std::process::Command::new("pkg-config")
            .args(["--libs", "mimalloc"])
            .output()
            .ok()
            .filter(|output| output.status.success());

        if let Some(output) = pkg_config_success {
            for arg in String::from_utf8(output.stdout).unwrap().split_whitespace() {
                if let Some(path) = arg.strip_prefix("-L") {
                    println!("cargo:rustc-link-search=native={path}");
                } else if let Some(lib) = arg.strip_prefix("-l") {
                    println!("cargo:rustc-link-lib={lib}");
                }
            }
        } else {
            if debug {
                println!("cargo:rustc-link-lib=mimalloc-debug");
            } else {
                println!("cargo:rustc-link-lib=mimalloc");
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

    for file in &[
        "bindings/rust/ffi/string_view.h",
        "bindings/rust/ffi/wrapper.h",
    ] {
        println!("cargo:rerun-if-changed={}", file);
    }

    for file in &[
        "scripts/syntax.txt",
        "scripts/tokenkinds.txt",
        "scripts/triviakinds.txt",
    ] {
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
