# ~~~
# SPDX-FileCopyrightText: Michael Popoloski
# SPDX-License-Identifier: MIT
# ~~~

if(NOT DEFINED SLANG_RUST_CXXBRIDGE_DIR)
  message(FATAL_ERROR "SLANG_RUST_CXXBRIDGE_DIR must be provided by build.rs")
endif()

cmake_path(CONVERT "${SLANG_RUST_CXXBRIDGE_DIR}" TO_CMAKE_PATH_LIST
           SLANG_RUST_CXXBRIDGE_DIR NORMALIZE)

set(SLANG_RUST_CXXBRIDGE_INCLUDE_DIR
    "${SLANG_RUST_CXXBRIDGE_DIR}/include")
set(SLANG_RUST_CXXBRIDGE_CRATE_DIR
    "${SLANG_RUST_CXXBRIDGE_DIR}/crate")
set(SLANG_RUST_CXXBRIDGE_SOURCES_DIR
    "${SLANG_RUST_CXXBRIDGE_DIR}/sources")

set(slang_rust_bridge_generated_sources
    "${SLANG_RUST_CXXBRIDGE_SOURCES_DIR}/slang/bindings/rust/ffi.rs.cc"
    "${SLANG_RUST_CXXBRIDGE_SOURCES_DIR}/slang/bindings/rust/ffi/cxx_sv.rs.cc")

foreach(source IN LISTS slang_rust_bridge_generated_sources)
  if(NOT EXISTS "${source}")
    message(FATAL_ERROR "Missing generated cxx bridge source: ${source}")
  endif()
endforeach()

add_library(
  slang_rust_bridge STATIC
  ${slang_rust_bridge_generated_sources}
  ${CMAKE_CURRENT_SOURCE_DIR}/ffi/wrapper.cc)

target_link_libraries(slang_rust_bridge PRIVATE slang::slang)
target_include_directories(
  slang_rust_bridge
  PRIVATE ${SLANG_RUST_CXXBRIDGE_INCLUDE_DIR} ${SLANG_RUST_CXXBRIDGE_CRATE_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_features(slang_rust_bridge PRIVATE cxx_std_20)

set(slang_rust_bridge_bundle_libs
    "$<TARGET_FILE:slang_rust_bridge>"
    "$<TARGET_FILE:slang_slang>"
    "$<TARGET_FILE:${slang_fmt_target}>")
if(SLANG_USE_MIMALLOC)
  list(APPEND slang_rust_bridge_bundle_libs "$<TARGET_FILE:${mimalloc_target}>")
endif()

set(slang_rust_bridge_bundle_manifest
    "${CMAKE_CURRENT_BINARY_DIR}/slang_rust_bridge_bundle_$<CONFIG>.txt")
string(JOIN "\n" slang_rust_bridge_bundle_manifest_content
       ${slang_rust_bridge_bundle_libs})
file(GENERATE
     OUTPUT "${slang_rust_bridge_bundle_manifest}"
     CONTENT "${slang_rust_bridge_bundle_manifest_content}\n")

add_custom_command(
  TARGET slang_rust_bridge
  POST_BUILD
  COMMAND
    ${CMAKE_COMMAND}
    -DOUTPUT="$<TARGET_FILE:slang_rust_bridge>"
    -DINPUT_LIBS_FILE="${slang_rust_bridge_bundle_manifest}"
    -DARCHIVER="${CMAKE_AR}"
    -DPLATFORM="${CMAKE_SYSTEM_NAME}"
    -P "${PROJECT_SOURCE_DIR}/cmake/merge_static_libs.cmake"
  VERBATIM)

if(SLANG_INCLUDE_INSTALL)
  install(
    TARGETS slang_rust_bridge
    EXPORT slangTargets
    LIBRARY COMPONENT slang_Development
    ARCHIVE COMPONENT slang_Development)
endif()
