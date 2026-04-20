if(NOT DEFINED OUTPUT)
  message(FATAL_ERROR "OUTPUT is required")
endif()

if(NOT DEFINED INPUT_LIBS_FILE OR INPUT_LIBS_FILE STREQUAL "")
  message(FATAL_ERROR "INPUT_LIBS_FILE is required")
endif()

if(NOT DEFINED ARCHIVER OR ARCHIVER STREQUAL "")
  message(FATAL_ERROR "ARCHIVER is required")
endif()

if(NOT DEFINED PLATFORM OR PLATFORM STREQUAL "")
  message(FATAL_ERROR "PLATFORM is required")
endif()

foreach(var OUTPUT INPUT_LIBS_FILE ARCHIVER)
  string(REGEX REPLACE "^\"(.*)\"$" "\\1" ${var} "${${var}}")
endforeach()
string(REGEX REPLACE "^\"(.*)\"$" "\\1" PLATFORM "${PLATFORM}")

file(STRINGS "${INPUT_LIBS_FILE}" INPUT_LIBS)

set(output_dir "${OUTPUT}.dir")
set(temp_output "${OUTPUT}.tmp")
file(MAKE_DIRECTORY "${output_dir}")

if(PLATFORM STREQUAL "Windows")
  execute_process(
    COMMAND "${ARCHIVER}" /NOLOGO "/OUT:${temp_output}" ${INPUT_LIBS}
    RESULT_VARIABLE merge_result)
elseif(PLATFORM STREQUAL "Darwin")
  execute_process(
    COMMAND /usr/bin/libtool -static -o "${temp_output}" ${INPUT_LIBS}
    RESULT_VARIABLE merge_result)
else()
  set(mri_script "${output_dir}/merge.mri")
  file(WRITE "${mri_script}" "create ${temp_output}\n")
  foreach(input_lib IN LISTS INPUT_LIBS)
    file(APPEND "${mri_script}" "addlib ${input_lib}\n")
  endforeach()
  file(APPEND "${mri_script}" "save\nend\n")

  execute_process(
    COMMAND "${ARCHIVER}" -M
    INPUT_FILE "${mri_script}"
    RESULT_VARIABLE merge_result)
endif()

if(NOT merge_result EQUAL 0)
  message(FATAL_ERROR "Failed to merge static libraries into ${OUTPUT}")
endif()

file(COPY_FILE "${temp_output}" "${OUTPUT}" ONLY_IF_DIFFERENT)
