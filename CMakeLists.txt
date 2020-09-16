project(sarge)

option(BUILD_SHARED_LIBS "Build shared library." OFF)

ADD_LIBRARY(sarge src/sarge.cpp)
add_library(sarge::sarge ALIAS sarge)

target_include_directories(sarge
        PUBLIC ${PROJECT_SOURCE_DIR}/src
)
