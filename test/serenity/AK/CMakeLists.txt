file(GLOB sources "${CMAKE_SOURCE_DIR}/serenity/AK/*.cpp")
add_library(AK SHARED ${sources})
target_include_directories(AK PUBLIC ${CMAKE_SOURCE_DIR}/serenity)
target_link_libraries(AK pthread)
