project( load_and_invect_image )
find_package( OpenCV REQUIRED )
add_executable( load_and_invert_image load_and_invert_image)
target_link_libraries( load_and_invert_image  ${OpenCV_LIBS} )
