cmake_minimum_required(VERSION 2.8)
project( ipl_helper )
## get libraries
set(OPENCV_LIB_COMPONENTS core highgui imgproc video)
set (CV_DIR /opt/local)
foreach(__CVLIB ${OPENCV_LIB_COMPONENTS})
    set(OpenCV_LIBS ${OpenCV_LIBS} "${CV_DIR}/lib/libopencv_${__CVLIB}.dylib")
endforeach (__CVLIB)

set (OpenCV_INCLUDE_DIRS /opt/local/include /opt/local/include/opencv2/highgui /opt/local/include/opencv)

include_directories(${OpenCV_INCLUDE_DIRS})
link_directories(${OpenCV_INCLUDE_DIRS})


#add_executable( ipl_helper ipl_helper)
#target_link_libraries( ipl_helper ${OpenCV_LIBS})

add_library(libraryipl SHARED ipl_helper)
target_link_libraries( libraryipl ${OpenCV_LIBS})

