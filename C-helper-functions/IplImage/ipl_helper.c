#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <highgui.h>
#include <cv.h>


int ipl_data_ref (uchar* img_data, int index)
{
return img_data[index];
}


void ipl_data_set (uchar* img_data, int index, uchar value)
{
    img_data[index] = value;
}

void invert_ipl_matrix ( uchar* img_data, int data_length)
{
    int i;
    for (i = 0; i < data_length; i++){
	img_data[i] = 255 - img_data[i];
    }
}

void ipl_apply_fn (int (*fn)(int), uchar* img_data, int data_length)
{
    int i;
    for (i = 0; i < data_length; i++){
	img_data[i] = fn(img_data[i]);
    }
}

int main(int argc, char *argv[])
{
    printf ("SIGN: %d\n", IPL_DEPTH_SIGN);
    printf ("8S: %d\n", IPL_DEPTH_8S);
    printf ("16S: %d\n", IPL_DEPTH_16S);
    printf ("32S: %d\n", IPL_DEPTH_32S);

    printf ("cap: %d\n", CV_CAP_OPENNI_IMAGE_GENERATOR);
    printf ("cv_fourcc: %d\n", CV_FOURCC('Z', 'Z', 'Z', 'Z'));
    printf ("default cv_fourcc: %d\n", CV_FOURCC_DEFAULT);
    
    
    return 0;
}
