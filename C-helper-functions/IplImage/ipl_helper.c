#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <highgui.h>
#include "opencv2/core/types_c.h"
#include "opencv2/core/core_c.h"

// a definition mentioning inline
inline int max(int a, int b) {
return a > b ? a : b;
}

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

uchar get_I(IplImage* img, int x, int y)
{
    uchar* ptr = (uchar*) ( img->imageData + y * img->widthStep );
    return ptr[x];
}


/* Returns writes intensities on the bresentham circle 
   assumes that no access will be out of bounds (2 < x and y < w/-3 */
void bresentham_circle (IplImage* img, int x, int y, uchar circle [])
{
    int width = img->width;
    int height = img->height;
    uchar* ptr;
    // line 0
    ptr = (uchar*) ( img->imageData + (y - 3) * img->widthStep );
    circle[16] = ptr[x-1];
    circle[1] = ptr[x];
    circle[2] = ptr[x+1];
    // line 1
    ptr = (uchar*) ( img->imageData + (y - 2) * img->widthStep );
    circle[15] = ptr[x - 2];
    circle[3] = ptr[x + 2];
    // line 2
    ptr = (uchar*) ( img->imageData + (y - 1) * img->widthStep );
    circle[14] = ptr[x - 3];
    circle[4] = ptr[x + 3];
    // line 3
    ptr = (uchar*) ( img->imageData + y * img->widthStep );
    circle[13] = ptr[x - 3];
    circle[0] = ptr[x];
    circle[5] = ptr[x + 3];
    // line 4
    ptr = (uchar*) ( img->imageData + (y + 1) * img->widthStep );
    circle[12] = ptr[x - 3];
    circle[6] = ptr[x + 3];
    // line 5
    ptr = (uchar*) ( img->imageData + (y + 2) * img->widthStep );
    circle[11] = ptr[x - 2];
    circle[7] = ptr[x + 2];
    // line 6
    ptr = (uchar*) ( img->imageData + (y + 3) * img->widthStep );
    circle[10] = ptr[x - 1];
    circle[9] = ptr[x];
    circle[8] = ptr[x + 1];       
}

/* Returns writes intensities on the bresentham circle 
   assumes that no access will be out of bounds (2 < x and y < w/-3 
   Vals array is 1 element larger than the bitmap (the first element of the bitmap
   is the center of the circle
*/
int are_numbers_contiguous (uchar bitmap [], uchar vals [], int len, int N, 
				uchar threshold )
{
    int sum;
    uchar number_exists;
    int offset, i;
    uchar is_contiguous;

    for (offset = 0; offset < len; offset++) {
	sum = 0;
	is_contiguous = 1;
	for (i = 0; i < N; i++) {
	    number_exists = bitmap[(i + offset) % len];
	    if (number_exists > 0) {
		sum += abs(vals[((i + offset) % len) + 1] - vals[0]) - threshold;
	    } else {
		is_contiguous = 0;
		break;
	    }
	}
	if (is_contiguous == 1)
	    return sum;
    }
    return 0;
}

/* Returns writes intensities on the bresentham circle 
   assumes that no access will be out of bounds (2 < x and y < w/-3 */
int corner_score (uchar pixel_values [], uchar bright_bm [], uchar dark_bm [], 
		  int len, int threshold)
{
    uchar p = pixel_values[0];
    int i;
    for (i = 1; i < len; i++){
	uchar I = pixel_values[i];
	if (I > (p + threshold)) {
	    bright_bm[i-1] = 1;
	    dark_bm[i-1] = 0;
	} else if (I < (p - threshold)) {
	    bright_bm[i-1] = 0;
	    dark_bm[i-1] = 1;
	} else {
	    bright_bm[i-1] = 0;
	    dark_bm[i-1] = 0;
	}
    }    
    int score_bright = are_numbers_contiguous(bright_bm, pixel_values, len-1, 9, 
					      threshold);
    int score_dark = are_numbers_contiguous(dark_bm, pixel_values, len-1, 9, 
					    threshold);
    return max(score_bright, score_dark);
}

void detect_corners (IplImage* src, IplImage* dst, uchar threshold)
{
    // byte arrays for storing intermediate pixel values
    int len = 17;
    uchar pixel_values [len];
    uchar bright_bitmap [len-1];
    uchar dark_bitmap [len-1];
    // get image dimensions
    int width = src->width;
    int height = src->height;
    // for each pixel in the image
    int score = 0;
    int x, y;
    for (x = 0; x < width; x++)
	for (y = 0; y < height; y++) {
	    // circle exists?
	    if ( (x > 2) && (x < (width - 3)) &&
		 (y > 2) && (y < (height - 3))) {
		// compute corner score
		bresentham_circle(src, x, y, pixel_values);
		score = corner_score(pixel_values, bright_bitmap, dark_bitmap, len,
			 threshold);
	    } else 
		score = 0;
	    int s = 0;
	    if ((score > 0) || (score < 0))
		cvRectangle (dst, cvPoint(x-s, y-s), cvPoint(x+s, y+s), 
			     CV_RGB(0, 0, 255), 1, 8, 0);
	    }
}


/* int main(int argc, char *argv[]) */
/* { */
/*     printf ("SIGN: %d\n", 1 << 31); */
/*     printf ("8S: %d\n", IPL_DEPTH_8S); */
/*     printf ("16S: %d\n", IPL_DEPTH_16S); */
/*     printf ("32S: %d\n", IPL_DEPTH_32S); */

/*     printf ("cap: %d\n", CV_CAP_OPENNI_IMAGE_GENERATOR); */
/*     printf ("cv_fourcc: %d\n", CV_FOURCC('Z', 'Z', 'Z', 'Z')); */
    
/*     printf ("default cv_fourcc: %d\n", CV_FOURCC_DEFAULT); */
    
    
/*     return 0; */
/* } */
