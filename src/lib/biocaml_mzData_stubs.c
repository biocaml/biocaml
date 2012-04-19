/* File: biocaml_mzXML_stubs.c

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

/* C code to do the decoding of base 64 data and its conversion to
 * an array of double.  Does not store the full decoded string.
 * Part of the code is influenced by the RAMP library.
 */

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

/* of_base64[c] gives the number (0..63) corresponding to the base64 char. */
static const unsigned char of_base64[] = {
  0,  /* NUL  */
  0,  /* SOH  */
  0,  /* STX  */
  0,  /* ETX  */
  0,  /* EOT  */
  0,  /* ENQ  */
  0,  /* ACK  */
  0,  /* BEL  */
  0,  /*  BS  */
  0,  /*  HT  */
  0,  /*  LF  */
  0,  /*  VT  */
  0,  /*  FF  */
  0,  /*  CR  */
  0,  /*  SO  */
  0,  /*  SI  */
  0,  /* DLE  */
  0,  /* DC1  */
  0,  /* DC2  */
  0,  /* DC3  */
  0,  /* DC4  */
  0,  /* NAK  */
  0,  /* SYN  */
  0,  /* ETB  */
  0,  /* CAN  */
  0,  /*  EM  */
  0,  /* SUB  */
  0,  /* ESC  */
  0,  /*  FS  */
  0,  /*  GS  */
  0,  /*  RS  */
  0,  /*  US  */
  0,  /*  SP  */
  0,  /*   !  */
  0,  /*   "  */
  0,  /*   #  */
  0,  /*   $  */
  0,  /*   %  */
  0,  /*   &  */
  0,  /*   '  */
  0,  /*   (  */
  0,  /*   )  */
  0,  /*   *  */
  62, /*   +  */
  0,  /*   ,  */
  0,  /*   -  */
  0,  /*   .  */
  63, /*   /  */
  52, /*   0  */
  53, /*   1  */
  54, /*   2  */
  55, /*   3  */
  56, /*   4  */
  57, /*   5  */
  58, /*   6  */
  59, /*   7  */
  60, /*   8  */
  61, /*   9  */
  0,  /*   :  */
  0,  /*   ;  */
  0,  /*   <  */
  0,  /*   =  */
  0,  /*   >  */
  0,  /*   ?  */
  0,  /*   @  */
  0,  /*   A  */
  1,  /*   B  */
  2,  /*   C  */
  3,  /*   D  */
  4,  /*   E  */
  5,  /*   F  */
  6,  /*   G  */
  7,  /*   H  */
  8,  /*   I  */
  9,  /*   J  */
  10, /*   K  */
  11, /*   L  */
  12, /*   M  */
  13, /*   N  */
  14, /*   O  */
  15, /*   P  */
  16, /*   Q  */
  17, /*   R  */
  18, /*   S  */
  19, /*   T  */
  20, /*   U  */
  21, /*   V  */
  22, /*   W  */
  23, /*   X  */
  24, /*   Y  */
  25, /*   Z  */
  0,  /*   [  */
  0,  /* '\'  */ 
  0,  /*   ]  */
  0,  /*   ^  */
  0,  /*   _  */
  0,  /*   `  */
  26, /*   a  */
  27, /*   b  */
  28, /*   c  */
  29, /*   d  */
  30, /*   e  */
  31, /*   f  */
  32, /*   g  */
  33, /*   h  */
  34, /*   i  */
  35, /*   j  */
  36, /*   k  */
  37, /*   l  */
  38, /*   m  */
  39, /*   n  */
  40, /*   o  */
  41, /*   p  */
  42, /*   q  */
  43, /*   r  */
  44, /*   s  */
  45, /*   t  */
  46, /*   u  */
  47, /*   v  */
  48, /*   w  */
  49, /*   x  */
  50, /*   y  */
  51, /*   z  */
  0,  /*   {  */
  0,  /*   |  */
  0,  /*   }  */
  0,  /*   ~  */
  0   /* DEL  */
};

/* Tables to retrieve quickly the output bytes */
static unsigned char of_base64_1[0x7fff]; /* 1st output byte */
static unsigned char of_base64_2[0x7fff]; /* 2nd output byte */
static unsigned char of_base64_3[0x7fff]; /* 3rd output byte */
static unsigned char of_base64_12[2 * 0x7fffff]; /* 1st & 2nd in one shot */
static int is_little_endian;
static size_t pack_offset;
/* Offset to pack 3 bytes in an int to make sure the integer is not
   larger than 0x7fffff. */

CAMLexport
value biocaml_base64_init(value vunit) 
{
  /* no OCaml alloc */
  int i, j, k, index;
  char *c;
  
  i = 1;
  is_little_endian = ((char *)&i)[0];
  if (is_little_endian) pack_offset = 0;
  else pack_offset = sizeof(int) - 3;
  
  /* Fill the lookup tables */
  for (i='+'; i<='z'; i++) {
    for (j='+'; j<='z'; j++) {
      index = (i<<8)|j;
      of_base64_1[index] = (of_base64[i]<<2)|(of_base64[j]>>4);
      of_base64_2[index] = (of_base64[i]<<4)|(of_base64[j]>>2);
      of_base64_3[index] = (of_base64[i]<<6)|(of_base64[j]);
    }
  }
  for (i='+'; i<='z'; i++) {
    for (j='+'; j<='z'; j++) {
      for (k='+'; k<='z'; k++) {
        /* Pack 3 chars (i,j,k) into an int to memcpy(of_base64_12) later */
        c = (char *) &index;
        index = 0;
        c += pack_offset; /* for big-endian ([index] not too large) */
        *c++ = i;
        *c++ = j;
        *c = k;
        index *= 2; /* 2 output bytes */
        of_base64_12[index++] = of_base64_1[(i<<8)|j];
        of_base64_12[index]   = of_base64_2[(j<<8)|k];
      }
    }
  }
  return(Val_unit);
}

#define DECODE_BODY(ty, lenbuf, set_index, to_host_order)               \
  b = buffer;           /* current position (byte) in buffer */         \
  data = (ty *) buffer; /* see buffer as elements of type [ty] */       \
  for(i = 0; i < npeaks; i++) {                                         \
    /* Fill the buffer to be able to read 1 float. */                   \
    while (b < (unsigned char *) (data + 1)) {                          \
      /* Decode 4 bytes of "peaks" into 3 bytes of "b" (buffer) */      \
      index = 0; /* not all bytes filled by memcpy */                   \
      memcpy(((char *)&index) + pack_offset, peaks, 3);                 \
      memcpy(b, of_base64_12 + 2 * index, 2);                           \
      b[2] = of_base64_3[(peaks[2]<<8)|(peaks[3])];                     \
      peaks += 4;                                                       \
      b += 3;                                                           \
    }                                                                   \
    /* convert from little byte endian (for 1 float/double) */          \
    to_host_order((unsigned char *) data);                              \
    /* Extract the number */                                            \
    vec[i] = *data++;                                                   \
    /* If at end of the buffer, rewind. */                              \
    if (b >= buffer + lenbuf) {                                         \
      b = buffer;                                                       \
      data = (ty *) buffer;                                             \
    }                                                                   \
  }

#define TO_HOST_LITTLE_ENDIAN(data) /* void; host order is little-endian */

#define DECODE(ty, lenbuf, to_host_big_endian)                          \
  const char *peaks = String_val(vpeaks);                               \
  int i, npeaks = Int_val(vnpeaks);                                     \
  unsigned int index;                                                   \
  unsigned char *b, buffer[lenbuf]; /* partially decoded string */      \
  ty *data; /* to cast the decoded bytes */                             \
  double *vec = (double *) Data_bigarray_val(vvec);                     \
                                                                        \
  if (is_little_endian) {                                               \
    DECODE_BODY(ty, lenbuf, INDEX_LITTLE_ENDIAN, TO_HOST_LITTLE_ENDIAN); \
  }                                                                     \
  else {                                                                \
    DECODE_BODY(ty, lenbuf, INDEX_BIG_ENDIAN, to_host_big_endian);      \
  }

static void swap_bytes32(unsigned char *b)
{
  /* From little-endian to big endian (for 1 float). */
  char c;
  /* bytes 0..3 */
  c = b[0];  b[0] = b[3];  b[3] = c;
  c = b[1];  b[1] = b[2];  b[2] = c;
}

CAMLexport value biocaml_base64_decode32(value vpeaks, value vnpeaks,
                                         value vvec)
{
  CAMLparam2(vpeaks, vvec);
  DECODE(float, 12, swap_bytes32);
  CAMLreturn(Val_unit);
}

static void swap_bytes64(unsigned char *b)
{
  /* From little-endian to big endian (for 1 double). */
  char c;
  c = b[0];  b[0] = b[7];  b[7] = c; /* bytes 0, 7 */
  c = b[1];  b[1] = b[6];  b[6] = c; /* bytes 1, 6 */
  c = b[2];  b[2] = b[5];  b[5] = c; /* bytes 2, 5 */
  c = b[3];  b[3] = b[4];  b[4] = c; /* bytes 3, 4 */
}

CAMLexport value biocaml_base64_decode64(value vpeaks, value vnpeaks,
                                         value vvec)
{
  CAMLparam2(vpeaks, vvec);
  DECODE(double, 24, swap_bytes64);
  CAMLreturn(Val_unit);
}



/* Local Variables: */
/* compile-command: "make -k -C ../.." */
/* End: */
