#include "zlib.h"
#include <malloc.h>

z_streamp openZStream(int level) {
  z_streamp stream = malloc(sizeof(z_stream));
  if (stream) {
    stream->zalloc = Z_NULL;
    stream->zfree = Z_NULL;
    stream->opaque = Z_NULL;
    if (deflateInit(stream, level) != Z_OK) {
      free(stream);
      stream = NULL;
    }
  }
  return stream;
}

int closeZStream(z_streamp stream) {
  int status = deflateEnd(stream);
  free(stream);
  return status;
}

int zStreamDeflate(z_streamp stream, void* input, unsigned int in_size, void* output, unsigned int out_size, int flush) {
  if (input) {
    stream->next_in = input;
    stream->avail_in = in_size;
  }
  stream->next_out = output;
  stream->avail_out = out_size;
  return deflate(stream, flush);
}

unsigned int zStreamAvailIn(z_streamp stream) {
  return stream->avail_in;
}

unsigned int zStreamAvailOut(z_streamp stream) {
  return stream->avail_out;
}
