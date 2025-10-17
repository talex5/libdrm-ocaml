#include <sys/sysmacros.h>
#include <sys/mman.h>

#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/custom.h>
#include <caml/io.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

value caml_libdrm_makedev(value v_maj, value v_min) {
	dev_t x = makedev(Long_val(v_maj), Long_val(v_min));
	return caml_copy_int64(x);
}

value caml_libdrm_major(value v_dev) { return Val_int(major(Int64_val(v_dev))); }
value caml_libdrm_minor(value v_dev) { return Val_int(minor(Int64_val(v_dev))); }
value caml_unix_error_of_code_2(value v_errno) { return caml_unix_error_of_code(Int_val(v_errno)); }

/* Memory-mapped files.
 *
 * This is based on [Unix.map_file], but we don't try to extend the file's size
 * (this fails for device files).
 */

static void caml_ba_unmap_file2(void * addr, uintnat len)
{
  uintnat page = sysconf(_SC_PAGESIZE);
  uintnat delta = (uintnat) addr % page;
  if (len == 0) return;         /* PR#5463 */
  addr = (void *)((uintnat)addr - delta);
  len  = len + delta;
  munmap(addr, len);
}

static void caml_ba_mapped_finalize2(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  if ((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_MAPPED_FILE) {
    if (b->proxy == NULL) {
      // printf("proxy is NULL: unmap\n");
      caml_ba_unmap_file2(b->data, caml_ba_byte_size(b));
    } else {
      if (-- b->proxy->refcount == 0) {
	// printf("refcount is 0: unmap\n");
	caml_ba_unmap_file2(b->proxy->data, b->proxy->size);
	free(b->proxy);
      } else {
	// printf("non-zero refcount\n");
      }
    }
  } else {
    // printf("Already unmapped manually\n");
  }
}

static struct custom_operations caml_ba_mapped_ops2 = {
  "_bigarray",
  caml_ba_mapped_finalize2,
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  caml_ba_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value caml_unix_mapped_alloc2(int flags, int num_dims, void * data, intnat * dim)
{
  uintnat asize;
  int i;
  value res;
  struct caml_ba_array * b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  CAMLassert(num_dims >= 0 && num_dims <= CAML_BA_MAX_NUM_DIMS);
  CAMLassert((flags & CAML_BA_KIND_MASK) < CAML_BA_FIRST_UNIMPLEMENTED_KIND);
  for (i = 0; i < num_dims; i++) dimcopy[i] = dim[i];
  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  res = caml_alloc_custom(&caml_ba_mapped_ops2, asize, 0, 1);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags | CAML_BA_MAPPED_FILE;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dimcopy[i];
  return res;
}

/* Simplified version of the stdlib's map_file that doesn't try to call fstat or ftruncate. */
CAMLprim value caml_map_pseudofile(value vfd, value vkind, value vstart, value vwidth, value vheight)
{
  int fd, flags;
  intnat dim[2] = { Long_val(vheight), Long_val(vwidth) };
  uintnat array_size;
  void *addr = NULL;

  fd = Int_val(vfd);
  flags = Caml_ba_kind_val(vkind) | CAML_BA_C_LAYOUT;
  if (dim[0] < 0) caml_invalid_argument("caml_map_pseudofile: negative width");
  if (dim[1] < 0) caml_invalid_argument("caml_map_pseudofile: negative height");
  array_size = dim[0] * dim[1] * caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  if (array_size > 0)
    addr = mmap(NULL, array_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, File_offset_val(vstart));
  if (addr == (void *) MAP_FAILED) uerror("map_file", Nothing);
  /* Build and return the OCaml bigarray */
  return caml_unix_mapped_alloc2(flags, 2, addr, dim);
}
