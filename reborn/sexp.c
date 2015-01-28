#include "sexp.h"

sexp sexp_cons_op (sexp ctx, sexp self, sexp_sint_t n, sexp head, sexp tail) {
  sexp pair = sexp_alloc_type(ctx, pair, SEXP_PAIR);
  if (sexp_exceptionp(pair)) return pair;
  sexp_car(pair) = head;
  sexp_cdr(pair) = tail;
  sexp_pair_source(pair) = SEXP_FALSE;
  return pair;
}

// TODO GC
void* sexp_alloc (sexp ctx, size_t size) {
  return malloc(size);
}

sexp sexp_equalp_bound (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp depth, sexp bound) {
  sexp_uint_t left_size, right_size;
  sexp_sint_t i, len;
  sexp t, *p, *q, depth2;
  char *p_left, *p_right, *q_left, *q_right;

 loop:
  if (a == b)
    return bound;
  else if ((!a || !sexp_pointerp(a)) || (!b || !sexp_pointerp(b))
           || (sexp_pointer_tag(a) != sexp_pointer_tag(b)))
    return SEXP_FALSE;

  /* a and b are both pointers of the same type */
#if SEXP_USE_FLONUMS && ! SEXP_USE_IMMEDIATE_FLONUMS
  if (sexp_pointer_tag(a) == SEXP_FLONUM)
    return sexp_flonum_eqv(a, b) ? bound : SEXP_FALSE;
#endif
  /* check limits */
  if (sexp_unbox_fixnum(bound) < 0 || sexp_unbox_fixnum(depth) < 0)
    return bound;
  depth2 = sexp_fx_sub(depth, SEXP_ONE);
  bound = sexp_fx_sub(bound, SEXP_ONE);
  t = sexp_object_type(ctx, a);
  p_left = ((char*)a) + offsetof(struct sexp_struct, value);
  p = (sexp*) (((char*)a) + sexp_type_field_base(t));
  q_left = ((char*)b) + offsetof(struct sexp_struct, value);
  q = (sexp*) (((char*)b) + sexp_type_field_base(t));
  /* if no fields, the base is value (just past the header) */
  if ((sexp)p == a) {p=(sexp*)p_left; q=(sexp*)q_left;}
  /* check preliminary non-object data */
  left_size = (char*)p - p_left;
  if ((left_size > 0) && memcmp(p_left, q_left, left_size))
    return SEXP_FALSE;
  /* check trailing non-object data */
  p_right = ((char*)p + sexp_type_num_slots_of_object(t,a)*sizeof(sexp));
  right_size = ((char*)a + sexp_type_size_of_object(t, a)) - p_right;
  if (right_size > 0) {
    q_right = ((char*)q + sexp_type_num_slots_of_object(t,b)*sizeof(sexp));
    if (right_size != ((char*)b + sexp_type_size_of_object(t, b)) - q_right)
      return SEXP_FALSE;
    if (memcmp(p_right, q_right, right_size))
      return SEXP_FALSE;
  }
  /* left and right non-object data is the same, now check eq-object slots */
  len = sexp_type_num_eq_slots_of_object(t, a);
  if (len > 0) {
    for (; len > 1; len--) {
      a = p[len-1]; b = q[len-1];
      if (a != b) {
        if ((!a || !sexp_pointerp(a)) || (!b || !sexp_pointerp(b))
            || (sexp_pointer_tag(a) != sexp_pointer_tag(b)))
          return SEXP_FALSE;
        else break;
      }
    }
    for (i=0; i<len-1; i++) {
      bound = sexp_equalp_bound(ctx, self, n, p[i], q[i], depth2, bound);
      if (sexp_not(bound)) return SEXP_FALSE;
    }
    /* tail-recurse on the last value (same depth) */
    a = p[len-1]; b = q[len-1]; goto loop;
  }
  return bound;
}

sexp sexp_equalp_op (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
  return sexp_make_boolean(
    sexp_truep(sexp_equalp_bound(ctx, self, n, a, b,
                                 sexp_make_fixnum(SEXP_DEFAULT_EQUAL_DEPTH),
                                 sexp_make_fixnum(SEXP_DEFAULT_EQUAL_BOUND))));
}

sexp sexp_make_vector_op (sexp ctx, sexp self, sexp_sint_t n, sexp len, sexp dflt) {
  sexp vec, *x;
  int i, clen = sexp_unbox_fixnum(len);
  if (! clen) return sexp_global(ctx, SEXP_G_EMPTY_VECTOR);
  vec = sexp_alloc_tagged(ctx, sexp_sizeof(vector) + clen*sizeof(sexp),
                          SEXP_VECTOR);
  if (sexp_exceptionp(vec)) return vec;
  x = sexp_vector_data(vec);
  for (i=0; i<clen; i++)
    x[i] = dflt;
  sexp_vector_length(vec) = clen;
  return vec;
}

sexp sexp_alloc_tagged_aux(sexp ctx, size_t size, sexp_uint_t tag sexp_current_source_param) {
#if SEXP_USE_TRACK_ALLOC_BACKTRACE
  int i;
  void* trace[SEXP_BACKTRACE_SIZE + 1];
#endif
  sexp res = (sexp) sexp_alloc(ctx, size);
  if (res && ! sexp_exceptionp(res)) {
    sexp_pointer_tag(res) = tag;
#if SEXP_USE_TRACK_ALLOC_SOURCE
    sexp_pointer_source(res) = source;
#if SEXP_USE_TRACK_ALLOC_BACKTRACE
    backtrace(trace, SEXP_BACKTRACE_SIZE + 1);
    for (i=0; i<SEXP_BACKTRACE_SIZE; i++) res->backtrace[i] = trace[i+1];
#endif
#endif
#if SEXP_USE_HEADER_MAGIC
    sexp_pointer_magic(res) = SEXP_POINTER_MAGIC;
#endif
  }
  return res;
}