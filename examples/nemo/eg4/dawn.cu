#define DAWN_GENERATED 1
#undef DAWN_BACKEND_T
#define DAWN_BACKEND_T CUDA
#ifndef BOOST_RESULT_OF_USE_TR1
#define BOOST_RESULT_OF_USE_TR1 1
#endif
#ifndef BOOST_NO_CXX11_DECLTYPE
#define BOOST_NO_CXX11_DECLTYPE 1
#endif
#ifndef GRIDTOOLS_DAWN_HALO_EXTENT
#define GRIDTOOLS_DAWN_HALO_EXTENT 3
#endif
#ifndef BOOST_PP_VARIADICS
#define BOOST_PP_VARIADICS 1
#endif
#ifndef BOOST_FUSION_DONT_USE_PREPROCESSED_FILES
#define BOOST_FUSION_DONT_USE_PREPROCESSED_FILES 1
#endif
#ifndef BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS
#define BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS 1
#endif
#ifndef GT_VECTOR_LIMIT_SIZE
#define GT_VECTOR_LIMIT_SIZE 30
#endif
#ifndef BOOST_FUSION_INVOKE_MAX_ARITY
#define BOOST_FUSION_INVOKE_MAX_ARITY GT_VECTOR_LIMIT_SIZE
#endif
#ifndef FUSION_MAX_VECTOR_SIZE
#define FUSION_MAX_VECTOR_SIZE GT_VECTOR_LIMIT_SIZE
#endif
#ifndef FUSION_MAX_MAP_SIZE
#define FUSION_MAX_MAP_SIZE GT_VECTOR_LIMIT_SIZE
#endif
#ifndef BOOST_MPL_LIMIT_VECTOR_SIZE
#define BOOST_MPL_LIMIT_VECTOR_SIZE GT_VECTOR_LIMIT_SIZE
#endif
#include <driver-includes/gridtools_includes.hpp>
using namespace gridtools::dawn;

namespace dawn_generated {
namespace cuda {
__constant__ int stage2421GlobalIIndices_[2];
__constant__ int stage2421GlobalJIndices_[2];
__constant__ int stage2429GlobalIIndices_[2];
__constant__ int stage2429GlobalJIndices_[2];
__constant__ int stage2433GlobalIIndices_[2];
__constant__ int stage2433GlobalJIndices_[2];
__constant__ int stage2437GlobalIIndices_[2];
__constant__ int stage2437GlobalJIndices_[2];
__constant__ int stage2440GlobalIIndices_[2];
__constant__ int stage2440GlobalJIndices_[2];
__constant__ int stage2444GlobalIIndices_[2];
__constant__ int stage2444GlobalJIndices_[2];
__constant__ int stage2448GlobalIIndices_[2];
__constant__ int stage2448GlobalJIndices_[2];
__constant__ int stage2465GlobalIIndices_[2];
__constant__ int stage2465GlobalJIndices_[2];
__constant__ int stage2506GlobalIIndices_[2];
__constant__ int stage2506GlobalJIndices_[2];
__constant__ int stage2527GlobalIIndices_[2];
__constant__ int stage2527GlobalJIndices_[2];
__constant__ int stage2532GlobalIIndices_[2];
__constant__ int stage2532GlobalJIndices_[2];
__constant__ int stage2536GlobalIIndices_[2];
__constant__ int stage2536GlobalJIndices_[2];
__constant__ int stage2540GlobalIIndices_[2];
__constant__ int stage2540GlobalJIndices_[2];
__constant__ int stage2543GlobalIIndices_[2];
__constant__ int stage2543GlobalJIndices_[2];
__constant__ int stage2547GlobalIIndices_[2];
__constant__ int stage2547GlobalJIndices_[2];
__constant__ int stage2557GlobalIIndices_[2];
__constant__ int stage2557GlobalJIndices_[2];
__constant__ int stage2577GlobalIIndices_[2];
__constant__ int stage2577GlobalJIndices_[2];
__constant__ int stage2585GlobalIIndices_[2];
__constant__ int stage2585GlobalJIndices_[2];
__constant__ int stage2601GlobalIIndices_[2];
__constant__ int stage2601GlobalJIndices_[2];
__constant__ unsigned globalOffsets_[2];
__device__ bool checkOffset(unsigned int min, unsigned int max, unsigned int val) {
  return (min <= val && val < max);
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128) psyclone_stencil1238_ms4319_kernel(
    const int isize, const int jsize, const int ksize, const int stride_111_1,
    const int stride_111_2, const int stride_110_1, const int tmpBeginIIndex,
    const int tmpBeginJIndex, const int jstride_tmp, const int kstride_tmp,
    ::dawn::float_type* const tmask, ::dawn::float_type* const tsn,
    ::dawn::float_type* const upsmsk, ::dawn::float_type* const rnfmsk_z,
    ::dawn::float_type* const ztfreez, ::dawn::float_type* const rnfmsk,
    gridtools::data_view<TmpStorage> zind_dv) {

  // Start kernel
  ::dawn::float_type* zind = &zind_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zind_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx001 = 0;
  int idx110 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_110_1;
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      ::dawn::float_type __local_zice_1228 = (::dawn::float_type)0.e0;
      if((__ldg(&(tsn[idx111])) <= (__ldg(&(ztfreez[idx110])) + (::dawn::float_type)0.1e0))) {
        __local_zice_1228 = (::dawn::float_type)1.e0;
      } else {
        __local_zice_1228 = (::dawn::float_type)0.e0;
      }
      ::dawn::float_type __local_res__1225 =
          (__ldg(&(rnfmsk[idx110])) * __ldg(&(rnfmsk_z[idx001])));
      ::dawn::float_type __local_tmp__1227 = __ldg(&(upsmsk[idx110]));
      if((__local_tmp__1227 > __local_res__1225)) {
        __local_res__1225 = __local_tmp__1227;
      }
      __local_tmp__1227 = __local_zice_1228;
      if((__local_tmp__1227 > __local_res__1225)) {
        __local_res__1225 = __local_tmp__1227;
      }
      zind_kcache[0] = (__local_res__1225 * __ldg(&(tmask[idx111])));
      zind_kcache[0] = ((int)1 - zind_kcache[0]);
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zind[idx_tmp] = zind_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx001 += 1;
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4320_kernel(const int isize, const int jsize, const int ksize) {

  // Start kernel
  ::dawn::float_type zwx_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators

  // Pre-fill of kcaches
  for(int k = ksize - 1 + 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4321_kernel(const int isize, const int jsize, const int ksize) {

  // Start kernel
  ::dawn::float_type zwy_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators

  // Pre-fill of kcaches
  for(int k = ksize - 1 + 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwy_kcache[0] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128) psyclone_stencil1238_ms4322_kernel(
    const int isize, const int jsize, const int ksize, const int stride_111_1,
    const int stride_111_2, const int tmpBeginIIndex, const int tmpBeginJIndex,
    const int jstride_tmp, const int kstride_tmp, ::dawn::float_type* const vmask,
    ::dawn::float_type* const mydomain, ::dawn::float_type* const umask,
    gridtools::data_view<TmpStorage> zwy_dv, gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwy = &zwy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zwy_kcache[1];
  ::dawn::float_type zwx_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (__ldg(&(umask[idx111])) *
                       (__ldg(&(mydomain[idx111 + 1 * 1])) - __ldg(&(mydomain[idx111]))));
      zwy_kcache[0] = (__ldg(&(vmask[idx111])) * (__ldg(&(mydomain[idx111 + stride_111_1 * 1])) -
                                                  __ldg(&(mydomain[idx111]))));
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwy[idx_tmp] = zwy_kcache[0];
      zwx[idx_tmp] = zwx_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4323_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       ::dawn::float_type* const zslpx_1) {

  // Start kernel
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;

  // jump iterators to match the beginning of next interval
  idx111 += stride_111_2 * (ksize - 1 + 0);

  // Pre-fill of kcaches
  for(int k = ksize - 1 + 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zslpx_1[idx111] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4324_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       ::dawn::float_type* const zslpy_0) {

  // Start kernel
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;

  // jump iterators to match the beginning of next interval
  idx111 += stride_111_2 * (ksize - 1 + 0);

  // Pre-fill of kcaches
  for(int k = ksize - 1 + 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zslpy_0[idx111] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4325_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const zslpy_0,
                                       ::dawn::float_type* const zslpx_1,
                                       gridtools::data_view<TmpStorage> zwy_dv,
                                       gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwy = &zwy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type tmp_sign_1_kcache[1];
  ::dawn::float_type res_sign_1_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type tmp_abs_1_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type res_abs_1_kcache[1];
  ::dawn::float_type res_abs_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)0.25e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = (__ldg(&(zwx[idx_tmp])) * __ldg(&(zwx[idx_tmp + 1 * -1])));
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      zslpx_1[idx111] = ((__ldg(&(zwx[idx_tmp])) + __ldg(&(zwx[idx_tmp + 1 * -1]))) *
                         ((::dawn::float_type)0.25e0 + res_sign_kcache[0]));
      tmp_abs_1_kcache[0] = (::dawn::float_type)0.25e0;
      res_abs_1_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_1_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_1_kcache[0] = tmp_abs_1_kcache[0];
      } else {
        res_abs_1_kcache[0] = (tmp_abs_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_1_kcache[0] = res_abs_1_kcache[0];
      tmp_sign_1_kcache[0] = (__ldg(&(zwy[idx_tmp])) * __ldg(&(zwy[idx_tmp + jstride_tmp * -1])));
      if((tmp_sign_1_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_1_kcache[0] = (res_sign_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      zslpy_0[idx111] = ((__ldg(&(zwy[idx_tmp])) + __ldg(&(zwy[idx_tmp + jstride_tmp * -1]))) *
                         ((::dawn::float_type)0.25e0 + res_sign_1_kcache[0]));
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4326_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const zslpy_0,
                                       gridtools::data_view<TmpStorage> zslpy_dv) {

  // Start kernel
  ::dawn::float_type* zslpy = &zslpy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx111 += max(0, blockIdx.z * 4) * stride_111_2;

  // jump tmp iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx_tmp += max(0, blockIdx.z * 4) * kstride_tmp;
  int kleg_lower_bound = max(0, blockIdx.z * 4);
  int kleg_upper_bound = min(ksize - 1 + -1, (blockIdx.z + 1) * 4 - 1);
  ;
  for(int k = kleg_lower_bound + 0; k <= kleg_upper_bound + 0; ++k) {
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpy_0[idx111] = __ldg(&(zslpy[idx_tmp]));
    }
    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4327_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       ::dawn::float_type* const zslpx,
                                       ::dawn::float_type* const zslpx_1) {

  // Start kernel
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;

  // jump iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx111 += max(0, blockIdx.z * 4) * stride_111_2;
  int kleg_lower_bound = max(0, blockIdx.z * 4);
  int kleg_upper_bound = min(ksize - 1 + -1, (blockIdx.z + 1) * 4 - 1);
  ;
  for(int k = kleg_lower_bound + 0; k <= kleg_upper_bound + 0; ++k) {
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpx_1[idx111] = __ldg(&(zslpx[idx111]));
    }
    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
  }
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128) psyclone_stencil1238_ms4328_kernel(
    const int isize, const int jsize, const int ksize, const int stride_111_1,
    const int stride_111_2, const int tmpBeginIIndex, const int tmpBeginJIndex,
    const int jstride_tmp, const int kstride_tmp, ::dawn::float_type* const zslpy_0,
    ::dawn::float_type* const zslpx_1, gridtools::data_view<TmpStorage> zwy_dv,
    gridtools::data_view<TmpStorage> zwx_dv, gridtools::data_view<TmpStorage> zslpy_dv) {

  // Start kernel
  ::dawn::float_type* zwy = &zwy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zslpy = &zslpy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zslpy_kcache[1];
  ::dawn::float_type zslpx_0_kcache[1];
  ::dawn::float_type tmp_sign_1_kcache[1];
  ::dawn::float_type res_abs_3_kcache[1];
  ::dawn::float_type tmp_abs_3_kcache[1];
  ::dawn::float_type res_min_kcache[1];
  ::dawn::float_type res_abs_2_kcache[1];
  ::dawn::float_type tmp_abs_2_kcache[1];
  ::dawn::float_type res_sign_1_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type tmp_min_kcache[1];
  ::dawn::float_type tmp_abs_1_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  ::dawn::float_type res_abs_1_kcache[1];
  ::dawn::float_type res_abs_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)1.e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = __ldg(&(zslpx_1[idx111]));
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_abs_1_kcache[0] = __ldg(&(zslpx_1[idx111]));
      res_abs_1_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_1_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_1_kcache[0] = tmp_abs_1_kcache[0];
      } else {
        res_abs_1_kcache[0] = (tmp_abs_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_min_kcache[0] = res_abs_1_kcache[0];
      tmp_abs_2_kcache[0] = __ldg(&(zwx[idx_tmp + 1 * -1]));
      res_abs_2_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_2_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_2_kcache[0] = tmp_abs_2_kcache[0];
      } else {
        res_abs_2_kcache[0] = (tmp_abs_2_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_min_kcache[0] = ((::dawn::float_type)2.e0 * res_abs_2_kcache[0]);
      if((tmp_min_kcache[0] < res_min_kcache[0])) {
        res_min_kcache[0] = tmp_min_kcache[0];
      }
      tmp_abs_3_kcache[0] = __ldg(&(zwx[idx_tmp]));
      res_abs_3_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_3_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_3_kcache[0] = tmp_abs_3_kcache[0];
      } else {
        res_abs_3_kcache[0] = (tmp_abs_3_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_min_kcache[0] = ((::dawn::float_type)2.e0 * res_abs_3_kcache[0]);
      if((tmp_min_kcache[0] < res_min_kcache[0])) {
        res_min_kcache[0] = tmp_min_kcache[0];
      }
      zslpx_0_kcache[0] = (res_sign_kcache[0] * res_min_kcache[0]);
      ::dawn::float_type __local_tm_1233 = (::dawn::float_type)1.e0;
      ::dawn::float_type __local_re_1200 = (::dawn::float_type)0.0;
      if((__local_tm_1233 > (::dawn::float_type)0.0)) {
        __local_re_1200 = __local_tm_1233;
      } else {
        __local_re_1200 = (__local_tm_1233 * (::dawn::float_type)-1.0);
      }
      res_sign_1_kcache[0] = __local_re_1200;
      tmp_sign_1_kcache[0] = __ldg(&(zslpy_0[idx111]));
      if((tmp_sign_1_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_1_kcache[0] = (res_sign_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_tm_1224 = __ldg(&(zslpy_0[idx111]));
      ::dawn::float_type __local_re_1218 = (::dawn::float_type)0.0;
      if((__local_tm_1224 > (::dawn::float_type)0.0)) {
        __local_re_1218 = __local_tm_1224;
      } else {
        __local_re_1218 = (__local_tm_1224 * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_re_1215 = __local_re_1218;
      ::dawn::float_type __local_tm_1202 = __ldg(&(zwy[idx_tmp + jstride_tmp * -1]));
      ::dawn::float_type __local_re_1207 = (::dawn::float_type)0.0;
      if((__local_tm_1202 > (::dawn::float_type)0.0)) {
        __local_re_1207 = __local_tm_1202;
      } else {
        __local_re_1207 = (__local_tm_1202 * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_tm_1201 = ((::dawn::float_type)2.e0 * __local_re_1207);
      if((__local_tm_1201 < __local_re_1215)) {
        __local_re_1215 = __local_tm_1201;
      }
      ::dawn::float_type __local_tm_1231 = __ldg(&(zwy[idx_tmp]));
      ::dawn::float_type __local_re_1209 = (::dawn::float_type)0.0;
      if((__local_tm_1231 > (::dawn::float_type)0.0)) {
        __local_re_1209 = __local_tm_1231;
      } else {
        __local_re_1209 = (__local_tm_1231 * (::dawn::float_type)-1.0);
      }
      __local_tm_1201 = ((::dawn::float_type)2.e0 * __local_re_1209);
      if((__local_tm_1201 < __local_re_1215)) {
        __local_re_1215 = __local_tm_1201;
      }
      zslpy_kcache[0] = (res_sign_1_kcache[0] * __local_re_1215);
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpy[idx_tmp] = zslpy_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4329_kernel(const int isize, const int jsize, const int ksize,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       gridtools::data_view<TmpStorage> zdt_dv) {

  // Start kernel
  ::dawn::float_type* zdt = &zdt_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zdt_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock);
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zdt_kcache[0] = (int)1;
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zdt[idx_tmp] = zdt_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += 1;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(160)
    psyclone_stencil1238_ms4330_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const zslpx,
                                       gridtools::data_view<TmpStorage> zslpx_0_dv) {

  // Start kernel
  ::dawn::float_type* zslpx_0 =
      &zslpx_0_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  } else if(threadIdx.y < 5) {
    iblock = threadIdx.x % 1 + 32;
    jblock = (int)threadIdx.x / 1 + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx111 += max(0, blockIdx.z * 4) * stride_111_2;

  // jump tmp iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx_tmp += max(0, blockIdx.z * 4) * kstride_tmp;
  int kleg_lower_bound = max(0, blockIdx.z * 4);
  int kleg_upper_bound = min(ksize - 1 + -1, (blockIdx.z + 1) * 4 - 1);
  ;
  for(int k = kleg_lower_bound + 0; k <= kleg_upper_bound + 0; ++k) {
    if(iblock >= 0 && iblock <= block_size_i - 1 + 1 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpx_0[idx_tmp] = __ldg(&(zslpx[idx111]));
    }
    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128) psyclone_stencil1238_ms4331_kernel(
    const int isize, const int jsize, const int ksize, const int stride_111_1,
    const int stride_111_2, const int tmpBeginIIndex, const int tmpBeginJIndex,
    const int jstride_tmp, const int kstride_tmp, ::dawn::float_type* const pvn,
    ::dawn::float_type* const pun, ::dawn::float_type* const mydomain,
    gridtools::data_view<TmpStorage> zwy_dv, gridtools::data_view<TmpStorage> zwx_dv,
    gridtools::data_view<TmpStorage> zind_dv, gridtools::data_view<TmpStorage> zslpy_dv,
    gridtools::data_view<TmpStorage> zdt_dv, gridtools::data_view<TmpStorage> zslpx_0_dv) {

  // Start kernel
  ::dawn::float_type* zwy = &zwy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zind = &zind_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zslpy = &zslpy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zdt = &zdt_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zslpx_0 =
      &zslpx_0_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zwy_kcache[1];
  ::dawn::float_type zwx_kcache[1];
  ::dawn::float_type zdt_kcache[1];
  ::dawn::float_type zind_kcache[1];
  ::dawn::float_type zzwy_kcache[1];
  ::dawn::float_type res_abs_kcache[1];
  ::dawn::float_type res_abs_1_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  ::dawn::float_type tmp_abs_1_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type res_sign_1_kcache[1];
  ::dawn::float_type zzwx_kcache[1];
  ::dawn::float_type tmp_sign_1_kcache[1];
  ::dawn::float_type zalpha_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zdt_kcache[0] = __ldg(&(zdt[idx_tmp]));
      zind_kcache[0] = __ldg(&(zind[idx_tmp]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)0.5e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = __ldg(&(pun[idx111]));
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_z0u_1237 = res_sign_kcache[0];
      zalpha_kcache[0] = ((::dawn::float_type)0.5e0 - __local_z0u_1237);
      ::dawn::float_type __local_zu_1199 =
          (__local_z0u_1237 -
           (((::dawn::float_type)0.5e0 * __ldg(&(pun[idx111]))) * zdt_kcache[0]));
      zzwx_kcache[0] = (__ldg(&(mydomain[idx111 + 1 * 1])) +
                        (zind_kcache[0] * (__local_zu_1199 * __ldg(&(zslpx_0[idx_tmp + 1 * 1])))));
      zzwy_kcache[0] = (__ldg(&(mydomain[idx111])) +
                        (zind_kcache[0] * (__local_zu_1199 * __ldg(&(zslpx_0[idx_tmp])))));
      zwx_kcache[0] = (__ldg(&(pun[idx111])) *
                       ((zalpha_kcache[0] * zzwx_kcache[0]) +
                        (((::dawn::float_type)1. - zalpha_kcache[0]) * zzwy_kcache[0])));
      tmp_abs_1_kcache[0] = (::dawn::float_type)0.5e0;
      res_abs_1_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_1_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_1_kcache[0] = tmp_abs_1_kcache[0];
      } else {
        res_abs_1_kcache[0] = (tmp_abs_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_1_kcache[0] = res_abs_1_kcache[0];
      tmp_sign_1_kcache[0] = __ldg(&(pvn[idx111]));
      if((tmp_sign_1_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_1_kcache[0] = (res_sign_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_z0v_1236 = res_sign_1_kcache[0];
      zalpha_kcache[0] = ((::dawn::float_type)0.5e0 - __local_z0v_1236);
      ::dawn::float_type __local_zv_1234 =
          (__local_z0v_1236 -
           (((::dawn::float_type)0.5e0 * __ldg(&(pvn[idx111]))) * zdt_kcache[0]));
      zzwx_kcache[0] =
          (__ldg(&(mydomain[idx111 + stride_111_1 * 1])) +
           (zind_kcache[0] * (__local_zv_1234 * __ldg(&(zslpy[idx_tmp + jstride_tmp * 1])))));
      zzwy_kcache[0] = (__ldg(&(mydomain[idx111])) +
                        (zind_kcache[0] * (__local_zv_1234 * __ldg(&(zslpy[idx_tmp])))));
      zwy_kcache[0] = (__ldg(&(pvn[idx111])) *
                       ((zalpha_kcache[0] * zzwx_kcache[0]) +
                        (((::dawn::float_type)1.e0 - zalpha_kcache[0]) * zzwy_kcache[0])));
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwy[idx_tmp] = zwy_kcache[0];
      zwx[idx_tmp] = zwx_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4332_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const mydomain,
                                       gridtools::data_view<TmpStorage> zwy_dv,
                                       gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwy = &zwy_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type mydomain_kcache[1];
  ::dawn::float_type ztra_kcache[1];
  ::dawn::float_type zbtr_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      mydomain_kcache[0] = mydomain[idx111];
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zbtr_kcache[0] = (::dawn::float_type)1.;
      ztra_kcache[0] =
          ((::dawn::float_type)-1.0 *
           (zbtr_kcache[0] *
            (((__ldg(&(zwx[idx_tmp])) - __ldg(&(zwx[idx_tmp + 1 * -1]))) + __ldg(&(zwy[idx_tmp]))) -
             __ldg(&(zwy[idx_tmp + jstride_tmp * -1])))));
      mydomain_kcache[0] = (mydomain_kcache[0] + ztra_kcache[0]);
    }
    // Flush of kcaches

    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      mydomain[idx111] = mydomain_kcache[0];
    }
    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches

  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4333_kernel(const int isize, const int jsize, const int ksize) {

  // Start kernel
  ::dawn::float_type zwx_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4334_kernel(const int isize, const int jsize, const int ksize,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zwx_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock);
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the beginning of next interval
  idx111 += (ksize - 1 + 0);

  // jump tmp iterators to match the beginning of next interval
  idx_tmp += kstride_tmp * (ksize - 1 + 0);

  // Pre-fill of kcaches
  for(int k = ksize - 1 + 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx[idx_tmp] = zwx_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += 1;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4335_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const tmask,
                                       ::dawn::float_type* const mydomain,
                                       gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zwx_kcache[1];
  ::dawn::float_type mydomain_kcache[2];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the beginning of next interval
  idx111 += stride_111_2 * (1);

  // jump tmp iterators to match the beginning of next interval
  idx_tmp += kstride_tmp * (1);

  // Pre-fill of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
    mydomain_kcache[0] = __ldg(&(mydomain[idx111 + stride_111_2 * -1]));
  }
  for(int k = 1 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      mydomain_kcache[1] = __ldg(&(mydomain[idx111]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (__ldg(&(tmask[idx111])) * (mydomain_kcache[0] - mydomain_kcache[1]));
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx[idx_tmp] = zwx_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches
    mydomain_kcache[0] = mydomain_kcache[1];

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4336_kernel(const int isize, const int jsize, const int ksize) {

  // Start kernel
  ::dawn::float_type zslpx_0_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zslpx_0_kcache[0] = (::dawn::float_type)0.e0;
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4337_kernel(const int isize, const int jsize, const int ksize,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       gridtools::data_view<TmpStorage> zwx_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zslpx_0_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type res_abs_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  ::dawn::float_type zwx_kcache[2];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock);
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the beginning of next interval
  idx111 += (1);

  // jump tmp iterators to match the beginning of next interval
  idx_tmp += kstride_tmp * (1);

  // Pre-fill of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
    zwx_kcache[0] = __ldg(&(zwx[idx_tmp]));
  }
  for(int k = 1 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx_kcache[1] = __ldg(&(zwx[idx_tmp + kstride_tmp * 1]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)0.25e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = (zwx_kcache[0] * zwx_kcache[1]);
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      zslpx_0_kcache[0] =
          ((zwx_kcache[0] + zwx_kcache[1]) * ((::dawn::float_type)0.25e0 + res_sign_kcache[0]));
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches
    zwx_kcache[0] = zwx_kcache[1];

    // increment iterators
    idx111 += 1;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4338_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const zslpx,
                                       gridtools::data_view<TmpStorage> zslpx_0_dv) {

  // Start kernel
  ::dawn::float_type* zslpx_0 =
      &zslpx_0_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx111 += max(1, blockIdx.z * 4) * stride_111_2;

  // jump tmp iterators to match the intersection of beginning of next interval and the parallel
  // execution block
  idx_tmp += max(1, blockIdx.z * 4) * kstride_tmp;
  int kleg_lower_bound = max(1, blockIdx.z * 4);
  int kleg_upper_bound = min(ksize - 1 + -1, (blockIdx.z + 1) * 4 - 1);
  ;
  for(int k = kleg_lower_bound + 0; k <= kleg_upper_bound + 0; ++k) {
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpx_0[idx_tmp] = __ldg(&(zslpx[idx111]));
    }
    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4339_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const zslpx,
                                       gridtools::data_view<TmpStorage> zwx_dv,
                                       gridtools::data_view<TmpStorage> zslpx_0_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zslpx_0 =
      &zslpx_0_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type res_abs_1_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  ::dawn::float_type tmp_abs_1_kcache[1];
  ::dawn::float_type tmp_min_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type zwx_kcache[2];
  ::dawn::float_type res_abs_kcache[1];
  ::dawn::float_type tmp_abs_2_kcache[1];
  ::dawn::float_type res_abs_2_kcache[1];
  ::dawn::float_type res_min_kcache[1];
  ::dawn::float_type tmp_abs_3_kcache[1];
  ::dawn::float_type res_abs_3_kcache[1];
  ::dawn::float_type zslpx_0_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the beginning of next interval
  idx111 += stride_111_2 * (1);

  // jump tmp iterators to match the beginning of next interval
  idx_tmp += kstride_tmp * (1);

  // Pre-fill of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
    zwx_kcache[0] = __ldg(&(zwx[idx_tmp]));
  }
  for(int k = 1 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpx_0_kcache[0] = __ldg(&(zslpx_0[idx_tmp]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx_kcache[1] = __ldg(&(zwx[idx_tmp + kstride_tmp * 1]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)1.e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = zslpx_0_kcache[0];
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_abs_1_kcache[0] = zslpx_0_kcache[0];
      res_abs_1_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_1_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_1_kcache[0] = tmp_abs_1_kcache[0];
      } else {
        res_abs_1_kcache[0] = (tmp_abs_1_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_min_kcache[0] = res_abs_1_kcache[0];
      tmp_abs_2_kcache[0] = zwx_kcache[1];
      res_abs_2_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_2_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_2_kcache[0] = tmp_abs_2_kcache[0];
      } else {
        res_abs_2_kcache[0] = (tmp_abs_2_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_min_kcache[0] = ((::dawn::float_type)2.e0 * res_abs_2_kcache[0]);
      if((tmp_min_kcache[0] < res_min_kcache[0])) {
        res_min_kcache[0] = tmp_min_kcache[0];
      }
      tmp_abs_3_kcache[0] = zwx_kcache[0];
      res_abs_3_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_3_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_3_kcache[0] = tmp_abs_3_kcache[0];
      } else {
        res_abs_3_kcache[0] = (tmp_abs_3_kcache[0] * (::dawn::float_type)-1.0);
      }
      tmp_min_kcache[0] = ((::dawn::float_type)2.e0 * res_abs_3_kcache[0]);
      if((tmp_min_kcache[0] < res_min_kcache[0])) {
        res_min_kcache[0] = tmp_min_kcache[0];
      }
      zslpx[idx111] = (res_sign_kcache[0] * res_min_kcache[0]);
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches
    zwx_kcache[0] = zwx_kcache[1];

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4340_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       ::dawn::float_type* const pwn,
                                       ::dawn::float_type* const mydomain) {

  // Start kernel
  ::dawn::float_type zwx_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      zwx_kcache[0] = (__ldg(&(pwn[idx111])) * __ldg(&(mydomain[idx111])));
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += stride_111_2;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4341_kernel(const int isize, const int jsize, const int ksize,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       gridtools::data_view<TmpStorage> zbtr_dv,
                                       gridtools::data_view<TmpStorage> zdt_dv) {

  // Start kernel
  ::dawn::float_type* zbtr = &zbtr_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zdt = &zdt_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zbtr_kcache[1];
  ::dawn::float_type zdt_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock);
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zdt_kcache[0] = (int)1;
      zbtr_kcache[0] = (::dawn::float_type)1.;
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zbtr[idx_tmp] = zbtr_kcache[0];
      zdt[idx_tmp] = zdt_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += 1;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128) psyclone_stencil1238_ms4342_kernel(
    const int isize, const int jsize, const int ksize, const int stride_111_1,
    const int stride_111_2, const int tmpBeginIIndex, const int tmpBeginJIndex,
    const int jstride_tmp, const int kstride_tmp, ::dawn::float_type* const pwn,
    ::dawn::float_type* const zslpx, ::dawn::float_type* const mydomain,
    gridtools::data_view<TmpStorage> zwx_dv, gridtools::data_view<TmpStorage> zind_dv,
    gridtools::data_view<TmpStorage> zbtr_dv, gridtools::data_view<TmpStorage> zdt_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zind = &zind_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zbtr = &zbtr_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zdt = &zdt_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zwx_kcache[1];
  ::dawn::float_type zzwx_kcache[1];
  ::dawn::float_type zalpha_kcache[1];
  ::dawn::float_type tmp_sign_kcache[1];
  ::dawn::float_type res_abs_kcache[1];
  ::dawn::float_type res_sign_kcache[1];
  ::dawn::float_type zslpx_kcache[2];
  ::dawn::float_type zzwy_kcache[1];
  ::dawn::float_type tmp_abs_kcache[1];
  ::dawn::float_type mydomain_kcache[2];
  ::dawn::float_type zind_kcache[1];
  ::dawn::float_type zdt_kcache[1];
  ::dawn::float_type zbtr_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // jump iterators to match the beginning of next interval
  idx111 += stride_111_2 * (1);

  // jump tmp iterators to match the beginning of next interval
  idx_tmp += kstride_tmp * (1);

  // Pre-fill of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
    zslpx_kcache[0] = __ldg(&(zslpx[idx111 + stride_111_2 * -1]));
    mydomain_kcache[0] = __ldg(&(mydomain[idx111 + stride_111_2 * -1]));
  }
  for(int k = 1 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zdt_kcache[0] = __ldg(&(zdt[idx_tmp]));
      zbtr_kcache[0] = __ldg(&(zbtr[idx_tmp]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zind_kcache[0] = __ldg(&(zind[idx_tmp + kstride_tmp * -1]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zslpx_kcache[1] = __ldg(&(zslpx[idx111]));
      mydomain_kcache[1] = __ldg(&(mydomain[idx111]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      tmp_abs_kcache[0] = (::dawn::float_type)0.5e0;
      res_abs_kcache[0] = (::dawn::float_type)0.0;
      if((tmp_abs_kcache[0] > (::dawn::float_type)0.0)) {
        res_abs_kcache[0] = tmp_abs_kcache[0];
      } else {
        res_abs_kcache[0] = (tmp_abs_kcache[0] * (::dawn::float_type)-1.0);
      }
      res_sign_kcache[0] = res_abs_kcache[0];
      tmp_sign_kcache[0] = __ldg(&(pwn[idx111]));
      if((tmp_sign_kcache[0] < (::dawn::float_type)0.0)) {
        res_sign_kcache[0] = (res_sign_kcache[0] * (::dawn::float_type)-1.0);
      }
      ::dawn::float_type __local_z0w_1208 = res_sign_kcache[0];
      zalpha_kcache[0] = ((::dawn::float_type)0.5e0 + __local_z0w_1208);
      ::dawn::float_type __local_zw_1205 =
          (__local_z0w_1208 -
           ((((::dawn::float_type)0.5e0 * __ldg(&(pwn[idx111]))) * zdt_kcache[0]) *
            zbtr_kcache[0]));
      zzwx_kcache[0] =
          (mydomain_kcache[1] + (zind_kcache[0] * (__local_zw_1205 * zslpx_kcache[1])));
      zzwy_kcache[0] =
          (mydomain_kcache[0] + (zind_kcache[0] * (__local_zw_1205 * zslpx_kcache[0])));
      zwx_kcache[0] = (__ldg(&(pwn[idx111])) *
                       ((zalpha_kcache[0] * zzwx_kcache[0]) +
                        (((::dawn::float_type)1. - zalpha_kcache[0]) * zzwy_kcache[0])));
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx[idx_tmp] = zwx_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches
    zslpx_kcache[0] = zslpx_kcache[1];
    mydomain_kcache[0] = mydomain_kcache[1];

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4343_kernel(const int isize, const int jsize, const int ksize,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       gridtools::data_view<TmpStorage> zbtr_dv) {

  // Start kernel
  ::dawn::float_type* zbtr = &zbtr_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type zbtr_kcache[1];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock);
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  for(int k = 0 + 0; k <= ksize - 1 + 0 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zbtr_kcache[0] = (::dawn::float_type)1.;
    }
    // Flush of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zbtr[idx_tmp] = zbtr_kcache[0];
    }
    // Flush of kcaches

    // Slide kcaches

    // increment iterators
    idx111 += 1;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
  }
  // Final flush of kcaches
}
template <typename TmpStorage>
__global__ void __launch_bounds__(128)
    psyclone_stencil1238_ms4344_kernel(const int isize, const int jsize, const int ksize,
                                       const int stride_111_1, const int stride_111_2,
                                       const int tmpBeginIIndex, const int tmpBeginJIndex,
                                       const int jstride_tmp, const int kstride_tmp,
                                       ::dawn::float_type* const mydomain,
                                       gridtools::data_view<TmpStorage> zwx_dv,
                                       gridtools::data_view<TmpStorage> zbtr_dv) {

  // Start kernel
  ::dawn::float_type* zwx = &zwx_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type* zbtr = &zbtr_dv(tmpBeginIIndex, tmpBeginJIndex, blockIdx.x, blockIdx.y, 0);
  ::dawn::float_type ztra_kcache[1];
  ::dawn::float_type zbtr_kcache[1];
  ::dawn::float_type zwx_kcache[2];
  const unsigned int nx = isize;
  const unsigned int ny = jsize;
  const int block_size_i = (blockIdx.x + 1) * 32 < nx ? 32 : nx - blockIdx.x * 32;
  const int block_size_j = (blockIdx.y + 1) * 4 < ny ? 4 : ny - blockIdx.y * 4;

  // computing the global position in the physical domain

  // In a typical cuda block we have the following regions

  // aa bbbbbbbb cc

  // aa bbbbbbbb cc

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // hh dddddddd ii

  // ee ffffffff gg

  // ee ffffffff gg

  // Regions b,d,f have warp (or multiple of warp size)

  // Size of regions a, c, h, i, e, g are determined by max_extent_t

  // Regions b,d,f are easily executed by dedicated warps (one warp for each line)

  // Regions (a,h,e) and (c,i,g) are executed by two specialized warp
  int iblock = 0 - 1;
  int jblock = 0 - 1;
  if(threadIdx.y < +4) {
    iblock = threadIdx.x;
    jblock = (int)threadIdx.y + 0;
  }
  // initialized iterators
  int idx111 = (blockIdx.x * 32 + iblock) * 1 + (blockIdx.y * 4 + jblock) * stride_111_1;
  int idx_tmp = (iblock + 0) * 1 + (jblock + 0) * jstride_tmp;

  // Pre-fill of kcaches
  if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
     jblock <= block_size_j - 1 + 0) {
    zwx_kcache[0] = __ldg(&(zwx[idx_tmp]));
  }
  for(int k = 0 + 0; k <= ksize - 1 + -1 + 0; ++k) {

    // Head fill of kcaches
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zwx_kcache[1] = __ldg(&(zwx[idx_tmp + kstride_tmp * 1]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0) {
      zbtr_kcache[0] = __ldg(&(zbtr[idx_tmp]));
    }
    if(iblock >= 0 && iblock <= block_size_i - 1 + 0 && jblock >= 0 &&
       jblock <= block_size_j - 1 + 0 &&
       checkOffset(stage2421GlobalIIndices_[0], stage2421GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2421GlobalJIndices_[0], stage2421GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2429GlobalIIndices_[0], stage2429GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2429GlobalJIndices_[0], stage2429GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2433GlobalIIndices_[0], stage2433GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2433GlobalJIndices_[0], stage2433GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2437GlobalIIndices_[0], stage2437GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2437GlobalJIndices_[0], stage2437GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2440GlobalIIndices_[0], stage2440GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2440GlobalJIndices_[0], stage2440GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2444GlobalIIndices_[0], stage2444GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2444GlobalJIndices_[0], stage2444GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2448GlobalIIndices_[0], stage2448GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2448GlobalJIndices_[0], stage2448GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2465GlobalIIndices_[0], stage2465GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2465GlobalJIndices_[0], stage2465GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2506GlobalIIndices_[0], stage2506GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2506GlobalJIndices_[0], stage2506GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2527GlobalIIndices_[0], stage2527GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2527GlobalJIndices_[0], stage2527GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2532GlobalIIndices_[0], stage2532GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2532GlobalJIndices_[0], stage2532GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2536GlobalIIndices_[0], stage2536GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2536GlobalJIndices_[0], stage2536GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2540GlobalIIndices_[0], stage2540GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2540GlobalJIndices_[0], stage2540GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2543GlobalIIndices_[0], stage2543GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2543GlobalJIndices_[0], stage2543GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2547GlobalIIndices_[0], stage2547GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2547GlobalJIndices_[0], stage2547GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2557GlobalIIndices_[0], stage2557GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2557GlobalJIndices_[0], stage2557GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2577GlobalIIndices_[0], stage2577GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2577GlobalJIndices_[0], stage2577GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2585GlobalIIndices_[0], stage2585GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2585GlobalJIndices_[0], stage2585GlobalJIndices_[1],
                   globalOffsets_[1] + jblock) &&
       checkOffset(stage2601GlobalIIndices_[0], stage2601GlobalIIndices_[1],
                   globalOffsets_[0] + iblock) &&
       checkOffset(stage2601GlobalJIndices_[0], stage2601GlobalJIndices_[1],
                   globalOffsets_[1] + jblock)) {
      ztra_kcache[0] =
          ((::dawn::float_type)-1.0 * (zbtr_kcache[0] * (zwx_kcache[0] - zwx_kcache[1])));
      mydomain[idx111] = ztra_kcache[0];
    }
    // Flush of kcaches

    // Flush of kcaches

    // Slide kcaches
    zwx_kcache[0] = zwx_kcache[1];

    // increment iterators
    idx111 += stride_111_2;
    idx_tmp += kstride_tmp;
  }
  // Final flush of kcaches

  // Final flush of kcaches

  // Final flush of kcaches
}

class psyclone {
public:
  struct sbase : public timer_cuda {

    sbase(std::string name) : timer_cuda(name) {}

    double get_time() { return total_time(); }
  };

  struct stencil_1238 : public sbase {

    // Members
    std::array<int, 2> stage2421GlobalIIndices;
    std::array<int, 2> stage2421GlobalJIndices;
    std::array<int, 2> stage2429GlobalIIndices;
    std::array<int, 2> stage2429GlobalJIndices;
    std::array<int, 2> stage2433GlobalIIndices;
    std::array<int, 2> stage2433GlobalJIndices;
    std::array<int, 2> stage2437GlobalIIndices;
    std::array<int, 2> stage2437GlobalJIndices;
    std::array<int, 2> stage2440GlobalIIndices;
    std::array<int, 2> stage2440GlobalJIndices;
    std::array<int, 2> stage2444GlobalIIndices;
    std::array<int, 2> stage2444GlobalJIndices;
    std::array<int, 2> stage2448GlobalIIndices;
    std::array<int, 2> stage2448GlobalJIndices;
    std::array<int, 2> stage2465GlobalIIndices;
    std::array<int, 2> stage2465GlobalJIndices;
    std::array<int, 2> stage2506GlobalIIndices;
    std::array<int, 2> stage2506GlobalJIndices;
    std::array<int, 2> stage2527GlobalIIndices;
    std::array<int, 2> stage2527GlobalJIndices;
    std::array<int, 2> stage2532GlobalIIndices;
    std::array<int, 2> stage2532GlobalJIndices;
    std::array<int, 2> stage2536GlobalIIndices;
    std::array<int, 2> stage2536GlobalJIndices;
    std::array<int, 2> stage2540GlobalIIndices;
    std::array<int, 2> stage2540GlobalJIndices;
    std::array<int, 2> stage2543GlobalIIndices;
    std::array<int, 2> stage2543GlobalJIndices;
    std::array<int, 2> stage2547GlobalIIndices;
    std::array<int, 2> stage2547GlobalJIndices;
    std::array<int, 2> stage2557GlobalIIndices;
    std::array<int, 2> stage2557GlobalJIndices;
    std::array<int, 2> stage2577GlobalIIndices;
    std::array<int, 2> stage2577GlobalJIndices;
    std::array<int, 2> stage2585GlobalIIndices;
    std::array<int, 2> stage2585GlobalJIndices;
    std::array<int, 2> stage2601GlobalIIndices;
    std::array<int, 2> stage2601GlobalJIndices;
    std::array<unsigned int, 2> globalOffsets;

    static std::array<unsigned int, 2>
    computeGlobalOffsets(int rank, const gridtools::dawn::domain& dom, int xcols, int ycols) {
      unsigned int rankOnDefaultFace = rank % (xcols * ycols);
      unsigned int row = rankOnDefaultFace / xcols;
      unsigned int col = rankOnDefaultFace % ycols;
      return {col * (dom.isize() - dom.iplus()), row * (dom.jsize() - dom.jplus())};
    }

    // Temporary storage typedefs
    using tmp_halo_t = gridtools::halo<0, 0, 0, 0, 0>;
    using tmp_meta_data_t = storage_traits_t::storage_info_t<0, 5, tmp_halo_t>;
    using tmp_storage_t = storage_traits_t::data_store_t<::dawn::float_type, tmp_meta_data_t>;
    const gridtools::dawn::domain m_dom;

    // temporary storage declarations
    tmp_meta_data_t m_tmp_meta_data;
    tmp_storage_t m_zwy;
    tmp_storage_t m_zwx;
    tmp_storage_t m_zind;
    tmp_storage_t m_zslpy;
    tmp_storage_t m_tmp_sign;
    tmp_storage_t m_res_min;
    tmp_storage_t m_tmp_abs_1;
    tmp_storage_t m_res_abs_1;
    tmp_storage_t m_zbtr;
    tmp_storage_t m_zalpha;
    tmp_storage_t m_zzwx;
    tmp_storage_t m_res_abs_3;
    tmp_storage_t m_res_sign_1;
    tmp_storage_t m_tmp_abs_2;
    tmp_storage_t m_tmp_min;
    tmp_storage_t m_tmp_abs_3;
    tmp_storage_t m_zdt;
    tmp_storage_t m_zzwy;
    tmp_storage_t m_res_abs;
    tmp_storage_t m_tmp_sign_1;
    tmp_storage_t m_ztra;
    tmp_storage_t m_res_abs_2;
    tmp_storage_t m_res_sign;
    tmp_storage_t m_tmp_abs;
    tmp_storage_t m_zslpx_0;

  public:
    stencil_1238(const gridtools::dawn::domain& dom_, int rank, int xcols, int ycols)
        : sbase("stencil_1238"), m_dom(dom_),
          stage2421GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2421GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2429GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2429GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2433GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2433GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2437GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + -1}),
          stage2437GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + -1}),
          stage2440GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2440GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2444GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2444GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2448GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + 0}),
          stage2448GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + 0}),
          stage2465GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + 0}),
          stage2465GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + 0}),
          stage2506GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + -1}),
          stage2506GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + -1}),
          stage2527GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + -1}),
          stage2527GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + -1}),
          stage2532GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2532GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2536GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2536GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2540GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2540GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2543GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2543GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2547GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2547GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2557GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2557GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2577GlobalIIndices({dom_.iminus() + 0, dom_.isize() - dom_.iplus() + 0}),
          stage2577GlobalJIndices({dom_.jminus() + 0, dom_.jsize() - dom_.jplus() + 0}),
          stage2585GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + -1}),
          stage2585GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + -1}),
          stage2601GlobalIIndices({dom_.iminus() + 1, dom_.isize() - dom_.iplus() + -1}),
          stage2601GlobalJIndices({dom_.jminus() + 1, dom_.jsize() - dom_.jplus() + -1}),
          globalOffsets({computeGlobalOffsets(rank, m_dom, xcols, ycols)}),
          m_tmp_meta_data(32 + 1, 4 + 0, (dom_.isize() + 32 - 1) / 32, (dom_.jsize() + 4 - 1) / 4,
                          dom_.ksize() + 2 * 0),
          m_zwy(m_tmp_meta_data), m_zwx(m_tmp_meta_data), m_zind(m_tmp_meta_data),
          m_zslpy(m_tmp_meta_data), m_tmp_sign(m_tmp_meta_data), m_res_min(m_tmp_meta_data),
          m_tmp_abs_1(m_tmp_meta_data), m_res_abs_1(m_tmp_meta_data), m_zbtr(m_tmp_meta_data),
          m_zalpha(m_tmp_meta_data), m_zzwx(m_tmp_meta_data), m_res_abs_3(m_tmp_meta_data),
          m_res_sign_1(m_tmp_meta_data), m_tmp_abs_2(m_tmp_meta_data), m_tmp_min(m_tmp_meta_data),
          m_tmp_abs_3(m_tmp_meta_data), m_zdt(m_tmp_meta_data), m_zzwy(m_tmp_meta_data),
          m_res_abs(m_tmp_meta_data), m_tmp_sign_1(m_tmp_meta_data), m_ztra(m_tmp_meta_data),
          m_res_abs_2(m_tmp_meta_data), m_res_sign(m_tmp_meta_data), m_tmp_abs(m_tmp_meta_data),
          m_zslpx_0(m_tmp_meta_data) {}
    static constexpr ::dawn::driver::cartesian_extent tmask_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent tsn_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent vmask_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent upsmsk_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent pwn_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent pvn_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent rnfmsk_z_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent pun_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent ztfreez_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent zslpx_extent = {0, 1, 0, 0, -1, 0};
    static constexpr ::dawn::driver::cartesian_extent rnfmsk_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent mydomain_extent = {0, 1, 0, 1, -1, 0};
    static constexpr ::dawn::driver::cartesian_extent umask_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent zslpy_0_extent = {0, 0, 0, 0, 0, 0};
    static constexpr ::dawn::driver::cartesian_extent zslpx_1_extent = {0, 0, 0, 0, 0, 0};

    void run(storage_ijk_t tmask_ds, storage_ijk_t tsn_ds, storage_ijk_t vmask_ds,
             storage_ij_t upsmsk_ds, storage_ijk_t pwn_ds, storage_ijk_t pvn_ds,
             storage_k_t rnfmsk_z_ds, storage_ijk_t pun_ds, storage_ij_t ztfreez_ds,
             storage_ijk_t zslpx_ds, storage_ij_t rnfmsk_ds, storage_ijk_t mydomain_ds,
             storage_ijk_t umask_ds, storage_ijk_t zslpy_0_ds, storage_ijk_t zslpx_1_ds) {

      // starting timers
      start();
      {
        ;
        gridtools::data_view<storage_ijk_t> tmask = gridtools::make_device_view(tmask_ds);
        gridtools::data_view<storage_ijk_t> tsn = gridtools::make_device_view(tsn_ds);
        gridtools::data_view<storage_ij_t> upsmsk = gridtools::make_device_view(upsmsk_ds);
        gridtools::data_view<storage_k_t> rnfmsk_z = gridtools::make_device_view(rnfmsk_z_ds);
        gridtools::data_view<storage_ij_t> ztfreez = gridtools::make_device_view(ztfreez_ds);
        gridtools::data_view<storage_ij_t> rnfmsk = gridtools::make_device_view(rnfmsk_ds);
        gridtools::data_view<tmp_storage_t> zind = gridtools::make_device_view(m_zind);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4319_kernel<<<blocks, threads>>>(
            nx, ny, nz, tmask_ds.strides()[1], tmask_ds.strides()[2], upsmsk_ds.strides()[1],
            m_zind.get_storage_info_ptr()->template begin<0>(),
            m_zind.get_storage_info_ptr()->template begin<1>(),
            m_zind.get_storage_info_ptr()->template stride<1>(),
            m_zind.get_storage_info_ptr()->template stride<4>(),
            (tmask.data() +
             tmask_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (tsn.data() + tsn_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (upsmsk.data() +
             upsmsk_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (rnfmsk_z.data() +
             rnfmsk_z_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (ztfreez.data() +
             ztfreez_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (rnfmsk.data() +
             rnfmsk_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zind);
      };
      {
        ;
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4320_kernel<<<blocks, threads>>>(nx, ny, nz);
      };
      {
        ;
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4321_kernel<<<blocks, threads>>>(nx, ny, nz);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> vmask = gridtools::make_device_view(vmask_ds);
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<storage_ijk_t> umask = gridtools::make_device_view(umask_ds);
        gridtools::data_view<tmp_storage_t> zwy = gridtools::make_device_view(m_zwy);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4322_kernel<<<blocks, threads>>>(
            nx, ny, nz, vmask_ds.strides()[1], vmask_ds.strides()[2],
            m_zwy.get_storage_info_ptr()->template begin<0>(),
            m_zwy.get_storage_info_ptr()->template begin<1>(),
            m_zwy.get_storage_info_ptr()->template stride<1>(),
            m_zwy.get_storage_info_ptr()->template stride<4>(),
            (vmask.data() +
             vmask_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (umask.data() +
             umask_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwy, zwx);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpx_1 = gridtools::make_device_view(zslpx_1_ds);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4323_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpx_1_ds.strides()[1], zslpx_1_ds.strides()[2],
            (zslpx_1.data() +
             zslpx_1_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)));
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpy_0 = gridtools::make_device_view(zslpy_0_ds);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4324_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpy_0_ds.strides()[1], zslpy_0_ds.strides()[2],
            (zslpy_0.data() +
             zslpy_0_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)));
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpy_0 = gridtools::make_device_view(zslpy_0_ds);
        gridtools::data_view<storage_ijk_t> zslpx_1 = gridtools::make_device_view(zslpx_1_ds);
        gridtools::data_view<tmp_storage_t> zwy = gridtools::make_device_view(m_zwy);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4325_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpy_0_ds.strides()[1], zslpy_0_ds.strides()[2],
            m_zwy.get_storage_info_ptr()->template begin<0>(),
            m_zwy.get_storage_info_ptr()->template begin<1>(),
            m_zwy.get_storage_info_ptr()->template stride<1>(),
            m_zwy.get_storage_info_ptr()->template stride<4>(),
            (zslpy_0.data() +
             zslpy_0_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (zslpx_1.data() +
             zslpx_1_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwy, zwx);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpy_0 = gridtools::make_device_view(zslpy_0_ds);
        gridtools::data_view<tmp_storage_t> zslpy = gridtools::make_device_view(m_zslpy);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = (m_dom.ksize() + 4 - 1) / 4;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4326_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpy_0_ds.strides()[1], zslpy_0_ds.strides()[2],
            m_zslpy.get_storage_info_ptr()->template begin<0>(),
            m_zslpy.get_storage_info_ptr()->template begin<1>(),
            m_zslpy.get_storage_info_ptr()->template stride<1>(),
            m_zslpy.get_storage_info_ptr()->template stride<4>(),
            (zslpy_0.data() +
             zslpy_0_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zslpy);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpx = gridtools::make_device_view(zslpx_ds);
        gridtools::data_view<storage_ijk_t> zslpx_1 = gridtools::make_device_view(zslpx_1_ds);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = (m_dom.ksize() + 4 - 1) / 4;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4327_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpx_ds.strides()[1], zslpx_ds.strides()[2],
            (zslpx.data() +
             zslpx_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (zslpx_1.data() +
             zslpx_1_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)));
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpy_0 = gridtools::make_device_view(zslpy_0_ds);
        gridtools::data_view<storage_ijk_t> zslpx_1 = gridtools::make_device_view(zslpx_1_ds);
        gridtools::data_view<tmp_storage_t> zwy = gridtools::make_device_view(m_zwy);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        gridtools::data_view<tmp_storage_t> zslpy = gridtools::make_device_view(m_zslpy);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4328_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpy_0_ds.strides()[1], zslpy_0_ds.strides()[2],
            m_zwy.get_storage_info_ptr()->template begin<0>(),
            m_zwy.get_storage_info_ptr()->template begin<1>(),
            m_zwy.get_storage_info_ptr()->template stride<1>(),
            m_zwy.get_storage_info_ptr()->template stride<4>(),
            (zslpy_0.data() +
             zslpy_0_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (zslpx_1.data() +
             zslpx_1_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwy, zwx, zslpy);
      };
      {
        ;
        gridtools::data_view<tmp_storage_t> zdt = gridtools::make_device_view(m_zdt);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4329_kernel<<<blocks, threads>>>(
            nx, ny, nz, m_zdt.get_storage_info_ptr()->template begin<0>(),
            m_zdt.get_storage_info_ptr()->template begin<1>(),
            m_zdt.get_storage_info_ptr()->template stride<1>(),
            m_zdt.get_storage_info_ptr()->template stride<4>(), zdt);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpx = gridtools::make_device_view(zslpx_ds);
        gridtools::data_view<tmp_storage_t> zslpx_0 = gridtools::make_device_view(m_zslpx_0);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 1, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = (m_dom.ksize() + 4 - 1) / 4;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4330_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpx_ds.strides()[1], zslpx_ds.strides()[2],
            m_zslpx_0.get_storage_info_ptr()->template begin<0>(),
            m_zslpx_0.get_storage_info_ptr()->template begin<1>(),
            m_zslpx_0.get_storage_info_ptr()->template stride<1>(),
            m_zslpx_0.get_storage_info_ptr()->template stride<4>(),
            (zslpx.data() +
             zslpx_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zslpx_0);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> pvn = gridtools::make_device_view(pvn_ds);
        gridtools::data_view<storage_ijk_t> pun = gridtools::make_device_view(pun_ds);
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<tmp_storage_t> zwy = gridtools::make_device_view(m_zwy);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        gridtools::data_view<tmp_storage_t> zind = gridtools::make_device_view(m_zind);
        gridtools::data_view<tmp_storage_t> zslpy = gridtools::make_device_view(m_zslpy);
        gridtools::data_view<tmp_storage_t> zdt = gridtools::make_device_view(m_zdt);
        gridtools::data_view<tmp_storage_t> zslpx_0 = gridtools::make_device_view(m_zslpx_0);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4331_kernel<<<blocks, threads>>>(
            nx, ny, nz, pvn_ds.strides()[1], pvn_ds.strides()[2],
            m_zwy.get_storage_info_ptr()->template begin<0>(),
            m_zwy.get_storage_info_ptr()->template begin<1>(),
            m_zwy.get_storage_info_ptr()->template stride<1>(),
            m_zwy.get_storage_info_ptr()->template stride<4>(),
            (pvn.data() + pvn_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (pun.data() + pun_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwy, zwx, zind, zslpy, zdt, zslpx_0);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<tmp_storage_t> zwy = gridtools::make_device_view(m_zwy);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4332_kernel<<<blocks, threads>>>(
            nx, ny, nz, mydomain_ds.strides()[1], mydomain_ds.strides()[2],
            m_zwy.get_storage_info_ptr()->template begin<0>(),
            m_zwy.get_storage_info_ptr()->template begin<1>(),
            m_zwy.get_storage_info_ptr()->template stride<1>(),
            m_zwy.get_storage_info_ptr()->template stride<4>(),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwy, zwx);
      };
      {
        ;
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4333_kernel<<<blocks, threads>>>(nx, ny, nz);
      };
      {
        ;
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4334_kernel<<<blocks, threads>>>(
            nx, ny, nz, m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(), zwx);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> tmask = gridtools::make_device_view(tmask_ds);
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4335_kernel<<<blocks, threads>>>(
            nx, ny, nz, tmask_ds.strides()[1], tmask_ds.strides()[2],
            m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(),
            (tmask.data() +
             tmask_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwx);
      };
      {
        ;
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4336_kernel<<<blocks, threads>>>(nx, ny, nz);
      };
      {
        ;
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4337_kernel<<<blocks, threads>>>(
            nx, ny, nz, m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(), zwx);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpx = gridtools::make_device_view(zslpx_ds);
        gridtools::data_view<tmp_storage_t> zslpx_0 = gridtools::make_device_view(m_zslpx_0);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = (m_dom.ksize() + 4 - 1) / 4;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4338_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpx_ds.strides()[1], zslpx_ds.strides()[2],
            m_zslpx_0.get_storage_info_ptr()->template begin<0>(),
            m_zslpx_0.get_storage_info_ptr()->template begin<1>(),
            m_zslpx_0.get_storage_info_ptr()->template stride<1>(),
            m_zslpx_0.get_storage_info_ptr()->template stride<4>(),
            (zslpx.data() +
             zslpx_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zslpx_0);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> zslpx = gridtools::make_device_view(zslpx_ds);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        gridtools::data_view<tmp_storage_t> zslpx_0 = gridtools::make_device_view(m_zslpx_0);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4339_kernel<<<blocks, threads>>>(
            nx, ny, nz, zslpx_ds.strides()[1], zslpx_ds.strides()[2],
            m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(),
            (zslpx.data() +
             zslpx_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwx, zslpx_0);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> pwn = gridtools::make_device_view(pwn_ds);
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4340_kernel<<<blocks, threads>>>(
            nx, ny, nz, pwn_ds.strides()[1], pwn_ds.strides()[2],
            (pwn.data() + pwn_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)));
      };
      {
        ;
        gridtools::data_view<tmp_storage_t> zbtr = gridtools::make_device_view(m_zbtr);
        gridtools::data_view<tmp_storage_t> zdt = gridtools::make_device_view(m_zdt);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4341_kernel<<<blocks, threads>>>(
            nx, ny, nz, m_zbtr.get_storage_info_ptr()->template begin<0>(),
            m_zbtr.get_storage_info_ptr()->template begin<1>(),
            m_zbtr.get_storage_info_ptr()->template stride<1>(),
            m_zbtr.get_storage_info_ptr()->template stride<4>(), zbtr, zdt);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> pwn = gridtools::make_device_view(pwn_ds);
        gridtools::data_view<storage_ijk_t> zslpx = gridtools::make_device_view(zslpx_ds);
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        gridtools::data_view<tmp_storage_t> zind = gridtools::make_device_view(m_zind);
        gridtools::data_view<tmp_storage_t> zbtr = gridtools::make_device_view(m_zbtr);
        gridtools::data_view<tmp_storage_t> zdt = gridtools::make_device_view(m_zdt);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4342_kernel<<<blocks, threads>>>(
            nx, ny, nz, pwn_ds.strides()[1], pwn_ds.strides()[2],
            m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(),
            (pwn.data() + pwn_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (zslpx.data() +
             zslpx_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwx, zind, zbtr, zdt);
      };
      {
        ;
        gridtools::data_view<tmp_storage_t> zbtr = gridtools::make_device_view(m_zbtr);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4343_kernel<<<blocks, threads>>>(
            nx, ny, nz, m_zbtr.get_storage_info_ptr()->template begin<0>(),
            m_zbtr.get_storage_info_ptr()->template begin<1>(),
            m_zbtr.get_storage_info_ptr()->template stride<1>(),
            m_zbtr.get_storage_info_ptr()->template stride<4>(), zbtr);
      };
      {
        ;
        gridtools::data_view<storage_ijk_t> mydomain = gridtools::make_device_view(mydomain_ds);
        gridtools::data_view<tmp_storage_t> zwx = gridtools::make_device_view(m_zwx);
        gridtools::data_view<tmp_storage_t> zbtr = gridtools::make_device_view(m_zbtr);
        const unsigned int nx = m_dom.isize() - m_dom.iminus() - m_dom.iplus();
        const unsigned int ny = m_dom.jsize() - m_dom.jminus() - m_dom.jplus();
        const unsigned int nz = m_dom.ksize() - m_dom.kminus() - m_dom.kplus();
        dim3 threads(32, 4 + 0, 1);
        const unsigned int nbx = (nx + 32 - 1) / 32;
        const unsigned int nby = (ny + 4 - 1) / 4;
        const unsigned int nbz = 1;
        cudaMemcpyToSymbol(stage2421GlobalIIndices_, stage2421GlobalIIndices.data(),
                           sizeof(int) * stage2421GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2421GlobalJIndices_, stage2421GlobalJIndices.data(),
                           sizeof(int) * stage2421GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalIIndices_, stage2429GlobalIIndices.data(),
                           sizeof(int) * stage2429GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2429GlobalJIndices_, stage2429GlobalJIndices.data(),
                           sizeof(int) * stage2429GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalIIndices_, stage2433GlobalIIndices.data(),
                           sizeof(int) * stage2433GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2433GlobalJIndices_, stage2433GlobalJIndices.data(),
                           sizeof(int) * stage2433GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalIIndices_, stage2437GlobalIIndices.data(),
                           sizeof(int) * stage2437GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2437GlobalJIndices_, stage2437GlobalJIndices.data(),
                           sizeof(int) * stage2437GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalIIndices_, stage2440GlobalIIndices.data(),
                           sizeof(int) * stage2440GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2440GlobalJIndices_, stage2440GlobalJIndices.data(),
                           sizeof(int) * stage2440GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalIIndices_, stage2444GlobalIIndices.data(),
                           sizeof(int) * stage2444GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2444GlobalJIndices_, stage2444GlobalJIndices.data(),
                           sizeof(int) * stage2444GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalIIndices_, stage2448GlobalIIndices.data(),
                           sizeof(int) * stage2448GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2448GlobalJIndices_, stage2448GlobalJIndices.data(),
                           sizeof(int) * stage2448GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalIIndices_, stage2465GlobalIIndices.data(),
                           sizeof(int) * stage2465GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2465GlobalJIndices_, stage2465GlobalJIndices.data(),
                           sizeof(int) * stage2465GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalIIndices_, stage2506GlobalIIndices.data(),
                           sizeof(int) * stage2506GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2506GlobalJIndices_, stage2506GlobalJIndices.data(),
                           sizeof(int) * stage2506GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalIIndices_, stage2527GlobalIIndices.data(),
                           sizeof(int) * stage2527GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2527GlobalJIndices_, stage2527GlobalJIndices.data(),
                           sizeof(int) * stage2527GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalIIndices_, stage2532GlobalIIndices.data(),
                           sizeof(int) * stage2532GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2532GlobalJIndices_, stage2532GlobalJIndices.data(),
                           sizeof(int) * stage2532GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalIIndices_, stage2536GlobalIIndices.data(),
                           sizeof(int) * stage2536GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2536GlobalJIndices_, stage2536GlobalJIndices.data(),
                           sizeof(int) * stage2536GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalIIndices_, stage2540GlobalIIndices.data(),
                           sizeof(int) * stage2540GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2540GlobalJIndices_, stage2540GlobalJIndices.data(),
                           sizeof(int) * stage2540GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalIIndices_, stage2543GlobalIIndices.data(),
                           sizeof(int) * stage2543GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2543GlobalJIndices_, stage2543GlobalJIndices.data(),
                           sizeof(int) * stage2543GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalIIndices_, stage2547GlobalIIndices.data(),
                           sizeof(int) * stage2547GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2547GlobalJIndices_, stage2547GlobalJIndices.data(),
                           sizeof(int) * stage2547GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalIIndices_, stage2557GlobalIIndices.data(),
                           sizeof(int) * stage2557GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2557GlobalJIndices_, stage2557GlobalJIndices.data(),
                           sizeof(int) * stage2557GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalIIndices_, stage2577GlobalIIndices.data(),
                           sizeof(int) * stage2577GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2577GlobalJIndices_, stage2577GlobalJIndices.data(),
                           sizeof(int) * stage2577GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalIIndices_, stage2585GlobalIIndices.data(),
                           sizeof(int) * stage2585GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2585GlobalJIndices_, stage2585GlobalJIndices.data(),
                           sizeof(int) * stage2585GlobalJIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalIIndices_, stage2601GlobalIIndices.data(),
                           sizeof(int) * stage2601GlobalIIndices.size());
        cudaMemcpyToSymbol(stage2601GlobalJIndices_, stage2601GlobalJIndices.data(),
                           sizeof(int) * stage2601GlobalJIndices.size());
        cudaMemcpyToSymbol(globalOffsets_, globalOffsets.data(),
                           sizeof(unsigned) * globalOffsets.size());
        dim3 blocks(nbx, nby, nbz);
        psyclone_stencil1238_ms4344_kernel<<<blocks, threads>>>(
            nx, ny, nz, mydomain_ds.strides()[1], mydomain_ds.strides()[2],
            m_zwx.get_storage_info_ptr()->template begin<0>(),
            m_zwx.get_storage_info_ptr()->template begin<1>(),
            m_zwx.get_storage_info_ptr()->template stride<1>(),
            m_zwx.get_storage_info_ptr()->template stride<4>(),
            (mydomain.data() +
             mydomain_ds.get_storage_info_ptr()->index(m_dom.iminus(), m_dom.jminus(), 0)),
            zwx, zbtr);
      };

      // stopping timers
      pause();
    }
  };
  static constexpr const char* s_name = "psyclone";
  stencil_1238 m_stencil_1238;

public:
  psyclone(const psyclone&) = delete;

  // Members

  // Stencil-Data
  gridtools::dawn::meta_data_t m_meta_data;
  gridtools::dawn::storage_t m_zslpy_0;
  gridtools::dawn::storage_t m_zslpx_1;

  psyclone(const gridtools::dawn::domain& dom, int rank = 1, int xcols = 1, int ycols = 1)
      : m_stencil_1238(dom, rank, xcols, ycols),
        m_meta_data(dom.isize(), dom.jsize(), dom.ksize() /*+ 2 *0*/ + 1),
        m_zslpy_0(m_meta_data, "zslpy_0"), m_zslpx_1(m_meta_data, "zslpx_1") {}

  template <typename S>
  void sync_storages(S field) {
    field.sync();
  }

  template <typename S0, typename... S>
  void sync_storages(S0 f0, S... fields) {
    f0.sync();
    sync_storages(fields...);
  }

  void run(storage_ijk_t tmask, storage_ijk_t tsn, storage_ijk_t vmask, storage_ij_t upsmsk,
           storage_ijk_t pwn, storage_ijk_t pvn, storage_k_t rnfmsk_z, storage_ijk_t pun,
           storage_ij_t ztfreez, storage_ijk_t zslpx, storage_ij_t rnfmsk, storage_ijk_t mydomain,
           storage_ijk_t umask) {
    sync_storages(tmask, tsn, vmask, upsmsk, pwn, pvn, rnfmsk_z, pun, ztfreez, zslpx, rnfmsk,
                  mydomain, umask);
    m_stencil_1238.run(tmask, tsn, vmask, upsmsk, pwn, pvn, rnfmsk_z, pun, ztfreez, zslpx, rnfmsk,
                       mydomain, umask, m_zslpy_0, m_zslpx_1);
    ;
    sync_storages(tmask, tsn, vmask, upsmsk, pwn, pvn, rnfmsk_z, pun, ztfreez, zslpx, rnfmsk,
                  mydomain, umask);
  }

  std::string get_name() const { return std::string(s_name); }

  void reset_meters() { m_stencil_1238.reset(); }

  double get_total_time() {
    double res = 0;
    res += m_stencil_1238.get_time();
    return res;
  }
};
} // namespace cuda
} // namespace dawn_generated

