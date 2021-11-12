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
#include <driver-includes/verify.hpp>

#include "wrapper_cuda.h"

#include "res.cu"

using namespace gridtools::dawn;
void run_nemo_from_host_cuda(int isize, int jsize, int ksize, 
  double *ztfreez, double *pwn, double *vmask, double *rnfmsk, double *mydomain, double *tmask, double *umask, double *tsn, double *pvn,
	      double *rnfmask_z, double *pun, double *upsmsk, double *zslpx) {
  
  domain dom(isize, jsize, ksize); 

  meta_data_ij_t meta_data_ij(dom.isize(), dom.jsize(), 1);
  meta_data_t meta_data_ijk(dom.isize(), dom.jsize(), dom.ksize());
  meta_data_k_t meta_data_k(1, 1, dom.ksize()); 
  
  // gridtools storages from raw ptrs
  storage_ij_t ztfreez_storage(meta_data_ij, ztfreez, gridtools::ownership::external_cpu);
  storage_ijk_t pwn_storage(meta_data_ijk, pwn, gridtools::ownership::external_cpu);
  storage_ijk_t vmask_storage(meta_data_ijk, vmask, gridtools::ownership::external_cpu);
  storage_ij_t rnfmsk_storage(meta_data_ij, rnfmsk, gridtools::ownership::external_cpu);
  storage_ijk_t mydomain_storage(meta_data_ijk, mydomain, gridtools::ownership::external_cpu);
  storage_ijk_t tmask_storage(meta_data_ijk, tmask, gridtools::ownership::external_cpu);
  storage_ijk_t umask_storage(meta_data_ijk, umask, gridtools::ownership::external_cpu);
  storage_ijk_t tsn_storage(meta_data_ijk, tsn, gridtools::ownership::external_cpu);
  storage_ijk_t pvn_storage(meta_data_ijk, pvn, gridtools::ownership::external_cpu);
  storage_k_t rnfmask_z_storage(meta_data_k, rnfmask_z, gridtools::ownership::external_cpu);
  storage_ijk_t pun_storage(meta_data_ijk, pun, gridtools::ownership::external_cpu);
  storage_ij_t upsmsk_storage(meta_data_ij, upsmsk, gridtools::ownership::external_cpu);
  storage_ijk_t zslpx_storage(meta_data_ijk, zslpx, gridtools::ownership::external_cpu);  

  dawn_generated::cuda::psyclone example(dom);

  example.run(ztfreez_storage, pwn_storage, vmask_storage, rnfmsk_storage, mydomain_storage, tmask_storage, umask_storage, tsn_storage, pvn_storage,
	      rnfmask_z_storage, pun_storage, upsmsk_storage, zslpx_storage);

}