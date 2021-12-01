#define DAWN_GENERATED 1
#define GRIDTOOLS_DAWN_HALO_EXTENT 0
#define GT_VECTOR_LIMIT_SIZE 30

#undef FUSION_MAX_VECTOR_SIZE
#undef FUSION_MAX_MAP_SIZE
#define FUSION_MAX_VECTOR_SIZE GT_VECTOR_LIMIT_SIZE
#define FUSION_MAX_MAP_SIZE FUSION_MAX_VECTOR_SIZE
#define BOOST_MPL_LIMIT_VECTOR_SIZE FUSION_MAX_VECTOR_SIZE
#define BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS

#include <driver-includes/gridtools_includes.hpp>
// The include below needs DAWN_GENERATED to be set

#include <driver-includes/storage_runtime.hpp>

#include <gridtools/storage/common/storage_info.hpp>
#include <gridtools/storage/data_store.hpp>
#include <gridtools/storage/storage_host/host_storage.hpp>

#include "wrapper_cpp.h"
#include "tra_adv_compute.cpp"

using namespace gridtools::dawn;
void run_nemo_from_host_cpp(int isize, int jsize, int ksize, 
  double *ztfreez, double *pwn, double *vmask, double *rnfmsk, double *mydomain, double *tmask, double *umask, double *tsn, double *pvn,
	      double *rnfmask_z, double *pun, double *upsmsk, double *zslpx) {
  
  domain dom(isize, jsize, ksize); 

  printf("domain isize %d\n",dom.isize());
  printf("domain jsize %d\n",dom.jsize());
  printf("domain ksize %d\n",dom.ksize());
  meta_data_ij_t meta_data_ij(dom.isize(), dom.jsize(), 1);
  meta_data_t meta_data_ijk({isize, jsize, ksize},{isize*jsize, jsize, 1});
  meta_data_k_t meta_data_k(1, 1, dom.ksize());  

  // gridtools storages from raw ptrs
  storage_ijk_t pwn_storage(meta_data_ijk, pwn);
  storage_ijk_t vmask_storage(meta_data_ijk, vmask);
  storage_ijk_t mydomain_storage(meta_data_ijk, mydomain);
  storage_ijk_t tmask_storage(meta_data_ijk, tmask);
  storage_ijk_t umask_storage(meta_data_ijk, umask);
  storage_ijk_t pvn_storage(meta_data_ijk, pvn);
  storage_ijk_t pun_storage(meta_data_ijk, pun);
  storage_ij_t rnfmsk_storage(meta_data_ij, rnfmsk);
  storage_ij_t ztfreez_storage(meta_data_ij, ztfreez);
  storage_ij_t upsmsk_storage(meta_data_ij, upsmsk);
  storage_k_t rnfmask_z_storage(meta_data_k, rnfmask_z);
  storage_ijk_t tsn_storage(meta_data_ijk, tsn);
  storage_ijk_t zslpx_storage(meta_data_ijk, zslpx);  

  dawn_generated::cxxnaive::psyclone example(dom);

  example.run(ztfreez_storage, pwn_storage, vmask_storage, rnfmsk_storage, mydomain_storage, tmask_storage, umask_storage, tsn_storage, pvn_storage,
	      rnfmask_z_storage, pun_storage, upsmsk_storage, zslpx_storage);

}
