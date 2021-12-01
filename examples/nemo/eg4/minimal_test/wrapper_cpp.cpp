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
void run_nemo_from_host_cpp(int isize, int jsize, int ksize, double *tsn) {
  
  domain dom(isize, jsize, ksize); 

  printf("domain isize %d\n",dom.isize());
  printf("domain jsize %d\n",dom.jsize());
  printf("domain ksize %d\n",dom.ksize());
  printf("domain iminus %d\n",dom.iminus());
  printf("domain iplus %d\n",dom.iplus());
  printf("domain jminus %d\n",dom.jminus());
  printf("domain jplus %d\n",dom.jplus());
  printf("domain kminus %d\n",dom.kminus());
  printf("domain kplus %d\n",dom.kplus());
  //meta_data_t meta_data_ijk(dom.isize(), dom.jsize(), dom.ksize());
  meta_data_t meta_data_ijk({isize, jsize, ksize}, {isize*jsize, isize, 1});

  // gridtools storages from raw ptrs
  storage_ijk_t tsn_storage(meta_data_ijk, tsn);

  dawn_generated::cxxnaive::psyclone example(dom);

  example.run(tsn_storage);

}
