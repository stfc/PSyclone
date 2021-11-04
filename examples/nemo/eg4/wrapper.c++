#include <driver-includes/gridtools_includes.hpp>
// The include below needs DAWN_GENERATED to be set
#include <driver-includes/storage_runtime.hpp>
#include "res.c++"

using namespace gridtools::dawn;
int main()
{
  
  domain dom(10, 10, 10);
  dom.set_halos(0, 0, 0, 0, 0, 0);

  meta_data_ij_t meta_data_ij(dom.isize(), dom.jsize(), 1);
  meta_data_t meta_data_ijk(dom.isize(), dom.jsize(), dom.ksize());
  meta_data_k_t meta_data_k(1, 1, dom.ksize());
  
  storage_ij_t ztfreez(meta_data_ij, "ztfreez");
  storage_ijk_t pwn(meta_data_ijk, "pwn");
  storage_ijk_t vmask(meta_data_ijk, "vmask");
  storage_ij_t rnfmsk(meta_data_ij, "rnfmsk");
  storage_ijk_t mydomain(meta_data_ijk, "mydomain");
  storage_ijk_t tmask(meta_data_ijk, "tmask");
  storage_ijk_t umask(meta_data_ijk, "umask");
  storage_ijk_t tsn(meta_data_ijk, "tsn");
  storage_ijk_t pvn(meta_data_ijk, "pvn");
  storage_k_t rnfmask_z(meta_data_k, "rnfmask_z");
  storage_ijk_t pun(meta_data_ijk, "pun");
  storage_ij_t upsmsk(meta_data_ij, "upsmsk");
  storage_ijk_t zslpx(meta_data_ijk, "zslpx");

  // Read or set values here

  dawn_generated::cxxnaive::psyclone example(dom);

  example.run(ztfreez, pwn, vmask, rnfmsk, mydomain, tmask, umask, tsn, pvn,
	      rnfmask_z, pun, upsmsk, zslpx);

}
