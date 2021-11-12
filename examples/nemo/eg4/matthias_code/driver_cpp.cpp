#include "wrapper_cpp.h"
#include "wrapper_cuda.h"

int main()
{
  
  int isize = 130;
  int jsize = 130;
  int ksize = 31;  

  // int isize = 31;  
  // int jsize = 31;  
  // int ksize = 31;  

  double * pwn = new double[isize*jsize*ksize];
  double * vmask = new double[isize*jsize*ksize];
  double * mydomain = new double[isize*jsize*ksize];
  double * tmask = new double[isize*jsize*ksize];
  double * umask = new double[isize*jsize*ksize];
  double * tsn = new double[isize*jsize*ksize];
  double * pvn = new double[isize*jsize*ksize];
  double * pun = new double[isize*jsize*ksize];
  double * zslpx = new double[isize*jsize*ksize];
  
  // double * rnfmsk = new double[isize*jsize];
  // double * ztfreez = new double[isize*jsize];
  // double * upsmsk = new double[isize*jsize];
  
  // double * rnfmask_z = new double[ksize];

  double * rnfmsk = new double[isize*jsize*ksize];
  double * ztfreez = new double[isize*jsize*ksize];
  double * upsmsk = new double[isize*jsize*ksize];
  
  double * rnfmask_z = new double[isize*jsize*ksize];

  run_nemo_from_host_cpp(isize, jsize, ksize, ztfreez, pwn, vmask, rnfmsk, mydomain, tmask, umask, tsn, pvn,
	      rnfmask_z, pun, upsmsk, zslpx);

  run_nemo_from_host_cuda(isize, jsize, ksize, ztfreez, pwn, vmask, rnfmsk, mydomain, tmask, umask, tsn, pvn,
	      rnfmask_z, pun, upsmsk, zslpx);

}
