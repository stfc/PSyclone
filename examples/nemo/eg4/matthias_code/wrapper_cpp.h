#pragma once
extern "C" {
void run_nemo_from_host_cpp(int isize, int jsize, int ksize, 
  double *ztfreez, double *pwn, double *vmask, double *rnfmsk, double *mydomain, double *tmask, double *umask, double *tsn, double *pvn,
	      double *rnfmask_z, double *pun, double *upsmsk, double *zslpx);
}