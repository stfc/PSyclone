#define DAWN_GENERATED 1
#undef DAWN_BACKEND_T
#define DAWN_BACKEND_T CXXNAIVE
#ifndef BOOST_RESULT_OF_USE_TR1
#define BOOST_RESULT_OF_USE_TR1 1
#endif
#ifndef BOOST_NO_CXX11_DECLTYPE
#define BOOST_NO_CXX11_DECLTYPE 1
#endif
#ifndef GRIDTOOLS_DAWN_HALO_EXTENT
#define GRIDTOOLS_DAWN_HALO_EXTENT 0
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
namespace cxxnaive {

class psyclone {

public:

  gridtools::dawn::meta_data_t m_meta_data;
  const gridtools::dawn::domain m_dom;
  
  psyclone(const psyclone&) = delete;

  psyclone(const gridtools::dawn::domain& dom, int rank = 1, int xcols = 1, int ycols = 1)
    : m_dom(dom), m_meta_data(dom.isize(), dom.jsize(), dom.ksize()) {
    printf("Hello from constructor\n");
  }

  void run(storage_ijk_t tsn_) {
      int iMin = m_dom.iminus();
      int iMax = m_dom.isize() - m_dom.iplus() - 1;
      int jMin = m_dom.jminus();
      int jMax = m_dom.jsize() - m_dom.jplus() - 1;
      int kMin = m_dom.kminus();
      int kMax = m_dom.ksize() - m_dom.kplus() - 1;
      printf("HELLO from cpp run method\n");
      printf("%d %d %d %d %d %d\n",iMin,iMax,jMin,jMax,kMin,kMax);
      {
        gridtools::data_view<storage_ijk_t> tsn = gridtools::make_host_view(tsn_);
        std::array<int, 3> tsn_offsets{0, 0, 0};
	tsn_.sync();
	for (int k=0; k<=kMax; k++){
	for (int j=0; j<=jMax; j++){
	for (int i=0; i<=iMax; i++){
	  printf("tsn(%d,%d,%d) = %.16f\n",i,j,k,tsn(i,j,k));
	}
	}
	}
	tsn_.sync();
      }
  }
};
} // namespace cxxnaive
} // namespace dawn_generated

