! -----------------------------------------------------------------------------
! CLFORTRAN - OpenCL bindings module for Fortran.
!
! This is the main module file and contains all OpenCL API definitions to be
! invoked from Fortran programs.
!
! -----------------------------------------------------------------------------
!
! Copyright (C) 2013 Company for Advanced Supercomputing Solutions LTD
! Bosmat 2a St.
! Shoham
! Israel 60850
! http://www.cass-hpc.com
!
! Author: Mordechai Butrashvily <support@cass-hpc.com>
!
! -----------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------

module clfortran
    USE ISO_C_BINDING
    implicit none

    ! Error Codes
    integer(c_int32_t), parameter :: CL_SUCCESS                                   =  0
    integer(c_int32_t), parameter :: CL_DEVICE_NOT_FOUND                          = -1
    integer(c_int32_t), parameter :: CL_DEVICE_NOT_AVAILABLE                      = -2
    integer(c_int32_t), parameter :: CL_COMPILER_NOT_AVAILABLE                    = -3
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_ALLOCATION_FAILURE             = -4
    integer(c_int32_t), parameter :: CL_OUT_OF_RESOURCES                          = -5
    integer(c_int32_t), parameter :: CL_OUT_OF_HOST_MEMORY                        = -6
    integer(c_int32_t), parameter :: CL_PROFILING_INFO_NOT_AVAILABLE              = -7
    integer(c_int32_t), parameter :: CL_MEM_COPY_OVERLAP                          = -8
    integer(c_int32_t), parameter :: CL_IMAGE_FORMAT_MISMATCH                     = -9
    integer(c_int32_t), parameter :: CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10
    integer(c_int32_t), parameter :: CL_BUILD_PROGRAM_FAILURE                     = -11
    integer(c_int32_t), parameter :: CL_MAP_FAILURE                               = -12
    integer(c_int32_t), parameter :: CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13
    integer(c_int32_t), parameter :: CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14
    integer(c_int32_t), parameter :: CL_COMPILE_PROGRAM_FAILURE                   = -15
    integer(c_int32_t), parameter :: CL_LINKER_NOT_AVAILABLE                      = -16
    integer(c_int32_t), parameter :: CL_LINK_PROGRAM_FAILURE                      = -17
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_FAILED                   = -18
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19

    integer(c_int32_t), parameter :: CL_INVALID_VALUE                             = -30
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE_TYPE                       = -31
    integer(c_int32_t), parameter :: CL_INVALID_PLATFORM                          = -32
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE                            = -33
    integer(c_int32_t), parameter :: CL_INVALID_CONTEXT                           = -34
    integer(c_int32_t), parameter :: CL_INVALID_QUEUE_PROPERTIES                  = -35
    integer(c_int32_t), parameter :: CL_INVALID_COMMAND_QUEUE                     = -36
    integer(c_int32_t), parameter :: CL_INVALID_HOST_PTR                          = -37
    integer(c_int32_t), parameter :: CL_INVALID_MEM_OBJECT                        = -38
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           = -39
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_SIZE                        = -40
    integer(c_int32_t), parameter :: CL_INVALID_SAMPLER                           = -41
    integer(c_int32_t), parameter :: CL_INVALID_BINARY                            = -42
    integer(c_int32_t), parameter :: CL_INVALID_BUILD_OPTIONS                     = -43
    integer(c_int32_t), parameter :: CL_INVALID_PROGRAM                           = -44
    integer(c_int32_t), parameter :: CL_INVALID_PROGRAM_EXECUTABLE                = -45
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_NAME                       = -46
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_DEFINITION                 = -47
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL                            = -48
    integer(c_int32_t), parameter :: CL_INVALID_ARG_INDEX                         = -49
    integer(c_int32_t), parameter :: CL_INVALID_ARG_VALUE                         = -50
    integer(c_int32_t), parameter :: CL_INVALID_ARG_SIZE                          = -51
    integer(c_int32_t), parameter :: CL_INVALID_KERNEL_ARGS                       = -52
    integer(c_int32_t), parameter :: CL_INVALID_WORK_DIMENSION                    = -53
    integer(c_int32_t), parameter :: CL_INVALID_WORK_GROUP_SIZE                   = -54
    integer(c_int32_t), parameter :: CL_INVALID_WORK_ITEM_SIZE                    = -55
    integer(c_int32_t), parameter :: CL_INVALID_GLOBAL_OFFSET                     = -56
    integer(c_int32_t), parameter :: CL_INVALID_EVENT_WAIT_LIST                   = -57
    integer(c_int32_t), parameter :: CL_INVALID_EVENT                             = -58
    integer(c_int32_t), parameter :: CL_INVALID_OPERATION                         = -59
    integer(c_int32_t), parameter :: CL_INVALID_GL_OBJECT                         = -60
    integer(c_int32_t), parameter :: CL_INVALID_BUFFER_SIZE                       = -61
    integer(c_int32_t), parameter :: CL_INVALID_MIP_LEVEL                         = -62
    integer(c_int32_t), parameter :: CL_INVALID_GLOBAL_WORK_SIZE                  = -63
    integer(c_int32_t), parameter :: CL_INVALID_PROPERTY                          = -64
    integer(c_int32_t), parameter :: CL_INVALID_IMAGE_DESCRIPTOR                  = -65
    integer(c_int32_t), parameter :: CL_INVALID_COMPILER_OPTIONS                  = -66
    integer(c_int32_t), parameter :: CL_INVALID_LINKER_OPTIONS                    = -67
    integer(c_int32_t), parameter :: CL_INVALID_DEVICE_PARTITION_COUNT            = -68
    
    ! OpenCL Version
    integer(c_int32_t), parameter :: CL_VERSION_1_0                               = 1
    integer(c_int32_t), parameter :: CL_VERSION_1_1                               = 1
    integer(c_int32_t), parameter :: CL_VERSION_1_2                               = 1

    ! cl_bool
    integer(c_int32_t), parameter :: CL_FALSE                                     = 0
    integer(c_int32_t), parameter :: CL_TRUE                                      = 1
    integer(c_int32_t), parameter :: CL_BLOCKING                                  = CL_TRUE
    integer(c_int32_t), parameter :: CL_NON_BLOCKING                              = CL_FALSE

    ! cl_platform_info
    integer(c_int32_t), parameter :: CL_PLATFORM_PROFILE                        = Z'0900'
    integer(c_int32_t), parameter :: CL_PLATFORM_VERSION                        = Z'0901'
    integer(c_int32_t), parameter :: CL_PLATFORM_NAME                           = Z'0902'
    integer(c_int32_t), parameter :: CL_PLATFORM_VENDOR                         = Z'0903'
    integer(c_int32_t), parameter :: CL_PLATFORM_EXTENSIONS                     = Z'0904'

    ! cl_device_type - bitfield
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_DEFAULT                     = b'00001'
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_CPU                         = b'00010'
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_GPU                         = b'00100'
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_ACCELERATOR                 = b'01000'
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_CUSTOM                      = b'10000'
    integer(c_int64_t), parameter :: CL_DEVICE_TYPE_ALL                         = Z'FFFFFFFF'

    ! cl_device_info
    integer(c_int32_t), parameter :: CL_DEVICE_TYPE                             = Z'1000'
    integer(c_int32_t), parameter :: CL_DEVICE_VENDOR_ID                        = Z'1001'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_COMPUTE_UNITS                = Z'1002'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS         = Z'1003'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_GROUP_SIZE              = Z'1004'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WORK_ITEM_SIZES              = Z'1005'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR      = Z'1006'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT     = Z'1007'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT       = Z'1008'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG      = Z'1009'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT     = Z'100A'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE    = Z'100B'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CLOCK_FREQUENCY              = Z'100C'
    integer(c_int32_t), parameter :: CL_DEVICE_ADDRESS_BITS                     = Z'100D'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_READ_IMAGE_ARGS              = Z'100E'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_WRITE_IMAGE_ARGS             = Z'100F'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_MEM_ALLOC_SIZE               = Z'1010'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE2D_MAX_WIDTH                = Z'1011'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE2D_MAX_HEIGHT               = Z'1012'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_WIDTH                = Z'1013'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_HEIGHT               = Z'1014'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE3D_MAX_DEPTH                = Z'1015'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_SUPPORT                    = Z'1016'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_PARAMETER_SIZE               = Z'1017'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_SAMPLERS                     = Z'1018'
    integer(c_int32_t), parameter :: CL_DEVICE_MEM_BASE_ADDR_ALIGN              = Z'1019'
    integer(c_int32_t), parameter :: CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE         = Z'101A'
    integer(c_int32_t), parameter :: CL_DEVICE_SINGLE_FP_CONFIG                 = Z'101B'
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHE_TYPE            = Z'101C'
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE        = Z'101D'
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_CACHE_SIZE            = Z'101E'
    integer(c_int32_t), parameter :: CL_DEVICE_GLOBAL_MEM_SIZE                  = Z'101F'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE         = Z'1020'
    integer(c_int32_t), parameter :: CL_DEVICE_MAX_CONSTANT_ARGS                = Z'1021'
    integer(c_int32_t), parameter :: CL_DEVICE_LOCAL_MEM_TYPE                   = Z'1022'
    integer(c_int32_t), parameter :: CL_DEVICE_LOCAL_MEM_SIZE                   = Z'1023'
    integer(c_int32_t), parameter :: CL_DEVICE_ERROR_CORRECTION_SUPPORT         = Z'1024'
    integer(c_int32_t), parameter :: CL_DEVICE_PROFILING_TIMER_RESOLUTION       = Z'1025'
    integer(c_int32_t), parameter :: CL_DEVICE_ENDIAN_LITTLE                    = Z'1026'
    integer(c_int32_t), parameter :: CL_DEVICE_AVAILABLE                        = Z'1027'
    integer(c_int32_t), parameter :: CL_DEVICE_COMPILER_AVAILABLE               = Z'1028'
    integer(c_int32_t), parameter :: CL_DEVICE_EXECUTION_CAPABILITIES           = Z'1029'
    integer(c_int32_t), parameter :: CL_DEVICE_QUEUE_PROPERTIES                 = Z'102A'
    integer(c_int32_t), parameter :: CL_DEVICE_NAME                             = Z'102B'
    integer(c_int32_t), parameter :: CL_DEVICE_VENDOR                           = Z'102C'
    integer(c_int32_t), parameter :: CL_DRIVER_VERSION                          = Z'102D'
    integer(c_int32_t), parameter :: CL_DEVICE_PROFILE                          = Z'102E'
    integer(c_int32_t), parameter :: CL_DEVICE_VERSION                          = Z'102F'
    integer(c_int32_t), parameter :: CL_DEVICE_EXTENSIONS                       = Z'1030'
    integer(c_int32_t), parameter :: CL_DEVICE_PLATFORM                         = Z'1031'
    integer(c_int32_t), parameter :: CL_DEVICE_DOUBLE_FP_CONFIG                 = Z'1032'
    ! 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF      = Z'1034'
    integer(c_int32_t), parameter :: CL_DEVICE_HOST_UNIFIED_MEMORY              = Z'1035'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR         = Z'1036'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT        = Z'1037'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_INT          = Z'1038'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG         = Z'1039'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT        = Z'103A'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE       = Z'103B'
    integer(c_int32_t), parameter :: CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF         = Z'103C'
    integer(c_int32_t), parameter :: CL_DEVICE_OPENCL_C_VERSION                 = Z'103D'
    integer(c_int32_t), parameter :: CL_DEVICE_LINKER_AVAILABLE                 = Z'103E'
    integer(c_int32_t), parameter :: CL_DEVICE_BUILT_IN_KERNELS                 = Z'103F'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_MAX_BUFFER_SIZE            = Z'1040'
    integer(c_int32_t), parameter :: CL_DEVICE_IMAGE_MAX_ARRAY_SIZE             = Z'1041'
    integer(c_int32_t), parameter :: CL_DEVICE_PARENT_DEVICE                    = Z'1042'
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_MAX_SUB_DEVICES        = Z'1043'
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_PROPERTIES             = Z'1044'
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_AFFINITY_DOMAIN        = Z'1045'
    integer(c_int32_t), parameter :: CL_DEVICE_PARTITION_TYPE                   = Z'1046'
    integer(c_int32_t), parameter :: CL_DEVICE_REFERENCE_COUNT                  = Z'1047'
    integer(c_int32_t), parameter :: CL_DEVICE_PREFERRED_INTEROP_USER_SYNC      = Z'1048'
    integer(c_int32_t), parameter :: CL_DEVICE_PRINTF_BUFFER_SIZE               = Z'1049'

    ! cl_device_fp_config - bitfield
    integer(c_int64_t), parameter :: CL_FP_DENORM                               = b'00000001'
    integer(c_int64_t), parameter :: CL_FP_INF_NAN                              = b'00000010'
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_NEAREST                     = b'00000100'
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_ZERO                        = b'00001000'
    integer(c_int64_t), parameter :: CL_FP_ROUND_TO_INF                         = b'00010000'
    integer(c_int64_t), parameter :: CL_FP_FMA                                  = b'00100000'
    integer(c_int64_t), parameter :: CL_FP_SOFT_FLOAT                           = b'01000000'
    integer(c_int64_t), parameter :: CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT        = b'10000000'

    ! cl_device_mem_cache_type
    integer(c_int32_t), parameter :: CL_NONE                                    = Z'0'
    integer(c_int32_t), parameter :: CL_READ_ONLY_CACHE                         = Z'1'
    integer(c_int32_t), parameter :: CL_READ_WRITE_CACHE                        = Z'2'

    ! cl_device_local_mem_type
    integer(c_int32_t), parameter :: CL_LOCAL                                   = Z'1'
    integer(c_int32_t), parameter :: CL_GLOBAL                                  = Z'2'

    ! cl_device_exec_capabilities - bitfield
    integer(c_int64_t), parameter :: CL_EXEC_KERNEL                             = b'01'
    integer(c_int64_t), parameter :: CL_EXEC_NATIVE_KERNEL                      = b'10'

    ! cl_command_queue_properties - bitfield
    integer(c_int64_t), parameter :: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE     = b'01'
    integer(c_int64_t), parameter :: CL_QUEUE_PROFILING_ENABLE                  = b'10'

    ! cl_context_info
    integer(c_int32_t), parameter :: CL_CONTEXT_REFERENCE_COUNT                 = Z'1080'
    integer(c_int32_t), parameter :: CL_CONTEXT_DEVICES                         = Z'1081'
    integer(c_int32_t), parameter :: CL_CONTEXT_PROPERTIES                      = Z'1082'
    integer(c_int32_t), parameter :: CL_CONTEXT_NUM_DEVICES                     = Z'1083'

    ! cl_context_properties type(c_ptr)
    integer(c_intptr_t), parameter :: CL_CONTEXT_PLATFORM                         = Z'1084'
    integer(c_intptr_t), parameter :: CL_CONTEXT_INTEROP_USER_SYNC                = Z'1085'

    ! cl_command_queue_info
    integer(c_int32_t), parameter :: CL_QUEUE_CONTEXT                           = Z'1090'
    integer(c_int32_t), parameter :: CL_QUEUE_DEVICE                            = Z'1091'
    integer(c_int32_t), parameter :: CL_QUEUE_REFERENCE_COUNT                   = Z'1092'
    integer(c_int32_t), parameter :: CL_QUEUE_PROPERTIES                        = Z'1093'

    ! cl_mem_flags - bitfield (int64)
    integer(c_int64_t), parameter :: CL_MEM_READ_WRITE                          = b'0000000001'
    integer(c_int64_t), parameter :: CL_MEM_WRITE_ONLY                          = b'0000000010'
    integer(c_int64_t), parameter :: CL_MEM_READ_ONLY                           = b'0000000100'
    integer(c_int64_t), parameter :: CL_MEM_USE_HOST_PTR                        = b'0000001000'
    integer(c_int64_t), parameter :: CL_MEM_ALLOC_HOST_PTR                      = b'0000010000'
    integer(c_int64_t), parameter :: CL_MEM_COPY_HOST_PTR                       = b'0000100000'
    !integer(c_int64_t), parameter :: reserved                                  = b'0001000000'
    integer(c_int64_t), parameter :: CL_MEM_HOST_WRITE_ONLY                     = b'0010000000'
    integer(c_int64_t), parameter :: CL_MEM_HOST_READ_ONLY                      = b'0100000000'
    integer(c_int64_t), parameter :: CL_MEM_HOST_NO_ACCESS                      = b'1000000000'

    ! cl_buffer_create_type
    integer(c_int32_t), parameter :: CL_BUFFER_CREATE_TYPE_REGION               = Z'1220'

    ! cl_channel_order
    integer(c_int32_t), parameter :: CL_R                                       = Z'10B0'
    integer(c_int32_t), parameter :: CL_A                                       = Z'10B1'
    integer(c_int32_t), parameter :: CL_RG                                      = Z'10B2'
    integer(c_int32_t), parameter :: CL_RA                                      = Z'10B3'
    integer(c_int32_t), parameter :: CL_RGB                                     = Z'10B4'
    integer(c_int32_t), parameter :: CL_RGBA                                    = Z'10B5'
    integer(c_int32_t), parameter :: CL_BGRA                                    = Z'10B6'
    integer(c_int32_t), parameter :: CL_ARGB                                    = Z'10B7'
    integer(c_int32_t), parameter :: CL_INTENSITY                               = Z'10B8'
    integer(c_int32_t), parameter :: CL_LUMINANCE                               = Z'10B9'
    integer(c_int32_t), parameter :: CL_Rx                                      = Z'10BA'
    integer(c_int32_t), parameter :: CL_RGx                                     = Z'10BB'
    integer(c_int32_t), parameter :: CL_RGBx                                    = Z'10BC'
    integer(c_int32_t), parameter :: CL_DEPTH                                   = Z'10BD'
    integer(c_int32_t), parameter :: CL_DEPTH_STENCIL                           = Z'10BE'

    ! cl_channel_type
    integer(c_int32_t), parameter :: CL_SNORM_INT8                              = Z'10D0'
    integer(c_int32_t), parameter :: CL_SNORM_INT16                             = Z'10D1'
    integer(c_int32_t), parameter :: CL_UNORM_INT8                              = Z'10D2'
    integer(c_int32_t), parameter :: CL_UNORM_INT16                             = Z'10D3'
    integer(c_int32_t), parameter :: CL_UNORM_SHORT_565                         = Z'10D4'
    integer(c_int32_t), parameter :: CL_UNORM_SHORT_555                         = Z'10D5'
    integer(c_int32_t), parameter :: CL_UNORM_INT_101010                        = Z'10D6'
    integer(c_int32_t), parameter :: CL_SIGNED_INT8                             = Z'10D7'
    integer(c_int32_t), parameter :: CL_SIGNED_INT16                            = Z'10D8'
    integer(c_int32_t), parameter :: CL_SIGNED_INT32                            = Z'10D9'
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT8                           = Z'10DA'
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT16                          = Z'10DB'
    integer(c_int32_t), parameter :: CL_UNSIGNED_INT32                          = Z'10DC'
    integer(c_int32_t), parameter :: CL_HALF_FLOAT                              = Z'10DD'
    integer(c_int32_t), parameter :: CL_FLOAT                                   = Z'10DE'
    integer(c_int32_t), parameter :: CL_UNORM_INT24                             = Z'10DF'

    ! cl_mem_object_type
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_BUFFER                       = Z'10F0'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE2D                      = Z'10F1'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE3D                      = Z'10F2'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE2D_ARRAY                = Z'10F3'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D                      = Z'10F4'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D_ARRAY                = Z'10F5'
    integer(c_int32_t), parameter :: CL_MEM_OBJECT_IMAGE1D_BUFFER               = Z'10F6'
    
    ! cl_mem_info
    integer(c_int32_t), parameter :: CL_MEM_TYPE                                = Z'1100'
    integer(c_int32_t), parameter :: CL_MEM_FLAGS                               = Z'1101'
    integer(c_int32_t), parameter :: CL_MEM_SIZE                                = Z'1102'
    integer(c_int32_t), parameter :: CL_MEM_HOST_PTR                            = Z'1103'
    integer(c_int32_t), parameter :: CL_MEM_MAP_COUNT                           = Z'1104'
    integer(c_int32_t), parameter :: CL_MEM_REFERENCE_COUNT                     = Z'1105'
    integer(c_int32_t), parameter :: CL_MEM_CONTEXT                             = Z'1106'
    integer(c_int32_t), parameter :: CL_MEM_ASSOCIATED_MEMOBJECT                = Z'1107'
    integer(c_int32_t), parameter :: CL_MEM_OFFSET                              = Z'1108'

    ! cl_image_info - Note that INFO was added to resolve naming conflicts.
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_FORMAT                       = Z'1110'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ELEMENT_SIZE                 = Z'1111'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ROW_PITCH                    = Z'1112'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_SLICE_PITCH                  = Z'1113'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_WIDTH                        = Z'1114'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_HEIGHT                       = Z'1115'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_DEPTH                        = Z'1116'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_ARRAY_SIZE                   = Z'1117'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_BUFFER                       = Z'1118'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_NUM_MIP_LEVELS               = Z'1119'
    integer(c_int32_t), parameter :: CL_IMAGE_INFO_NUM_SAMPLES                  = Z'111A'
    
    ! cl_addressing_mode
    integer(c_int32_t), parameter :: CL_ADDRESS_NONE                            = Z'1130'
    integer(c_int32_t), parameter :: CL_ADDRESS_CLAMP_TO_EDGE                   = Z'1131'
    integer(c_int32_t), parameter :: CL_ADDRESS_CLAMP                           = Z'1132'
    integer(c_int32_t), parameter :: CL_ADDRESS_REPEAT                          = Z'1133'
    integer(c_int32_t), parameter :: CL_ADDRESS_MIRRORED_REPEAT                 = Z'1134'

    ! cl_filter_mode
    integer(c_int32_t), parameter :: CL_FILTER_NEAREST                          = Z'1140'
    integer(c_int32_t), parameter :: CL_FILTER_LINEAR                           = Z'1141'

    ! cl_sampler_info
    integer(c_int32_t), parameter :: CL_SAMPLER_REFERENCE_COUNT                 = Z'1150'
    integer(c_int32_t), parameter :: CL_SAMPLER_CONTEXT                         = Z'1151'
    integer(c_int32_t), parameter :: CL_SAMPLER_NORMALIZED_COORDS               = Z'1152'
    integer(c_int32_t), parameter :: CL_SAMPLER_ADDRESSING_MODE                 = Z'1153'
    integer(c_int32_t), parameter :: CL_SAMPLER_FILTER_MODE                     = Z'1154'
    
    ! cl_program_info
    integer(c_int32_t), parameter :: CL_PROGRAM_REFERENCE_COUNT                 = Z'1160'
    integer(c_int32_t), parameter :: CL_PROGRAM_CONTEXT                         = Z'1161'
    integer(c_int32_t), parameter :: CL_PROGRAM_NUM_DEVICES                     = Z'1162'
    integer(c_int32_t), parameter :: CL_PROGRAM_DEVICES                         = Z'1163'
    integer(c_int32_t), parameter :: CL_PROGRAM_SOURCE                          = Z'1164'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_SIZES                    = Z'1165'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARIES                        = Z'1166'
    integer(c_int32_t), parameter :: CL_PROGRAM_NUM_KERNELS                     = Z'1167'
    integer(c_int32_t), parameter :: CL_PROGRAM_KERNEL_NAMES                    = Z'1168'

    ! cl_program_build_info
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_STATUS                    = Z'1181'
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_OPTIONS                   = Z'1182'
    integer(c_int32_t), parameter :: CL_PROGRAM_BUILD_LOG                       = Z'1183'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE                     = Z'1184'
    
    ! cl_program_binary_type
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_NONE                = Z'0'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT     = Z'1'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_LIBRARY             = Z'2'
    integer(c_int32_t), parameter :: CL_PROGRAM_BINARY_TYPE_EXECUTABLE          = Z'4'

    ! cl_build_status
    integer(c_int32_t), parameter :: CL_BUILD_SUCCESS                           = 0
    integer(c_int32_t), parameter :: CL_BUILD_NONE                              = -1
    integer(c_int32_t), parameter :: CL_BUILD_ERROR                             = -2
    integer(c_int32_t), parameter :: CL_BUILD_IN_PROGRESS                       = -3
    
    ! cl_kernel_info
    integer(c_int32_t), parameter :: CL_KERNEL_FUNCTION_NAME                    = Z'1190'
    integer(c_int32_t), parameter :: CL_KERNEL_NUM_ARGS                         = Z'1191'
    integer(c_int32_t), parameter :: CL_KERNEL_REFERENCE_COUNT                  = Z'1192'
    integer(c_int32_t), parameter :: CL_KERNEL_CONTEXT                          = Z'1193'
    integer(c_int32_t), parameter :: CL_KERNEL_PROGRAM                          = Z'1194'
    integer(c_int32_t), parameter :: CL_KERNEL_ATTRIBUTES                       = Z'1195'

    ! cl_kernel_arg_info
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_QUALIFIER            = Z'1196'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_QUALIFIER             = Z'1197'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_TYPE_NAME                    = Z'1198'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_TYPE_QUALIFIER               = Z'1199'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_NAME                         = Z'119A'

    ! cl_kernel_arg_address_qualifier
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_GLOBAL               = Z'119B'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_LOCAL                = Z'119C'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_CONSTANT             = Z'119D'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ADDRESS_PRIVATE              = Z'119E'

    ! cl_kernel_arg_access_qualifier
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_READ_ONLY             = Z'11A0'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_WRITE_ONLY            = Z'11A1'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_READ_WRITE            = Z'11A2'
    integer(c_int32_t), parameter :: CL_KERNEL_ARG_ACCESS_NONE                  = Z'11A3'
    
    ! cl_kernel_arg_type_qualifer - bitfield (int64)
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_NONE                    = b'000'
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_CONST                   = b'001'
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_RESTRICT                = b'010'
    integer(c_int64_t), parameter :: CL_KERNEL_ARG_TYPE_VOLATILE                = b'100'

    ! cl_kernel_work_group_info
    integer(c_int32_t), parameter :: CL_KERNEL_WORK_GROUP_SIZE                  = Z'11B0'
    integer(c_int32_t), parameter :: CL_KERNEL_COMPILE_WORK_GROUP_SIZE          = Z'11B1'
    integer(c_int32_t), parameter :: CL_KERNEL_LOCAL_MEM_SIZE                   = Z'11B2'
    integer(c_int32_t), parameter :: CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE= Z'11B3'
    integer(c_int32_t), parameter :: CL_KERNEL_PRIVATE_MEM_SIZE                 = Z'11B4'
    integer(c_int32_t), parameter :: CL_KERNEL_GLOBAL_WORK_SIZE                 = Z'11B5'
    
    ! cl_event_info
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_QUEUE                     = Z'11D0'
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_TYPE                      = Z'11D1'
    integer(c_int32_t), parameter :: CL_EVENT_REFERENCE_COUNT                   = Z'11D2'
    integer(c_int32_t), parameter :: CL_EVENT_COMMAND_EXECUTION_STATUS          = Z'11D3'
    integer(c_int32_t), parameter :: CL_EVENT_CONTEXT                           = Z'11D4'
    
    ! cl_profiling_info
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_QUEUED                = Z'1280'
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_SUBMIT                = Z'1281'
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_START                 = Z'1282'
    integer(c_int32_t), parameter :: CL_PROFILING_COMMAND_END                   = Z'1283'
    
    ! ------------
    ! Types
    ! ------------
    
    type, BIND(C) :: cl_image_format 
        integer(c_int32_t) :: image_channel_order
        integer(c_int32_t) :: image_channel_data_type
    end type
    
    type, BIND(C) :: cl_image_desc
        integer(c_int32_t)  :: image_type
        integer(c_size_t)   :: image_width
        integer(c_size_t)   :: image_height
        integer(c_size_t)   :: image_depth
        integer(c_size_t)   :: image_array_size
        integer(c_size_t)   :: image_row_pitch
        integer(c_size_t)   :: image_slice_pitch
        integer(c_int32_t)  :: num_mip_levels
        integer(c_int32_t)  :: num_samples
        integer(c_intptr_t) :: buffer
    end type
    

    !
    ! Start interfaces.
    !
    contains

    ! ------------
    ! Platform API
    ! ------------

    ! clGetPlatformIDs
        integer(c_int32_t) function clGetPlatformIDs(num_entries, &
                platforms, num_platforms) &
            BIND(C, NAME='clGetPlatformIDs')
            USE ISO_C_BINDING

            integer(c_int32_t), value, intent(in)   :: num_entries
            type(c_ptr), value, intent(in)          :: platforms
            integer(c_int32_t), intent(out)         :: num_platforms
        end function

    ! clGetPlatformInfo
        integer(c_int32_t) function clGetPlatformInfo(platform, param_name, &
                param_value_size, param_value, param_value_size_ret) &
            BIND(C, NAME='clGetPlatformInfo')
            USE ISO_C_BINDING

            integer(c_intptr_t), value, intent(in)      :: platform
            integer(c_int32_t), value, intent(in)       :: param_name
            integer(c_size_t), value, intent(in)        :: param_value_size
            type(c_ptr), value, intent(in)              :: param_value
            integer(c_size_t), intent(out)              :: param_value_size_ret
        end function

    ! ----------
    ! Device API
    ! ----------

    ! clGetDeviceIDs
        integer(c_int32_t) function clGetDeviceIDs(platform, &
                device_type, &
                num_entries, &
                devices, &
                num_devices) &
            BIND(C, NAME='clGetDeviceIDs')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: platform
            integer(c_int64_t), value  :: device_type
            integer(c_int32_t), value  :: num_entries
            type(c_ptr), value         :: devices
            integer(c_int32_t), intent(out) :: num_devices

        end function

    ! clGetDeviceInfo
        integer(c_int) function clGetDeviceInfo(device, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetDeviceInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clCreateSubDevices
        integer(c_int32_t) function clCreateSubDevices(in_device, &
                properties, &
                num_devices, &
                out_devices, &
                num_devices_ret) &
            BIND(C, NAME='clCreateSubDevices')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: in_device
            type(c_ptr), value         :: properties
            integer(c_int32_t), value  :: num_devices
            type(c_ptr), value         :: out_devices
            integer(c_int32_t), intent(out) :: num_devices_ret

        end function

    ! clRetainDevice
        integer(c_int32_t) function clRetainDevice(device) &
            BIND(C, NAME='clRetainDevice')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
        end function

    ! clReleaseDevice
        integer(c_int32_t) function clReleaseDevice(device) &
            BIND(C, NAME='clReleaseDevice')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: device
        end function

    ! ------------
    ! Context APIs
    ! ------------

    ! clCreateContext
        integer(c_intptr_t) function clCreateContext(properties, &
                num_devices, &
                devices, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clCreateContext')
            USE ISO_C_BINDING

            ! Define parameters.
            type(c_ptr), value        :: properties
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value        :: devices
            type(c_funptr), value     :: pfn_notify
            type(c_ptr), value        :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateContextFromType
        integer(c_intptr_t) function clCreateContextFromType(properties, &
                device_type, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clCreateContextFromType')
            USE ISO_C_BINDING

            ! Define parameters.
            type(c_ptr), value        :: properties
            integer(c_int64_t), value :: device_type
            type(c_funptr), value     :: pfn_notify
            type(c_ptr), value        :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainContext
        integer(c_int32_t) function clRetainContext(context) &
            BIND(C, NAME='clRetainContext')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context

        end function

    ! clReleaseContext
        integer(c_int32_t) function clReleaseContext(context) &
            BIND(C, NAME='clReleaseContext')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context

        end function

    ! clGetContextInfo
        integer(c_int32_t) function clGetContextInfo(context, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetContextInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: param_name
            integer(c_size_t), value :: param_value_size
            type(c_ptr), value :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ------------------
    ! Command Queue APIs
    ! ------------------

    ! clCreateCommandQueue
        integer(c_intptr_t) function clCreateCommandQueue(context, &
                device, &
                properties, &
                errcode_ret) &
            BIND(C, NAME='clCreateCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_intptr_t), value :: device
            integer(c_int64_t), value  :: properties
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clRetainCommandQueue
        integer(c_int32_t) function clRetainCommandQueue(command_queue) &
            BIND(C, NAME='clRetainCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clReleaseCommandQueue
        integer(c_int32_t) function clReleaseCommandQueue(command_queue) &
            BIND(C, NAME='clReleaseCommandQueue')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clGetCommandQueueInfo
        integer(c_int32_t) function clGetCommandQueueInfo(command_queue, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetCommandQueueInfo')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ------------------
    ! Memory Object APIs
    ! ------------------

    ! clCreateBuffer
        integer(c_intptr_t) function clCreateBuffer(context, &
                flags, &
                sizeb, &
                host_ptr, &
                errcode_ret) &
            BIND(C, NAME='clCreateBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            integer(c_size_t), value    :: sizeb
            type(c_ptr), value          :: host_ptr
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateSubBuffer
        integer(c_intptr_t) function clCreateSubBuffer(buffer, &
                flags, &
                buffer_create_type, &
                buffer_create_info, &
                errcode_ret) &
            BIND(C, NAME='clCreateSubBuffer')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: buffer
            integer(c_int64_t), value   :: flags
            integer(c_int32_t), value   :: buffer_create_type
            type(c_ptr), value          :: buffer_create_info
            integer(c_int32_t), intent(out) :: errcode_ret

        end function

    ! clCreateImage
        integer(c_intptr_t) function clCreateImage(context, &
                flags, &
                image_format, &
                image_desc, &
                host_ptr, &
                errcode_ret) &
            BIND(C, NAME='clCreateImage')
            USE ISO_C_BINDING

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            type(cl_image_format)       :: image_format
            type(cl_image_desc)         :: image_desc
            type(c_ptr), value          :: host_ptr
            integer(c_int32_t), intent(out) :: errcode_ret

        end function
                        
    ! clRetainMemObject
        integer(c_int32_t) function clRetainMemObject(mem_obj) &
            BIND(C, NAME='clRetainMemObject')

            ! Define parameters.
            integer(c_intptr_t), value  :: mem_obj

        end function

    ! clReleaseMemObject
        integer(c_int32_t) function clReleaseMemObject(mem_obj) &
            BIND(C, NAME='clReleaseMemObject')

            ! Define parameters.
            integer(c_intptr_t), value  :: mem_obj

        end function

    ! clGetSupportedImageFormats
        integer(c_int32_t) function clGetSupportedImageFormats(context, &
                flags, &
                image_type, &
                num_entries, &
                image_formats, &
                num_image_formats) &
            BIND(C, NAME='clGetSupportedImageFormats')

            ! Define parameters.
            integer(c_intptr_t), value  :: context
            integer(c_int64_t), value   :: flags
            integer(c_int32_t), value   :: image_type
            integer(c_int32_t), value   :: num_entries
            type(c_ptr), value          :: image_formats
            integer(c_int32_t), intent(out) :: num_image_formats

        end function
        
    ! clGetMemObjectInfo
        integer(c_int32_t) function clGetMemObjectInfo(memobj, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetMemObjectInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: memobj
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function
        
    ! clGetImageInfo
        integer(c_int32_t) function clGetImageInfo(image, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetImageInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clSetMemObjectDestructorCallback
        integer(c_int32_t) function clSetMemObjectDestructorCallback(memobj, &
                 pfn_notify, &
                 user_data) &
            BIND(C, NAME='clSetMemObjectDestructorCallback')

            ! Define parameters.
            integer(c_intptr_t), value  :: memobj
            type(c_funptr), value       :: pfn_notify
            type(c_ptr), value          :: user_data

        end function

    ! ------------
    ! Sampler APIs
    ! ------------
    
    ! clCreateSampler
        integer(c_intptr_t) function clCreateSampler(context, &
                normalized_coords, &
                addressing_mode, &
                filter_mode, &
                errcode_ret) &
            BIND(C, NAME='clCreateSampler')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: normalized_coords
            integer(c_int32_t), value :: addressing_mode 
            integer(c_int32_t), value :: filter_mode
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function
        
    ! clRetainSampler
        integer(c_int32_t) function clRetainSampler(sampler) &
            BIND(C, NAME='clRetainSampler')
            
            ! Define parameters.
            integer(c_intptr_t), value :: sampler
            
        end function

    ! clReleaseSampler
        integer(c_int32_t) function clReleaseSampler(sampler) &
            BIND(C, NAME='clReleaseSampler')
            
            ! Define parameters.
            integer(c_intptr_t), value :: sampler
            
        end function
        
    ! clGetSamplerInfo
        integer(c_int32_t) function clGetSamplerInfo(sampler, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetSamplerInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: sampler
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function
        
    ! -------------------
    ! Program Object APIs
    ! -------------------
    
    ! clCreateProgramWithSource
        integer(c_intptr_t) function clCreateProgramWithSource(context, &
                count, &
                strings, &
                lengths, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithSource')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: count
            type(c_ptr), value :: strings
            type(c_ptr), value :: lengths
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clCreateProgramWithBinary
        integer(c_intptr_t) function clCreateProgramWithBinary(context, &
                num_devices, &
                device_list, &
                lengths, &
                binaries, &
                binary_status, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithBinary')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: lengths
            type(c_ptr), value :: binaries
            type(c_ptr), value :: binary_status
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clCreateProgramWithBuiltInKernels
        integer(c_intptr_t) function clCreateProgramWithBuiltInKernels(context, &
                num_devices, &
                device_list, &
                kernel_names, &
                errcode_ret) &
            BIND(C, NAME='clCreateProgramWithBuiltInKernels')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: kernel_names
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clRetainProgram
        integer(c_int32_t) function clRetainProgram(program) &
            BIND(C, NAME='clRetainProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            
        end function

    ! clReleaseProgram
        integer(c_int32_t) function clReleaseProgram(program) &
            BIND(C, NAME='clReleaseProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            
        end function

    ! clBuildProgram
        integer(c_int32_t) function clBuildProgram(program, &
                num_devices, &
                device_list, &
                options, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clBuildProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            
        end function

    ! clCompileProgram
        integer(c_int32_t) function clCompileProgram(program, &
                num_devices, &
                device_list, &
                options, &
                num_input_headers, &
                input_headers, &
                header_include_names, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clCompileProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            integer(c_int32_t), value :: num_input_headers
            type(c_ptr), value :: input_headers
            type(c_ptr), value :: header_include_names
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            
        end function

    ! clLinkProgram
        integer(c_intptr_t) function clLinkProgram(context, &
                num_devices, &
                device_list, &
                options, &
                num_input_programs, &
                input_programs, &
                pfn_notify, &
                user_data, &
                errcode_ret) &
            BIND(C, NAME='clLinkProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            integer(c_int32_t), value :: num_devices
            type(c_ptr), value :: device_list
            type(c_ptr), value :: options
            integer(c_int32_t), value :: num_input_programs
            type(c_ptr), value :: input_programs
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clUnloadPlatformCompiler
        integer(c_int32_t) function clUnloadPlatformCompiler(platform) &
            BIND(C, NAME='clUnloadPlatformCompiler')
            
            ! Define parameters.
            integer(c_intptr_t), value :: platform
            
        end function

    ! clGetProgramInfo
        integer(c_int32_t) function clGetProgramInfo(program, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetProgramInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetProgramBuildInfo
        integer(c_int32_t) function clGetProgramBuildInfo(program, &
                 device, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetProgramBuildInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_intptr_t), value :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ------------------
    ! Kernel Object APIs
    ! ------------------
    
    ! clCreateKernel
        integer(c_intptr_t) function clCreateKernel(program, &
                kernel_name, &
                errcode_ret) &
            BIND(C, NAME='clCreateKernel')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            type(c_ptr), value :: kernel_name
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clCreateKernelsInProgram
        integer(c_int32_t) function clCreateKernelsInProgram(program, &
                num_kernels, &
                kernels, &
                num_kernels_ret) &
            BIND(C, NAME='clCreateKernelsInProgram')
            
            ! Define parameters.
            integer(c_intptr_t), value :: program
            integer(c_int32_t), value :: num_kernels
            type(c_ptr), value :: kernels
            integer(c_int32_t), intent(out) :: num_kernels_ret
            
        end function

    ! clRetainKernel
        integer(c_int32_t) function clRetainKernel(kernel) &
            BIND(C, NAME='clRetainKernel')
            
            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            
        end function

    ! clReleaseKernel
        integer(c_int32_t) function clReleaseKernel(kernel) &
            BIND(C, NAME='clReleaseKernel')
            
            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            
        end function
        
    ! clSetKernelArg
        integer(c_int32_t) function clSetKernelArg(kernel, &
                arg_index, &
                arg_size, &
                arg_value) &
            BIND(C, NAME='clSetKernelArg')
            
            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: arg_index
            integer(c_size_t), value :: arg_size
            type(c_ptr), value :: arg_value
            
        end function

    ! clGetKernelInfo
        integer(c_int32_t) function clGetKernelInfo(kernel, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetKernelInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetKernelArgInfo
        integer(c_int32_t) function clGetKernelArgInfo(kernel, &
                arg_index, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetKernelArgInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value  :: arg_index
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clGetKernelWorkGroupInfo
        integer(c_int32_t) function clGetKernelWorkGroupInfo(kernel, &
                device, &
                param_name, &
                param_value_size, &
                param_value, &
                param_value_size_ret) &
            BIND(C, NAME='clGetKernelWorkGroupInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: kernel
            integer(c_intptr_t), value  :: device
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! -----------------
    ! Event Object APIs
    ! -----------------
    
    ! clWaitForEvents
        integer(c_int32_t) function clWaitForEvents(num_events, &
                event_list) &
            BIND(C, NAME='clWaitForEvents')
            
            ! Define parameters.
            integer(c_int32_t), value :: num_events
            type(c_ptr), value :: event_list
            
        end function

    ! clGetEventInfo
        integer(c_int32_t) function clGetEventInfo(event, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetEventInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! clCreateUserEvent
        integer(c_intptr_t) function clCreateUserEvent(context, &
                errcode_ret) &
            BIND(C, NAME='clCreateUserEvent')
            
            ! Define parameters.
            integer(c_int32_t), value :: context
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clRetainEvent
        integer(c_int32_t) function clRetainEvent(event) &
            BIND(C, NAME='clRetainEvent')
            
            ! Define parameters.
            integer(c_intptr_t), value :: event
            
        end function

    ! clReleaseEvent
        integer(c_int32_t) function clReleaseEvent(event) &
            BIND(C, NAME='clReleaseEvent')
            
            ! Define parameters.
            integer(c_intptr_t), value :: event
            
        end function

    ! clSetUserEventStatus
        integer(c_int32_t) function clSetUserEventStatus(event, &
                execution_status) &
            BIND(C, NAME='clSetUserEventStatus')
            
            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value :: execution_status
            
        end function

    ! clSetEventCallback
        integer(c_int32_t) function clSetEventCallback(event, &
                command_exec_callback_type, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clSetEventCallback')
            
            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value :: command_exec_callback_type
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            
        end function

    ! --------------
    ! Profiling APIs
    ! --------------

    ! clGetEventProfilingInfo
        integer(c_int32_t) function clGetEventProfilingInfo(event, &
                 param_name, &
                 param_value_size, &
                 param_value, &
                 param_value_size_ret) &
            BIND(C, NAME='clGetEventProfilingInfo')

            ! Define parameters.
            integer(c_intptr_t), value :: event
            integer(c_int32_t), value  :: param_name
            integer(c_size_t), value   :: param_value_size
            type(c_ptr), value         :: param_value
            integer(c_size_t), intent(out) :: param_value_size_ret

        end function

    ! ---------------------
    ! Flush and Finish APIs
    ! ---------------------

    ! clFlush
        integer(c_int32_t) function clFlush(command_queue) &
            BIND(C, NAME='clFlush')

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function

    ! clFinish
        integer(c_int32_t) function clFinish(command_queue) &
            BIND(C, NAME='clFinish')

            ! Define parameters.
            integer(c_intptr_t), value :: command_queue

        end function
    
    ! ----------------------
    ! Enqueued Commands APIs
    ! ----------------------
    
    ! clEnqueueReadBuffer
        integer(c_int32_t) function clEnqueueReadBuffer(command_queue, &
                buffer, &
                blocking_read, &
                offset, &
                size, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_read
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function
    
    ! clEnqueueReadBufferRect
        integer(c_int32_t) function clEnqueueReadBufferRect(command_queue, &
                buffer, &
                blocking_read, &
                buffer_offset, &
                host_offset, &
                region, &
                buffer_row_pitch, &
                buffer_slice_pitch, &
                host_row_pitch, &
                host_slice_pitch, &                      
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadBufferRect')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_read
            type(c_ptr), value :: buffer_offset
            type(c_ptr), value :: host_offset
            type(c_ptr), value :: region
            integer(c_size_t), value :: buffer_row_pitch
            integer(c_size_t), value :: buffer_slice_pitch
            integer(c_size_t), value :: host_row_pitch
            integer(c_size_t), value :: host_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueWriteBuffer
        integer(c_int32_t) function clEnqueueWriteBuffer(command_queue, &
                buffer, &
                blocking_write, &
                offset, &
                size, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_write
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueWriteBufferRect
        integer(c_int32_t) function clEnqueueWriteBufferRect(command_queue, &
                buffer, &
                blocking_write, &
                buffer_offset, &
                host_offset, &
                region, &
                buffer_row_pitch, &
                buffer_slice_pitch, &
                host_row_pitch, &
                host_slice_pitch, &                      
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteBufferRect')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_write
            type(c_ptr), value :: buffer_offset
            type(c_ptr), value :: host_offset
            type(c_ptr), value :: region
            integer(c_size_t), value :: buffer_row_pitch
            integer(c_size_t), value :: buffer_slice_pitch
            integer(c_size_t), value :: host_row_pitch
            integer(c_size_t), value :: host_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueFillBuffer
        integer(c_int32_t) function clEnqueueFillBuffer(command_queue, &
                buffer, &
                pattern, &
                pattern_size, &
                offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueFillBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            type(c_ptr), value :: pattern
            integer(c_size_t), value :: pattern_size
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueCopyBuffer
        integer(c_int32_t) function clEnqueueCopyBuffer(command_queue, &
                src_buffer, &
                dst_buffer, &
                src_offset, &
                dst_offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_buffer
            integer(c_size_t), value :: src_offset
            integer(c_size_t), value :: dst_offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueCopyBufferRect
        integer(c_int32_t) function clEnqueueCopyBufferRect(command_queue, &
                src_buffer, &
                dst_buffer, &
                src_origin, &
                dst_origin, &
                region, &
                src_row_pitch, &
                src_slice_pitch, &
                dst_row_pitch, &
                dst_slice_pitch, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBufferRect')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_buffer
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: src_row_pitch
            integer(c_size_t), value :: src_slice_pitch
            integer(c_size_t), value :: dst_row_pitch
            integer(c_size_t), value :: dst_slice_pitch
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueReadImage
        integer(c_int32_t) function clEnqueueReadImage(command_queue, &
                image, &
                blocking_read, &
                origin, &
                region, &
                row_pitch, &
                slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueReadImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_read
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: row_pitch
            integer(c_size_t), value :: slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueWriteImage
        integer(c_int32_t) function clEnqueueWriteImage(command_queue, &
                image, &
                blocking_write, &
                origin, &
                region, &
                input_row_pitch, &
                input_slice_pitch, &
                ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueWriteImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_write
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: input_row_pitch
            integer(c_size_t), value :: input_slice_pitch
            type(c_ptr), value :: ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueFillImage
        integer(c_int32_t) function clEnqueueFillImage(command_queue, &
                image, &
                fill_color, &
                origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueFillImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            type(c_ptr), value :: fill_color
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueCopyImage
        integer(c_int32_t) function clEnqueueCopyImage(command_queue, &
                src_image, &
                dst_image, &
                src_origin, &
                dst_origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_image
            integer(c_intptr_t), value :: dst_image
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueCopyImageToBuffer
        integer(c_int32_t) function clEnqueueCopyImageToBuffer(command_queue, &
                src_image, &
                dst_buffer, &
                src_origin, &
                region, &
                dst_offset, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyImageToBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_image
            integer(c_intptr_t), value :: dst_buffer
            type(c_ptr), value :: src_origin
            type(c_ptr), value :: region
            integer(c_size_t), value :: dst_offset
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueCopyBufferToImage
        integer(c_int32_t) function clEnqueueCopyBufferToImage(command_queue, &
                src_buffer, &
                dst_image, &
                src_offset, &
                dst_origin, &
                region, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueCopyBufferToImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: src_buffer
            integer(c_intptr_t), value :: dst_image
            integer(c_size_t), value :: src_offset
            type(c_ptr), value :: dst_origin
            type(c_ptr), value :: region
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueMapBuffer
        type(c_ptr) function clEnqueueMapBuffer(command_queue, &
                buffer, &
                blocking_map, &
                map_flags, &
                offset, &
                size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event, &
                errcode_ret) &
            BIND(C, NAME='clEnqueueMapBuffer')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: buffer
            integer(c_int32_t), value :: blocking_map
            integer(c_int64_t), value :: map_flags
            integer(c_size_t), value :: offset
            integer(c_size_t), value :: size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clEnqueueMapImage
        type(c_ptr) function clEnqueueMapImage(command_queue, &
                image, &
                blocking_map, &
                map_flags, &
                origin, &
                region, &
                image_row_pitch, &
                image_slice_pitch, &
                num_events_in_wait_list, &
                event_wait_list, &
                event, &
                errcode_ret) &
            BIND(C, NAME='clEnqueueMapImage')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: image
            integer(c_int32_t), value :: blocking_map
            integer(c_int64_t), value :: map_flags
            type(c_ptr), value :: origin
            type(c_ptr), value :: region
            integer(c_size_t), intent(out) :: image_row_pitch
            integer(c_size_t), intent(out) :: image_slice_pitch
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            integer(c_int32_t), intent(out) :: errcode_ret
            
        end function

    ! clEnqueueUnmapMemObject
        integer(c_int32_t) function clEnqueueUnmapMemObject(command_queue, &
                memobj, &
                mapped_ptr, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueUnmapMemObject')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: memobj
            type(c_ptr), value :: mapped_ptr
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueMigrateMemObjects
        integer(c_int32_t) function clEnqueueMigrateMemObjects(command_queue, &
                num_mem_objects, &
                mem_objects, &
                flags, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueMigrateMemObjects')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_mem_objects
            type(c_ptr), value :: mem_objects
            integer(c_int64_t), value :: flags
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueNDRangeKernel.
        integer(c_int32_t) function clEnqueueNDRangeKernel(command_queue, &
                kernel, &
                work_dim, &
                global_work_offset, &
                global_work_size, &
                local_work_size, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueNDRangeKernel')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: work_dim
            type(c_ptr), value :: global_work_offset
            type(c_ptr), value :: global_work_size
            type(c_ptr), value :: local_work_size
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueTask
        integer(c_int32_t) function clEnqueueTask(command_queue, &
                kernel, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueTask')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_intptr_t), value :: kernel
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueNativeKernel
        integer(c_int32_t) function clEnqueueNativeKernel(command_queue, &
                user_func, &
                args, &
                cb_args, &
                num_mem_objects, &
                mem_list, &
                args_mem_loc, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueNativeKernel')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            type(c_funptr), value :: user_func
            type(c_ptr), value :: args
            integer(c_size_t), value :: cb_args
            integer(c_int32_t), value :: num_mem_objects
            type(c_ptr), value :: mem_list
            type(c_ptr), value :: args_mem_loc
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueMarkerWithWaitList
        integer(c_int32_t) function clEnqueueMarkerWithWaitList(command_queue, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueMarkerWithWaitList')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clEnqueueMarkerWithWaitList
        integer(c_int32_t) function clEnqueueBarrierWithWaitList(command_queue, &
                num_events_in_wait_list, &
                event_wait_list, &
                event) &
            BIND(C, NAME='clEnqueueBarrierWithWaitList')
            
            ! Define parameters.
            integer(c_intptr_t), value :: command_queue
            integer(c_int32_t), value :: num_events_in_wait_list
            type(c_ptr), value :: event_wait_list
            type(c_ptr), value :: event
            
        end function

    ! clSetPrintfCallback
        integer(c_int32_t) function clSetPrintfCallback(context, &
                pfn_notify, &
                user_data) &
            BIND(C, NAME='clSetPrintfCallback')
            
            ! Define parameters.
            integer(c_intptr_t), value :: context
            type(c_funptr), value :: pfn_notify
            type(c_ptr), value :: user_data
            
        end function
    
    ! -------------------------
    ! Extension function access
    ! -------------------------
        type(c_funptr) function clGetExtensionFunctionAddressForPlatform(platform, &
                func_name) &
            BIND(C, NAME='clGetExtensionFunctionAddressForPlatform')
            
            ! Define parameters.
            integer(c_intptr_t), value :: platform
            type(c_ptr), value :: func_name
            
        end function
            
    !
    !end interface
    !

end module clfortran
