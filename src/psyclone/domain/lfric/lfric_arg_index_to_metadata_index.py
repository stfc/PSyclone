from psyclone.domain.lfric import KernelArgOrder

class LFRicArgIndexToMetadataIndex(KernelArgOrder):
    ''' xxx '''

    @classmethod
    def _initialise(cls, _):
        ''' xxx '''
        super()._initialise({})
        cls._index = 0

    #def __getattr__(cls, name):
    #    def func(*args):
    #        if name in ["_scalar", "_field", ...]:
    #            cls._add_arg(*args)
    #        elif name in ["_cell_position", "_mesh_height", ...]:
    #            cls._index += 1
    #        else:
    #            getattr(cls, name)(*args)
    #    return func

    @classmethod
    def _scalar(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)
        
    @classmethod
    def _field(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _field_vector(cls, meta_arg):
        ''' xxx '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        for idx in range(int(meta_arg.vector_length)):
            cls._add_arg(meta_arg)

    @classmethod
    def _operator(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _cma_operator(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _add_arg(cls, meta_arg):
        ''' xxx '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        cls._info[cls._index] = meta_arg_index
        cls._index += 1

    @classmethod
    def _cell_position(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_height(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d_no_halos(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _cell_map(cls):
        ''' xxx '''
        cls._index += 4

    @classmethod
    def _stencil_2d_unknown_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_2d_max_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_unknown_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_unknown_direction(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_2d(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil(cls, meta_arg):
        ''' xxx '''
        cls._index += 1
