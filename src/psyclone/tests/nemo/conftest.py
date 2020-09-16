import pytest
from psyclone.configuration import Config


@pytest.fixture(name="psyir_backend")
def psyir_backend_fixture(monkeypatch):
    ''' pytest fixture that turns on the use of the PSyIR Fortran backend
    for the NEMO API. '''
    config = Config.get().api_conf("nemo")
    monkeypatch.setattr(config, "use_psyir_backend", True)
    #yield()
    #Config._instance = None
