#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk, V. K. Atkinson, STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office


from psyclone.psyGen import Kern
from psyclone.psyir.nodes import (Call, CodeBlock, Routine,
                                  IntrinsicCall)
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.psyir.symbols import DataSymbol, SymbolError
from psyclone.psyGen import BuiltIn


class MarkRoutineForGPUMixin:
    ''' This Mixin provides the "validate_it_can_run_on_gpu" method that
    given a routine or kernel node, it checks that the callee code is valid
    to run on a GPU. It is implemented as a Mixin because transformations
    from multiple programming models, e.g. OpenMP and OpenACC, can reuse
    the same logic.

    '''
    def validate_it_can_run_on_gpu(self, node, options):
        '''
        Check that the supplied node can be marked as available to be
        called on GPU.

        :param node: the kernel or routine to validate.
        :type node: :py:class:`psyclone.psyGen.Kern` |
                    :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether to allow routines with
            CodeBlocks to run on the GPU.
        :param str options["device_string"]: provide a compiler-platform
            identifier.

        :raises TransformationError: if the node is not a kernel or a routine.
        :raises TransformationError: if the target is a built-in kernel.
        :raises TransformationError: if it is a kernel but without an
                                     associated PSyIR.
        :raises TransformationError: if any of the symbols in the kernel are
                                     accessed via a module use statement (and
                                     are not compile-time constants).
        :raises TransformationError: if the routine contains any CodeBlocks.
        :raises TransformationError: if the kernel contains any calls to other
                                     routines.
        '''
        force = options.get("force", False) if options else False
        device_string = options.get("device_string", "") if options else ""

        if not isinstance(node, (Kern, Routine)):
            raise TransformationError(
                f"The {type(self).__name__} must be applied to a sub-class of "
                f"Kern or Routine but got '{type(node).__name__}'.")

        # If it is a kernel call it must have an accessible implementation
        if isinstance(node, BuiltIn):
            raise TransformationError(
                f"Applying {type(self).__name__} to a built-in kernel is not "
                f"yet supported and kernel '{node.name}' is of type "
                f"'{type(node).__name__}'")

        if isinstance(node, Kern):
            # Get the PSyIR routine from the associated kernel. If there is an
            # exception (this could mean that there is no associated tree
            # or that the frontend failed to convert it into PSyIR) reraise it
            # as a TransformationError
            try:
                kernel_schedules = node.get_callees()
            except Exception as error:
                raise TransformationError(
                    f"Failed to create PSyIR for kernel '{node.name}'. "
                    f"Cannot transform such a kernel.") from error

            k_or_r = "Kernel"
        else:
            # Supplied node is a PSyIR Routine which *is* a Schedule.
            kernel_schedules = [node]
            k_or_r = "routine"

        # Check that the routine(s) do(es) not access any data that is
        # imported via a 'use' statement.
        for sched in kernel_schedules:
            vam = sched.reference_accesses()
            ktable = sched.symbol_table
            for sig in vam.all_signatures:
                name = sig.var_name
                first = vam[sig][0].node
                if isinstance(first, Reference):
                    table = ktable
                else:
                    try:
                        table = first.scope.symbol_table
                    except SymbolError:
                        # The node associated with this access is not within a
                        # scoping region.
                        table = ktable
                symbol = table.lookup(name)
                if symbol.is_import:
                    # resolve_type does nothing if the Symbol type is known.
                    try:
                        symbol.resolve_type()
                    except (SymbolError, FileNotFoundError):
                        # TODO #11 - log that we failed to resolve this Symbol.
                        pass
                    if (isinstance(symbol, DataSymbol) and symbol.is_constant):
                        # An import of a compile-time constant is fine.
                        continue
                    raise TransformationError(
                        f"{k_or_r} '{node.name}' accesses the symbol "
                        f"'{symbol}' which is imported. If this symbol "
                        f"represents data then it must first be converted to a"
                        f" {k_or_r} argument using the "
                        f"KernelImportsToArguments transformation.")

            # We forbid CodeBlocks because we can't be certain that what they
            # contain can be executed on a GPU. However, we do permit the user
            # to override this check.
            cblocks = sched.walk(CodeBlock)
            if not force:
                if cblocks:
                    cblock_txt = ("\n  " + "\n  ".join(
                        str(node) for node in cblocks[0].get_ast_nodes)
                                  + "\n")
                    option_txt = "options={'force': True}"
                    raise TransformationError(
                        f"Cannot safely apply {type(self).__name__} to "
                        f"{k_or_r} '{node.name}' because its PSyIR contains "
                        f"one or more CodeBlocks:{cblock_txt}You may use "
                        f"'{option_txt}' to override this check.")

            for call in sched.walk(Call):
                if not call.is_available_on_device(device_string):
                    if isinstance(call, IntrinsicCall):
                        if device_string:
                            device_str = (f"on the '{device_string}' "
                                          f"accelerator device")
                        else:
                            device_str = "on the default accelerator device"
                        raise TransformationError(
                            f"{k_or_r} '{node.name}' calls intrinsic "
                            f"'{call.intrinsic.name}' which is not available "
                            f"{device_str}. Use the 'device_string' option to "
                            f"specify a different device."
                        )
                    call_str = call.debug_string().rstrip("\n")
                    raise TransformationError(
                        f"{k_or_r} '{node.name}' calls another routine "
                        f"'{call_str}' which is not available on the "
                        f"accelerator device and therefore cannot have "
                        f"{type(self).__name__} applied to it (TODO #342).")
