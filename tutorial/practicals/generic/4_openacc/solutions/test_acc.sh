# Bash script to provide rudimentary testing of generated OpenACC version
# of the tracer advection mini-app.

# Run the mini-app and count the number of kernel launches.
kernel_count=`NV_ACC_NOTIFY=1 JPK=30 JPJ=100 JPI=100 IT=2 ./tra_adv.exe 2>&1 | grep -c 'CUDA kernel'`

# We're expecting 36.
if [ "${kernel_count}" -eq 36 ]; then
    echo "Test passed: ${kernel_count} kernel launches"
else
    echo "Test failed: had ${kernel_count} kernel launches but expected 36"
    exit 1
fi

# Check that the answers match those on the CPU (this requires
# '-Mnofma' with the NVIDIA compiler).
diff --brief ./output.dat solutions/output_100_100_30_2.dat

exit $!
