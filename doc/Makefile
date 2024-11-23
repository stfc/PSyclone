
all:
	make -C developer_guide html SPHINXOPTS="-W --keep-going"
	make -C developer_guide linkcheck || echo "Ignoring error of link checking"
	make -C reference_guide html SPHINXOPTS="-W --keep-going"
	make -C user_guide html SPHINXOPTS="-W --keep-going"

clean:
	make -C developer_guide clean
	make -C reference_guide allclean
	make -C user_guide clean

