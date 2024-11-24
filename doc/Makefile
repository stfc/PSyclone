
all: developer_guide reference_guide user_guide
.PHONY: developer_guide reference_guide user_guide

developer_guide:
	make -C developer_guide html SPHINXOPTS="-W --keep-going"

reference_guide:
	make -C reference_guide html SPHINXOPTS="-W --keep-going"

user_guide:
	make -C user_guide html SPHINXOPTS="-W --keep-going"

clean:
	make -C developer_guide clean
	make -C reference_guide allclean
	make -C user_guide clean

