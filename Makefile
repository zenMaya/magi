CONFIG_FILE = ./config.scm
DEST_MOUNT = /mnt
HOSTS = casper balthasar melchior
HOST = $(shell hostname)
USERS = maya
GLP = ./:$(GUILE_LOAD_PATH)
GUIX_CONFIG_DIR = $(XDG_CONFIG_HOME)/guix
GUIX_CHANNELS_FILE= $(GUIX_CONFIG_DIR)/channels.scm

targets-home = $(foreach host,$(HOSTS),$(foreach user, $(USERS),$(addsuffix -home,$(user)-$(host))))
targets-system = $(addsuffix -system, $(HOSTS))

default:
ifeq ($(shell id -u),0)
	make $(HOST)-system-reconfigure
else
	make $(USER)-$(HOST)-home-reconfigure
endif

%-home-build: $(GUIX_CHANNELS_FILE)
	GUILE_LOAD_PATH=$(GLP) MAGI_TARGET=home-$* \
	guix home build $(CONFIG_FILE)

%-home-reconfigure: $(GUIX_CHANNELS_FILE)
	GUILE_LOAD_PATH=$(GLP) MAGI_TARGET=home-$* \
	guix home reconfigure $(CONFIG_FILE)

%-system-init: $(GUIX_CHANNELS_FILE) cow-store
	GUILE_LOAD_PATH=$(GLP) MAGI_TARGET=system-$* \
	guix system init $(CONFIG_FILE) $(DEST_MOUNT)

%-system-build: $(GUIX_CHANNELS_FILE)
	GUILE_LOAD_PATH=$(GLP) MAGI_TARGET=system-$* \
	guix system build $(CONFIG_FILE)

%-system-reconfigure: $(GUIX_CHANNELS_FILE)
	GUILE_LOAD_PATH=$(GLP) MAGI_TARGET=system-$* \
	guix system reconfigure $(CONFIG_FILE)

$(targets-home): %-home-reconfigure
$(targets-system): %-system-reconfigure

.PHONY: help

help:
	$(info The targets are '$(targets-home)' and '$(targets-system)')

$(GUIX_CHANNELS_FILE):
	mkdir -p $(GUIX_CONFIG_DIR) \
	ln channels.scm $(GUIX_CHANNELS_FILE) \
	guix pull \
	hash guix

.PHONY: cow-store
cow-store:
	herd start cow-store $(DEST_MOUNT)
