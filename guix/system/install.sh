#!bash

if [test $1="casper"]; then
     mkfs.fat -F32 -n boot $2
     mkfs.btrfs -L system $3
else
    echo "unrecognized option, please modify this script"
    exit
fi

mount LABEL=system /mnt
mkdir -p /mnt/boot/efi
mount LABEL=boot /mnt/boot/efi

herd start cow-store /mnt

mkdir -p ~/.config/guix
cp ./magi/guix/channels.scm ~/.config/guix
guix pull
hash guix

guix system -L ~/magi/guix/system init ~/magi/guix/system/"$1".scm /mnt
