#!/bin/sh

if [ $1="casper" ]; then
    echo "Creating the filesystem"
     #mkfs.fat -F32 -n boot $2
     mkfs.btrfs -L system $3
else
    echo "Unrecognized option, please modify this script"
    exit
fi

echo "Mounting the filesystem to /mnt"

mount LABEL=system /mnt
mkdir -p /mnt/boot/efi
mount $2 /mnt/boot/efi

echo "Starting herd cow-store"

herd start cow-store /mnt

echo "Pulling channels"

mkdir -p ~/.config/guix
cp ./magi/guix/channels.scm ~/.config/guix
guix pull
hash guix

echo "Installing system"

guix system -L ~/magi/guix/system init ~/magi/guix/system/"$1".scm /mnt
