#!/bin/sh

if make -C .. ; then
../randomsound -v -v -v -v -T data-`date +'%F-%H-%M'`.bin
fi
