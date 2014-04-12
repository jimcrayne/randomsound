#!/bin/sh

if make -C .. ; then
DATFILE=data-`date +'%F-%H-%M'`.bin
../randomsound -v -v -v -v -T $DATFILE
shannon $DATFILE
fi
