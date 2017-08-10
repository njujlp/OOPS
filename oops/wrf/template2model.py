#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Replace xxxx and XXXX in templace ModelX

Example:
     $  template2model.py mom5cice5 MOM5CICE55'
     will replace xxxx with mom5cice5 and XXXX with MOM5CICE5 

Todo:
     *
"""

import glob
import sys

if __name__ == '__main__':

    print sys.argv[0],': Replaces xxxx and XXXX with user defined strings in *.*'
    try:
        namespace=sys.argv[1]      # Replacement for xxxx
        NAMESPACE=sys.argv[2]      # Replacement for XXXX
#       wildc=sys.argv[3]          # Wild card (*.cc, *.f90, ...) 
    except:
        print 'Usage example: template2model.py mom5cice5 MOM5CICE5 *.c'
        sys.exit("Error: Wrong arguments")

    flist=glob.glob("*.*")
    for fname in flist:
        print fname
        if (fname!=sys.argv[0]):
            with open(fname, 'r') as file :
                filedata = file.read()
                filedata = filedata.replace('xxxx', namespace)
                filedata = filedata.replace('XXXX', NAMESPACE)
            with open(fname, 'w') as file:
                file.write(filedata)
