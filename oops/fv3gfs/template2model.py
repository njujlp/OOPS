#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Replace fv3gfs and FV3GFS in templace ModelX

Example:
     $  template2model.py mom5cice5 MOM5CICE55'
     will replace fv3gfs with mom5cice5 and FV3GFS with MOM5CICE5 

Todo:
     *
"""

import glob
import sys

if __name__ == '__main__':

    print sys.argv[0],': Replaces fv3gfs and FV3GFS with user defined strings in *.*'
    try:
        namespace=sys.argv[1]      # Replacement for fv3gfs
        NAMESPACE=sys.argv[2]      # Replacement for FV3GFS
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
                filedata = filedata.replace('fv3gfs', namespace)
                filedata = filedata.replace('FV3GFS', NAMESPACE)
            with open(fname, 'w') as file:
                file.write(filedata)
