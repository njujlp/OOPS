#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Replace fv3 and FV3 in templace ModelX

Example:
     $  template2model.py mom5cice5 MOM5CICE55'
     will replace fv3 with mom5cice5 and FV3 with MOM5CICE5 

Todo:
     *
"""

import glob
import sys

if __name__ == '__main__':

    print sys.argv[0],': Replaces fv3 and FV3 with user defined strings in *.*'
    try:
        namespace=sys.argv[1]      # Replacement for fv3
        NAMESPACE=sys.argv[2]      # Replacement for FV3
        #wildc=sys.argv[3]          # Wild card (*.cc, *.f90, ...) 
    except:
        print 'Usage example: template2model.py fv3 FV3'
        sys.exit("Error: Wrong arguments")

    flist=glob.glob('*.*')
    for fname in flist:
        print fname
        if (fname!=sys.argv[0]):
            with open(fname, 'r') as file :
                filedata = file.read()
                filedata = filedata.replace('xxxx', namespace)
                filedata = filedata.replace('XXXX', NAMESPACE)
            with open(fname, 'w') as file:
                file.write(filedata)
