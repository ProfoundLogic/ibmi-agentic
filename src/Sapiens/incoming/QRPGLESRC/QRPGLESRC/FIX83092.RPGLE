    Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE FIX83092                                                                       *
    H**************************************************************************************
    H*  Description:  Soft Delete FAU2P records based on FAU1P                            *
    H*                                                                                    *
    H*  Created: 08/26/13   Programmer C:079   Support: MC133 83092                       *
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FFAU1P     IF   E             DISK
    F*
    F* Upload Detail File
    F*
     FFAU2L1    UF   E           K DISK
    F*
    D*
    C*
    C* Define Fields
    C*
     C     *like         define    u2trid        kktrid
    C*
    C* Key List
    C*
     C     u2ky          klist
     C                   kfld                    kktrid
    C*
    C*----------------------------------------------------------------------
    C*  Read through FAU1P file
    C*----------------------------------------------------------------------
    C*
    C* Read through FAU1P and select soft deleted records
    C*
     C                   read      pu101                                  99
    C*
b02  C                   Dow       Not *in99
    C*
b03  C                   If        u1prfl = 'D' and u1dldt <> 0
    C*
    C* Read through fau2p and soft delete
    C*
     C                   eval      kktrid = u1trid
    C*
     C     u2ky          chain     pu201                              99
    C*
b02  C                   Dow       Not *in99
    C*
     C                   eval      u2prfl = 'D'
     C                   update    pu201
    C*
     C     u2ky          reade     pu201                                  99
    C*
e03  C                   Enddo
    C*
e03  C                   Endif
    C*
     C                   read      pu101                                  99
    C*
e03  C                   Enddo
    C*
    C* End the program and close the files
    C*
e03  C                   eval      *inlr = *on
