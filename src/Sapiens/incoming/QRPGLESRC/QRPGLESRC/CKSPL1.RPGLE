     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO COPY CHECK REQUEST FOR A SPECIFIC DATE
     H*****************************************************************
     H* Description: READS THE CHECK SCHEDULE FILE FOR A SPECIFIC     *
     H*              DATE AND CALLS CKSPL2 TO DUPLICATE THE CK SCHD   *
     H*              & CHK DATA                                       *
     H*                                                               *
     H*****************************************************************
     FWACPL     IF   E           K DISK
     FWSCKL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPRDT            7            CYYMMDD
     C*
     C*  Key list
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXPRDT            7 0
     C*
     C     CKKY          KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    CPACCT
     C*
     C*----------------------------------------------------------------
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQPRDT        XXPRDT            7 0
     C*
     C*  Start of main loop
     C*
     C     CPKY          CHAIN     PCP01                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     CKKY          CHAIN     PCK01                              98
     C  N98              MOVE      CKLCHK        XXCHK2            7
     C   98              MOVE      *ALL'9'       XXCHK2
     C*
     C                   MOVE      CPFND         XXFND             3
     C                   MOVE      CPCKTP        XXCKTP           10
     C                   MOVE      CPACCT        XXACCT           20
     C                   MOVE      CPCHK#        XXCHK#            7
     C                   MOVE      CPPRDT        X2PRDT            7
     C*
     C                   CALL      'CKSPL2'
     C                   PARM                    XXFND
     C                   PARM                    XXCKTP
     C                   PARM                    XXACCT
     C                   PARM                    XXCHK#
     C                   PARM                    XXCHK2
     C                   PARM                    X2PRDT
     C*
     C     CPKY          READE     PCP01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
