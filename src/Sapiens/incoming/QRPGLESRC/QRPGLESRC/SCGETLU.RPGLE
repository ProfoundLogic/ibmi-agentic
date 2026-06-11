     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F**** GET THE "LU NAME" FROM THE IMWSP FILE (OTHER VALUES AS WELL)
     F**
     F**
     F**  #02   DR214 - FOR IDARTS COMMUNICATIONS
     F**
     F**
     FIMWSL2    IF   E           K DISK

     D**
     D**
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C***
     C     *ENTRY        PLIST
     C                   PARM                    INJBNM           10                1
     C                   PARM                    @@LUNM           10                2
     C                   PARM                    @@RTN             1                3
     C                   PARM                    @@WSID            2                4
     C                   PARM                    @@IDRV            1                5
     C                   PARM                    @@STAT            1                6
     C                   PARM                    @@VQUE           10                7
     C                   PARM                    @@VQLB           10                8
     C                   PARM                    @@VRQ            10                9
     C                   PARM                    @@VRQL           10               10
     C                   PARM                    @@SRCV           10               11
     C                   PARM                    @@SRLB           10               12
     C                   PARM                    @@SSND           10               13
     C                   PARM                    @@SSLB           10               14
     C*
     C                   MOVE      *BLANKS       @@LUNM
     C                   MOVE      *BLANKS       @@RTN
     **
     C     Q1PRMS        IFGE      4
     C                   MOVEL     *BLANK        @@WSID            2
     C                   MOVEL     *BLANK        @@IDRV            1
     C                   MOVEL     *BLANK        @@STAT            1
     C                   MOVEL     *BLANK        @@VQUE           10
     C                   MOVEL     *BLANK        @@VQLB           10
     C                   MOVEL     *BLANK        @@VRQ            10
     C                   MOVEL     *BLANK        @@VRQL           10
     C                   MOVEL     *BLANK        @@SRCV           10
     C                   MOVEL     *BLANK        @@SRLB           10
     C                   MOVEL     *BLANK        @@SSND           10
     C                   MOVEL     *BLANK        @@SSLB           10
     C                   ENDIF
     ***
     C     INJBNM        CHAIN     PWS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     WSLUNM        @@LUNM           10
     C     Q1PRMS        IFGE      4
     C                   MOVEL     WSWSID        @@WSID            2
     C                   MOVEL     WSIDRV        @@IDRV            1
     C                   MOVEL     WSSTAT        @@STAT            1
     C                   MOVEL     WSVQUE        @@VQUE           10
     C                   MOVEL     WSVQLB        @@VQLB           10
     C                   MOVEL     WSVRQ         @@VRQ            10
     C                   MOVEL     WSVRQL        @@VRQL           10
     C                   MOVEL     WSSRCV        @@SRCV           10
     C                   MOVEL     WSSRLB        @@SRLB           10
     C                   MOVEL     WSSSND        @@SSND           10
     C                   MOVEL     WSSSLB        @@SSLB           10
     C                   ENDIF
     C                   MOVEL     *BLANKS       @@RTN             1
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       @@LUNM
     C                   MOVE      '1'           @@RTN             1
     C                   ENDIF
     C*
     C                   RETURN
     C                   MOVE      '1'           *INLR
     C*
