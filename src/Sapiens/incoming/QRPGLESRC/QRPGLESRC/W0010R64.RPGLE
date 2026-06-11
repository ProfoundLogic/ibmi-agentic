     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title W0010R64 - Maintenance program for Flexible Inforamtion
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM: W0010R                                           *
      * CALLED TO:                                                    *
      *                                                               *
      * created 07/28/10  C:255  Project: 132WOJ                      *
      *****************************************************************
      * Revision:                                                     *
      * 02/21/11 Project 132WOJ Priss#37(Construction Rvw.)
      *                          1) PGMSTR : Added condition          *
      *                                                               *
      * 12/30/10 Supoort 75013
      *                          1) Added code to update maintenance  *
      *                             fields.                           *
      *                          2) Write the records if not found in *
      *                             WDFHP file.                       *
      *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0010f64  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
      * Generic Table
     FWTGTL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
    F*  Physical file definition (for maintaining record)
     Fwdfhp     uf a e             disk
     F                                     rename(pfh01:dbrcdf)
     Fwdfhl     if   e           k disk    infds(dbfds2)
     Fwmfnp     if   e             disk
     D*
     Ddbfds2           ds
     Ddbrrn2                 397    400b 0
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Flexible Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D***  Beg   Add  ***  12/30/10  **************************************
     D XXDT            s              7  0
     D***  End   Add  ***  12/30/10  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    ##rrn             9 0
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID            4
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY            10
     C*
     C     FHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    GTTBID        XXTBID
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*  VALIDE THE FLEXIBLE FIELDS
     C*
     C  N90              IF        (SCEHD1 = ' '  AND SCTBD1<> ' ')
     C                             OR (SCERQ1 <> ' 'AND SCEHD1=' ')
     C                             OR (SCERW1 <> ' 'AND SCEHD1=' ')
     C                             OR (SCFDF1 <> ' 'AND SCEHD1=' ')
     C                   SETON                                        43  90
     C                   ENDIF                                                  SCEHD1 = ' '
     C*
     C  N90              IF        SCERQ1 <> ' ' AND SCERQ1<> 'Y'
     C                   SETON                                        49  90
     C                   ENDIF                                                  SCERQ1 <>  ' '
     C*
     C  N90              IF        SCERW1 <> ' ' AND SCERW1<> 'Y'
     C                   SETON                                        50  90
     C                   ENDIF                                                  SCERW1 <>  ' '
     C*
     C  N90              IF        SCERQ1 <> ' ' AND SCERW1<> SCERQ1
     C                   SETON                                        51  90
     C                   ENDIF                                                  SCERQ1 <>  ' '
     C*
     C  N90              IF        SCEHD1 <> ' ' AND SCTBD1<> ' '
     C*
     C                   MOVEL     SCTBD1        KKTBID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        53  90
     C                   ENDIF
     C                   ENDIF                                                  SCEHD1 <> BLANKS
     C*
     C  N90              IF        SCFDF1 <> ' ' AND SCTBD1<> ' '
     C                   MOVEL     SCTBD1        XXTBID
     C                   MOVEL     SCFDF1        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        56  90
     C                   ENDIF
     C                   ENDIF                                                  SCFDF1 <> BLANKS
     C*Field#2 Validation
     C  N90              IF        (SCEHD2 = ' '  AND SCTBD2<> ' ')
     C                             OR (SCERQ2 <> ' 'AND SCEHD2=' ')
     C                             OR (SCERW2 <> ' 'AND SCEHD2=' ')
     C                             OR (SCFDF2 <> ' 'AND SCEHD2=' ')
     C                   SETON                                        44  90
     C                   ENDIF                                                  SCEHD2 = ' '
     C*
     C  N90              IF        SCERQ2 <> ' ' AND SCERQ2<> 'Y'
     C                   SETON                                        57  90
     C                   ENDIF                                                  SCERQ2 <>  ' '
     C*
     C  N90              IF        SCERW2 <> ' ' AND SCERW2<> 'Y'
     C                   SETON                                        58  90
     C                   ENDIF                                                  SCERW2 <>  ' '
     C*
     C  N90              IF        SCERQ2 <> ' ' AND SCERW2<> SCERQ2
     C                   SETON                                        59  90
     C                   ENDIF                                                  SCERQ2 <>  ' '
     C*
     C  N90              IF        SCEHD2 <> ' ' AND SCTBD2<> ' '
     C*
     C                   MOVEL     SCTBD2        KKTBID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        65  90
     C                   ENDIF
     C                   ENDIF                                                  SCEHD1 <> BLANKS
     C*
     C  N90              IF        SCFDF2 <> ' ' AND SCTBD2<> ' '
     C                   MOVEL     SCTBD2        XXTBID
     C                   MOVEL     SCFDF2        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        68  90
     C                   ENDIF
     C                   ENDIF                                                  SCFDF2 <> BLANKS
     C*Field#3 Validation
     C  N90              IF        (SCEHD3 = ' '  AND SCTBD3<> ' ')
     C                             OR (SCERQ3 <> ' 'AND SCEHD3=' ')
     C                             OR (SCERW3 <> ' 'AND SCEHD3=' ')
     C                             OR (SCFDF3 <> ' 'AND SCEHD3=' ')
     C                   SETON                                        45  90
     C                   ENDIF                                                  SCEHD3 = ' '
     C*
     C  N90              IF        SCERQ3 <> ' ' AND SCERQ3<> 'Y'
     C                   SETON                                        69  90
     C                   ENDIF                                                  SCERQ3 <>  ' '
     C*
     C  N90              IF        SCERW3 <> ' ' AND SCERW3<> 'Y'
     C                   SETON                                        70  90
     C                   ENDIF                                                  SCERW3 <>  ' '
     C*
     C  N90              IF        SCERQ3 <> ' ' AND SCERW3<> SCERQ3
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCERQ3 <>  ' '
     C*
     C  N90              IF        SCEHD3 <> ' ' AND SCTBD3<> ' '
     C*
     C                   MOVEL     SCTBD3        KKTBID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF                                                  SCEHD1 <> BLANKS
     C*
     C  N90              IF        SCFDF3 <> ' ' AND SCTBD3<> ' '
     C                   MOVEL     SCTBD3        XXTBID
     C                   MOVEL     SCFDF3        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF                                                  SCFDF3 <> BLANKS
     C*Field#4 Validation
     C  N90              IF           (SCERQ4 <> ' 'AND SCEHD4=' ')
     C                             OR (SCERW4 <> ' 'AND SCEHD4=' ')
     C
     C
     C                   SETON                                        46  90
     C                   ENDIF                                                  SCEHD4 = ' '
     C*
     C  N90              IF        SCERQ4 <> ' ' AND SCERQ4<> 'Y'
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCERQ4 <>  ' '
     C*
     C  N90              IF        SCERW4 <> ' ' AND SCERW4<> 'Y'
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCERW4 <>  ' '
     C*
     C  N90              IF        SCERQ4 <> ' ' AND SCERW4<> SCERQ4
     C                   SETON                                        79  90
     C                   ENDIF                                                  SCERQ4 <>  ' '
     C*Field#5 Validation
     C  N90              IF           (SCERQ5 <> ' 'AND SCEHD5= ' ')
     C                             OR (SCERW5 <> ' 'AND SCEHD5= ' ')
     C
     C
     C                   SETON                                        47  90
     C                   ENDIF                                                  SCEHD5 = ' '
     C*
     C  N90              IF        SCERQ5 <> ' ' AND SCERQ5<> 'Y'
     C                   SETON                                        85  90
     C                   ENDIF                                                  SCERQ5 <>  ' '
     C*
     C  N90              IF        SCERW5 <> ' ' AND SCERW5<> 'Y'
     C                   SETON                                        86  90
     C                   ENDIF                                                  SCERW5 <>  ' '
     C*
     C  N90              IF        SCERQ5 <> ' ' AND SCERW5<> SCERQ5
     C                   SETON                                        87  90
     C                   ENDIF                                                  SCERQ5 <>  ' '
     C*Field#6 Validation
     C  N90              IF        (SCEHD6 = ' '  AND SCTBD6<> ' ')
     C                             OR (SCERQ6 <> ' 'AND SCEHD6=' ')
     C                             OR (SCERW6 <> ' 'AND SCEHD6=' ')
     C                             OR (SCFDF6 <> ' 'AND SCEHD6=' ')
     C                   SETON                                        48  90
     C                   ENDIF                                                  SCEHD6 = ' '
     C*
     C  N90              IF        SCERQ6 <> ' ' AND SCERQ6<> 'Y'
     C                   SETON                                        94  90
     C                   ENDIF                                                  SCERQ6 <>  ' '
     C*
     C  N90              IF        SCERW6 <> ' ' AND SCERW6<> 'Y'
     C                   SETON                                        95  90
     C                   ENDIF                                                  SCERW6 <>  ' '
     C*
     C  N90              IF        SCERQ6 <> ' ' AND SCERW6<> SCERQ6
     C                   SETON                                        96  90
     C                   ENDIF                                                  SCERQ6 <>  ' '
     C*
     C  N90              IF        SCEHD6 <> ' ' AND SCTBD6<> ' '
     C*
     C                   MOVEL     SCTBD6        KKTBID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        98  90
     C                   ENDIF
     C                   ENDIF                                                  SCEHD5 <> BLANKS
     C*
     C  N90              IF        SCFDF6 <> ' ' AND SCTBD6<> ' '
     C                   MOVEL     SCTBD6        XXTBID
     C                   MOVEL     SCFDF6        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C                   IF        *IN99 = *ON
     C                   SETON                                        23  90
     C                   ENDIF
     C                   ENDIF                                                  SCFDF6 <> BLANKS
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   IF        HFLD= 'SCEHD1'
     C                   SETON                                        2526
     C                   SETON                                        626364
     C                   ENDIF
     C*
     C                   IF        HFLD= 'SCERQ1'
     C                   SETON                                        2526
     C                   SETOFF                                       626364
     C                   ENDIF
     C*
     C                   IF        HFLD= 'SCERW1'
     C                   SETON                                        26
     C                   SETON                                        626364
     C                   SETOFF                                       25
     C                   ENDIF
     C*
     C                   IF        HFLD= 'SCTBD1'
     C                   CALL      'W2491R'
     C                   PARM      SCPRDS        ##PRDS           40
     C                   PARM      ' '           ##CALL            1
     C                   PARM      ' '           ##ID              4
     C                   PARM      ' '           ##RTCD            1
     C*
     C                   IF        ##RTCD ='A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD ='B'
     C                   MOVE      *ON           *INKL
     C                   MOVE      *ON           *IN52
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   ENDIF
     C*
     C                   IF        ##ID=*BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN52
     C                   ENDIF
     C*
     C                   IF        ##ID <> *BLANKS
     C                   MOVEL     ##ID          SCTBD1
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        HFLD='SCFDF1'
     C                   IF        SCTBD1 = ' '
     C**** NOTIFY TO ENTER IT FIRST
     C                   MOVE      *ON           *IN54
     C                   ELSE
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCTBD1        ##ID
     C                   PARM      ' '           ##KEY            10
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD = 'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD= 'B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C                   IF        ##KEY = *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C                   IF        ##KEY   <>   *BLANKS
     C                   MOVEL     ##KEY         SCFDF1
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN32
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCEHD2'
     C                   SETON                                        25
     C                   SETON                                        626364
     C                   SETOFF                                       26
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCERQ2'
     C                   SETON                                          2526
     C                   SETON                                          6364
     C                   SETOFF                                         62
     C                   ENDIF
     C*
     C                   IF        HFLD  = 'SCERW2'
     C                   SETON                                          2526
     C                   SETON                                          6264
     C                   SETOFF                                         63
     C                   ENDIF
     C*
     C                   IF        HFLD =  'SCTBD2'
     C*
     C                   CALL      'W2491R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##ID
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD ='A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD ='B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C*
     C                   IF        ##ID= *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C*
     C                   IF        ##ID <> *BLANKS
     C                   MOVEL     ##ID          SCTBD2
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        HFLD=  'SCFDF2'
     C*
     C                   IF        SCTBD2  =   ' '
     C**** NOTIFY TO ENTER IT FIRST
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCTBD2        ##ID
     C                   PARM      ' '           ##KEY
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD = 'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD=   'B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C                   IF        ##KEY =  *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C                   IF        ##KEY <>   *BLANKS
     C                   MOVEL     ##KEY         SCFDF2
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN33
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*FIELD#3
     C                   IF        HFLD = 'SCEHD3'
     C                   SETON                                        2526
     C                   SETON                                        6263
     C                   SETOFF                                       64
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCERQ3'
     C                   SETON                                        626364
     C                   SETOFF                                         2526
     C                   ENDIF
     C*
     C                   IF        HFLD  = 'SCERW3'
     C                   SETON                                        266364
     C                   SETOFF                                         2562
     C                   ENDIF
     C*
     C                   IF        HFLD =  'SCTBD3'
     C*
     C                   CALL      'W2491R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##ID
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD ='A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD ='B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN72
     C                   ENDIF
     C*
     C                   IF        ##ID= *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN72
     C                   ENDIF
     C*
     C                   IF        ##ID <> *BLANKS
     C                   MOVEL     ##ID          SCTBD3
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        HFLD=  'SCFDF3'
     C*
     C                   IF        SCTBD3  =   ' '
     C**** NOTIFY TO ENTER IT FIRST
     C                   MOVE      *ON           *IN74
     C                   ELSE
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCTBD3        ##ID
     C                   PARM      ' '           ##KEY
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD = 'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD=   'B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C*
     C                   IF        ##KEY =  *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C*
     C                   IF        ##KEY <>   *BLANKS
     C                   MOVEL     ##KEY         SCFDF3
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*FIELD#4
     C                   IF        HFLD = 'SCEHD4'
     C                   SETON                                        266264
     C                   SETOFF                                       2563
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCERQ4'
     C                   SETON                                        266263
     C                   SETOFF                                       2564
     C                   ENDIF
     C*
     C                   IF        HFLD  = 'SCERW4'
     C                   SETON                                        256364
     C                   SETOFF                                       2662
     C                   ENDIF
     C*
     C*FIELD#5
     C                   IF        HFLD = 'SCEHD5'
     C                   SETON                                        256264
     C                   SETOFF                                       2663
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCERQ5'
     C                   SETON                                        256263
     C                   SETOFF                                       2664
     C                   ENDIF
     C*
     C                   IF        HFLD  = 'SCERW5'
     C                   SETON                                        252664
     C                   SETOFF                                       6263
     C                   ENDIF
     C*
     C*FIELD#6
     C                   IF        HFLD = 'SCEHD6'
     C                   SETON                                        252663
     C                   SETOFF                                       6264
     C                   ENDIF
     C*
     C                   IF        HFLD = 'SCERQ6'
     C                   SETON                                        252662
     C                   SETOFF                                       6364
     C                   ENDIF
     C*
     C                   IF        HFLD  = 'SCERW6'
     C                   SETON                                        6364
     C                   SETOFF                                       252662
     C                   ENDIF
     C*
     C                   IF        HFLD =  'SCTBD6'
     C*
     C                   CALL      'W2491R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##ID
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD ='A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD ='B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   IF        ##ID= *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   IF        ##ID <> *BLANKS
     C                   MOVEL     ##ID          SCTBD6
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   MOVE      *ON           *IN22
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        HFLD=  'SCFDF6'
     C*
     C                   IF        SCTBD6  =   ' '
     C**** NOTIFY TO ENTER IT FIRST
     C                   MOVE      *ON           *IN21
     C                   ELSE
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCTBD6        ##ID
     C                   PARM      ' '           ##KEY
     C                   PARM      ' '           ##RTCD
     C*
     C                   IF        ##RTCD = 'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   IF        ##RTCD=   'B'
     C                   MOVE      *ON           *INKL
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN22
     C                   ENDIF
     C*
     C                   IF        ##KEY =  *BLANKS
     C*** POSITION CURSOR ON FIELD SEARCHED
     C                   MOVE      *ON           *IN22
     C                   ENDIF
     C*
     C                   IF        ##KEY <>   *BLANKS
     C                   MOVEL     ##KEY         SCFDF6
     C*** POSITION CURSOR TO NEXT FIELD SEARCHED
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  KJDO
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   MOVEL     FHFH#1        SCEHD1
     C                   MOVEL     FHFH1Q        SCERQ1
     C                   MOVEL     FHFH1W        SCERW1
     C                   MOVEL     FHF1TB        SCTBD1
     C                   MOVEL     FHF1DF        SCFDF1
     C                   MOVEL     FHFH#2        SCEHD2
     C                   MOVEL     FHFH2Q        SCERQ2
     C                   MOVEL     FHFH2W        SCERW2
     C                   MOVEL     FHF2TB        SCTBD2
     C                   MOVEL     FHF2DF        SCFDF2
     C                   MOVEL     FHFH#3        SCEHD3
     C                   MOVEL     FHFH3Q        SCERQ3
     C                   MOVEL     FHFH3W        SCERW3
     C                   MOVEL     FHF3TB        SCTBD3
     C                   MOVEL     FHF3DF        SCFDF3
     C                   MOVEL     FHFH#4        SCEHD4
     C                   MOVEL     FHFH4Q        SCERQ4
     C                   MOVEL     FHFH4W        SCERW4
     C                   MOVEL     FHFH#5        SCEHD5
     C                   MOVEL     FHFH5Q        SCERQ5
     C                   MOVEL     FHFH5W        SCERW5
     C                   MOVEL     FHFH#6        SCEHD6
     C                   MOVEL     FHFH6Q        SCERQ6
     C                   MOVEL     FHFH6W        SCERW6
     C                   MOVEL     FHF6TB        SCTBD6
     C                   MOVEL     FHF6DF        SCFDF6
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   MOVEL     SCEHD1        FHFH#1
     C                   MOVEL     SCERQ1        FHFH1Q
     C                   MOVEL     SCERW1        FHFH1W
     C                   MOVEL     SCTBD1        FHF1TB
     C                   MOVEL     SCFDF1        FHF1DF
     C                   MOVEL     SCEHD2        FHFH#2
     C                   MOVEL     SCERQ2        FHFH2Q
     C                   MOVEL     SCERW2        FHFH2W
     C                   MOVEL     SCTBD2        FHF2TB
     C                   MOVEL     SCFDF2        FHF2DF
     C                   MOVEL     SCEHD3        FHFH#3
     C                   MOVEL     SCERQ3        FHFH3Q
     C                   MOVEL     SCERW3        FHFH3W
     C                   MOVEL     SCTBD3        FHF3TB
     C                   MOVEL     SCFDF3        FHF3DF
     C                   MOVEL     SCEHD4        FHFH#4
     C                   MOVEL     SCERQ4        FHFH4Q
     C                   MOVEL     SCERW4        FHFH4W
     C                   MOVEL     SCEHD5        FHFH#5
     C                   MOVEL     SCERQ5        FHFH5Q
     C                   MOVEL     SCERW5        FHFH5W
     C                   MOVEL     SCEHD6        FHFH#6
     C                   MOVEL     SCERQ6        FHFH6Q
     C                   MOVEL     SCERW6        FHFH6W
     C                   MOVEL     SCTBD6        FHF6TB
     C                   MOVEL     SCFDF6        FHF6DF
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C***  Beg   Del  ***  12/30/10  **************************************
     C*************      if        @@mode = 'A'
     C*************      seton                                        40
     C*************      seton                                        42
     C*************      else
     C*************      setoff                                       40
     C*************      setoff                                       42
     C*************      endif
     C***  End   Del  ***  12/30/10  **************************************
     c     ##rrn         chain(n)  pfn01                              60
     c                   eval      scfnd  = fnfnd
     c                   eval      scname = fnname
     c                   move      fnco#         ##co#
     c                   move      fnfnd         xxfnd
     c*
     c     fhky          chain     pfh01                              60
     c  N60              Z-ADD     DBRRN2        @@rrn             9 0
     C***  Beg   Add  ***  12/30/10  **************************************
     C***  Beg   Chg  ***  02/21/11  **************************************
     C*****              if        *in60 = *on
     C                   if        *in60 = *on and @@mode = 'C'
     C***  End   Chg  ***  02/21/11  **************************************
     C                   seton                                        40
     C                   seton                                        42
     C                   eval      @@mode = 'A'
     C                   endif
     C***  End   Add  ***  12/30/10  **************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C***  Beg   Add  ***  12/30/10  **************************************
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         XXDT
     C*
     C                   EVAL      FHUSER =Q1USER
     C                   EVAL      FHPGM  =Q1PGM
     C                   MOVE      dstime        FHTIME
     C                   MOVE      xxdt          FHDT
     C***  End   Add  ***  12/30/10  **************************************
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C***  Beg   Add  ***  12/30/10  **************************************
     C                   move      fnco#         fhco#
     C                   move      fnfnd         fhfnd
     C***  End   Add  ***  12/30/10  **************************************
     C                   exsr      mvscdb
     C***  Beg   Add  ***  12/30/10  **************************************
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt
     C*
     C                   EVAL      FHUSER =Q1USER
     C                   EVAL      FHIUSR =Q1USER
     C                   EVAL      FHPGM  =Q1PGM
     C                   EVAL      FHIPGM  =Q1PGM
     C                   MOVE      dstime        FHTIME
     C                   MOVE      dstime        FHITIM
     C                   MOVE      xxdt          FHDT
     C                   MOVE      xxdt          FHIDT
     C***  End   Add  ***  12/30/10  **************************************
     C                   write     dbrcdf
     C                   feod      wdfhp
     C                   endsr
