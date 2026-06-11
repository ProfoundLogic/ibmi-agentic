     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*====================================================================
     H/title Collateral Dissolve Report
     H*====================================================================
     H*  Created: 01/25/10   C:232  Project: WC1851
     H*====================================================================
    H*  Revisions:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H*====================================================================

    F*  Physical file definition (for maintaining record)
     FWXGSL     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMCML     IF   E           k disk
     FWMCHL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTC3L     IF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWMEAP     O    E             DISK    RENAME(PEA01 : MEA0L6) prefix(m6)
      *
     FWXRNL     IF   E           K DISK
     FWXRNP     UF A E             DISK    RENAME(PRN01:PRN02)
      *
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*
      *
     FQsysprt   o    f  132        printer oflind(*inof)

    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------

     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     d                 ds
     d  grp#                          3    inz
     d  year#                         3    inz
     d  case#                         7    inz

     D CO#             S              3  0
     D QUE             S             10
     D rcd_found       S               n   inz
     D ##CO#           S              3  0 inz
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     D Count_01        S              9  0 inz
     D Count_02        S              9  0 inz
     D Total_01        S             15  2 inz
     D Total_02        S             15  2 inz
     d* Route Information
     d routeCo#        s              3
     d routeDate       s               d   datfmt(*usa)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d companynm       s             60
     D comapi          c                   const('WA001R')
     D/copy wprar
     C*
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@OCDT            8
     C                   PARM                    @@NADT            8
     C                   PARM                    @@ROFL            1
     C                   PARM                    QUE

     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM

     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C

     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

    C*================================================================
    C*  KLIST definitions
    C*================================================================

     C     CMKY          KLIST
     C                   KFLD                    CO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     CHKY          KLIST
     C                   KFLD                    CO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    CMCASE
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    CO#
     C*
     C     ELKY          KLIST
     C                   KFLD                    CO#
     C                   KFLD                    GSFND
     C*
     C     F1KY          KLIST
     C                   KFLD                    CO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C*
     C     RNKY          KLIST
     C                   KFLD                    CO#
     C*
     C     ELKY1         KLIST
     C                   KFLD                    CO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
    C*================================================================
    C*                  M A I N L I N E
    C*================================================================
      *
     C                   MOVEL     @@OCDT        XXOCDT1           8 0
     C                   MOVEL     @@NADT        XXNADT1           8 0
      *
     C                   Z-ADD     XXOCDT1       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOCDT            7 0
      *
     C                   Z-ADD     XXNADT1       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXNADT            7 0
      *
     C                   exsr      initialize
      *
      *****************************************************************
      *
     C*  Read through file to make sure at least one group is included
      *
     C                   MOVEL     *ZEROS        @@FND             5 0
      *
     C     FNKY          CHAIN     PGS01                              71
      *
     C                   DOW       *IN71=*OFF
      *
     C                   IF        GSIEFL =  'I'
      *
      * Read  WTC3L to retrive C3CPCD...
      *
     C                   MOVEL     *BLANKS       XXCPCD
      *
     C     ELKY          CHAIN     PC301                              80
     C                   IF        *IN80 = *OFF
     C                   MOVEL     C3CPCD        XXCPCD            2
     C                   ELSE
     C     FNKY          READE     PGS01                                  71
     C                   ITER
     C                   ENDIF
      *
     C                   IF        @@FND <> 0
      *
      * GROUP CHANGED
     C                   EVAL      COUNT_01 = 0
     C                   EVAL      Total_01 = 0
     C                   EXSR      SREMP
     C                   IF        COUNT_01 <> 0
     C                   EXCEPT    SPACE#
     C                   EXCEPT    DISP01
      *
      *  If swcpf/wcfl25 = 'Y'.. then Create return batch at FND break.
      *
     C                   IF        WCFL25 = 'Y' and @@ROFL = 'N'
     C                   EXSR      WRITERFB
     C                   ENDIF
      *
     C                   ENDIF
     C                   EVAL      @@FND = @@FND + 1
      *
     C                   ELSE
      *
     C                   EVAL      COUNT_01 = 0
     C                   EVAL      Total_01 = 0
     C                   EXSR      SREMP
     C                   IF        COUNT_01 <> 0
     C                   EXCEPT    SPACE#
     C                   EXCEPT    DISP01
      *
      *  If swcpf/wcfl25 = 'Y'.. then Create return batch at FND break.
      *
     C                   IF        WCFL25 = 'Y'
     C                   EXSR      WRITERFB
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   EVAL      @@FND = @@FND + 1
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     GSFND         XXFND             3 0
      *
     C     FNKY          READE     PGS01                                  71
     C                   ENDDO
      *
     C                   IF        COUNT_02 <> 0
     C                   EXCEPT    DISP51
     C                   EXCEPT    SPACE#
     C                   EXCEPT    FOT1
     C                   ENDIF
      *
     C                   If        COUNT_02 = 0
     C                   EXCEPT    SKIP#
     C                   EXCEPT    HEADER
     C                   EXCEPT    SPACE#
     C                   EXCEPT    FOT2
     C                   EXCEPT    FOT1
     C                   ENDIF
      ****************************************************************
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfile = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Eval      *inlr = *on
     C                   Return
      *
     *===============================================================*
      ****************************************************************
     *===============================================================*
     C     SREMP         BEGSR
      *
     C     ELKY          CHAIN     PEL01                              72
      *
     C                   DOW       *IN72=*OFF
      *
      *Initialize SVCLDT = 0, SVACDT = 0
      *
     C                   MOVEL     *ZEROS        SVCLDT            7 0
     C                   MOVEL     *ZEROS        SVCLDT1           8 0
     C                   MOVEL     *ZEROS        SVACDT            7 0
     C                   MOVEL     *ZEROS        SVACDT1           8 0
     C                   MOVEL     *ZEROS        XXCLDT1           8 0
     C                   MOVEL     *ZEROS        XXCLDT            7 0
     C                   MOVEL     *ZEROS        XXCMDT1           8 0
     C                   MOVEL     *ZEROS        XXCMDT            7 0
      *
     C                   MOVEL     ELDT          SVACDT            7 0
     C                   Z-ADD     ELDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVACDT1           8 0
      *
      * If Collateral transfer A/R code is not setup, then skip employers
      *
     C     F1KY          SETGT     PF101
     C     F1KY          READPE    PF101                                  77
     C                   IF        *IN77 = *OFF
     C                   IF        F1COTA = *BLANKS AND F1ACOT = *BLANKS
     C     ELKY          READE     PEL01                                  72
     C                   ITER
     C                   ENDIF
     C                   ENDIF
      *
      *If no activity since date entered, and If ELDT > date entered then skip employer
      *
     C                   IF        XXNADT1 <> *ZEROS
     C                   IF        SVACDT > XXNADT
     C     ELKY          READE     PEL01                                  72
     C                   ITER
     C                   ELSE
     C                   Z-ADD     ELDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVACDT1           8 0
     C                   ENDIF
     C                   ENDIF
      *
     C                   EXSR      SRCMP
      *
     C     ELKY          READE     PEL01                                  72
     C                   ENDDO
     C                   ENDSR                                                  SREMP
      ****************************************************************
     C     SRCMP         BEGSR
      *
     C     CMKY          CHAIN     PCM01                              73
      *
     C                   MOVEL     *ZEROS        XXCMDT1           8 0
     C                   MOVEL     *ZEROS        XXCMDT            7 0
     C                   MOVE      *ZEROS        XXTFAM
     C                   MOVE      *ZEROS        XXCLDT
     C                   MOVEL     'N'           PROCESS           1
      *
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCMDT1
     C                   MOVEL     CMDT          XXCMDT
      *
      * If no records found in WMCMP then process this employer
      *
     C                   IF        *IN73 = '1'
      *
     C***                IF        XXOCDT <> *ZEROS
     C***                EXSR      SREAP
     C                   MOVEL     'Y'           PROCESS
     C***                ENDIF
      *
     C                   ELSE
      *
     C                   DOW       *IN73 = *OFF
      *
     C                   MOVE      *ZEROS        XXTFAM
      *
      * If No Open Claims Date" is entered and the status of
      * of claim is open then skip employer
      *
     C                   IF        XXOCDT <> *ZEROS
     C                   IF        CMSTAT = 'O'
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      * If CMDT > no activity since entered date then skip employer
      *
     C                   IF        XXNADT <> *ZEROS
     C                   IF        XXCMDT > XXNADT
      *
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
      *
     C                   ELSE
      *
     C                   IF        XXCMDT > SVACDT
     C                   MOVEL     XXCMDT1       SVACDT1
     C                   MOVEL     XXCMDT        SVACDT
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      * If No Open Claims Date" is entered and ..
      * If record found in WMCMP with closed status(WMCMP/CMSTAT = C)..
      * then Retrieve closed date from WMCHL
      *
     C                   IF        XXOCDT <> *ZEROS
      *
     C                   IF        CMSTAT = 'C'
     C*
     C     CHKY          SETGT     PCH01
     C     CHKY          READPE    PCH01                                  60
     C*
     C                   IF        CHOCST = 'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCLDT1           8 0
     C                   MOVEL     CHEFDT        XXCLDT            7 0
     C                   EVAL      SVCLDT1 = XXCLDT1
     C                   EVAL      SVCLDT  = XXCLDT
      *
     C                   ELSE
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   IF        XXOCDT <> *ZEROS AND XXNADT = *ZEROS
     C                   IF        SVCLDT < XXOCDT
     C                   MOVEL     'Y'           PROCESS
     C                   ELSE
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   IF        XXOCDT = *ZEROS AND XXNADT <> *ZEROS
     C                   IF        SVACDT < XXNADT
     C                   MOVEL     'Y'           PROCESS
     C                   ELSE
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C                   IF        XXOCDT <> *ZEROS AND XXNADT <> *ZEROS
     C                   IF        (SVACDT < XXNADT) AND
     C                             ( SVCLDT < XXOCDT )
     C                   MOVEL     'Y'           PROCESS
     C                   ELSE
     C                   MOVEL     'N'           PROCESS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  IF XXOCDT <> *ZEROS
      *
     C     CMKY          READE     PCM01                                  73
     C                   ENDDO
     C                   ENDIF                                                  *IN73 = '1'
      *
      * If all the conditions pass, then process the policy for Collateral..
      *
     C                   IF        PROCESS = 'Y'
      *
     C                   EXSR      SREAP
     C                   MOVEL     'N'           PROCESS
      *
     C                   IF        XXTFAM > 0
      *
     C                   IF        XXFND <> GSFND
     C                   MOVEL     GSFND         XXFND
     C                   EXCEPT    SKIP#
     C                   EXCEPT    HEADER
     C                   EXCEPT    SPACE#
     C                   ENDIF
      *
     C                   EVAL      COUNT_01 = COUNT_01 + 1
     C                   EVAL      Total_01 = Total_01 + XXTFAM
     C                   EVAL      COUNT_02 = COUNT_02 + 1
     C                   EVAL      Total_02 = Total_02 + XXTFAM
     C                   EXSR      PrntData
     C                   EVAL      XXTFAM = 0
     C                   ENDIF                                                  IF XXTFAM > 0
      *
     C                   ENDIF                                                  IF PROCESS = 'Y'
      *
     C                   ENDSR                                                  SRCMP
      *****************************************************************
      *****************************************************************
     C     SREAP         BEGSR
     C*
     C                   MOVEL     *ZEROS        XXTFAM            9 2
     C                   MOVEL     *ZEROS        XXCOTA            9 2
     C                   MOVEL     *ZEROS        XXACOT            9 2
      *
     C     CMKY          CHAIN     PEA01                              74
      *
     C                   DOW       *IN74 = '0'
      *
     C                   IF        F1COTA <> *BLANKS
     C                   IF        EAARCD = F1COTA
     C                   EVAL      XXCOTA = XXCOTA  +  EAAMT
     C                   ENDIF
     C                   ENDIF
      *
     C                   IF        F1ACOT <> *BLANKS
     C                   IF        EAARCD = F1ACOT
     C                   EVAL      XXACOT = XXACOT  +  EAAMT
     C                   ENDIF
     C                   ENDIF
      *
     C     CMKY          READE     PEA01                                  74
     C                   ENDDO
     C
     C                   EVAL      XXTFAM = XXCOTA + XXACOT
     C
     C                   ENDSR                                                  SREAP
      *****************************************************************
     C     PrntData      Begsr
      *
     C                   IF        @@ROFL = 'N'
      *
     C                   EXSR      WRITEAR
     C                   EXSR      GETBAT#
      *
     C                   ENDIF
      *
     C                   move      GSFND         grp#
     C                   move      ELFYR         year#
     C                   move      ELEMP#        empl#             9 0
     C                   move      ELDIV         DIV#              5 0
     C                   Move      SVCLDT1       ##ccdt            8 0
     C                   Move      SVACDT1       ##nasdt           8 0
     C                   Move      XXTFAM        ##colamt          9 2
     C                   IF        @@ROFL = 'N'
     C                   Move      XXBAT#        ##retbat          9 0
     C                   ENDIF
      *
     C                   IF        *INOF = '1'
     C                   EXCEPT    HEADER
     C                   EVAL      *INOF = '0'
     C                   ENDIF
      *
     C                   except    prtdata
      *
      * Set Stop Collateral Flag to 'Y' in WDELP..
      *
     C                   IF        @@ROFL = 'N'
     C     ELKY1         CHAIN     PEL01
     C                   IF        %FOUND
     C                   EVAL      ELSTCO = 'Y'
     C                   UPDATE    PEL01
     C                   ENDIF
     C                   ENDIF
      *
     C                   Endsr
     *===============================================================*
     C     WRITEAR       BEGSR
      *
      * Write an A/R record with A/R code as C3CPCD and amount as
      * accumulated collateral paid
      *
     c                   clear                   mea0l6
     C                   EVAL      M6EACO#  = CO#
     C                   EVAL      M6EAFND  = GSFND
     C                   EVAL      M6EAEMP# = ELEMP#
     C                   EVAL      M6EADIV  = ELDIV
     C                   EVAL      M6EARDIV = ELDIV
     C                   EVAL      M6EAFYR  = ELFYR
     C                   EVAL      M6EAAMT  = XXTFAM
     C                   EVAL      M6EAARCD = XXCPCD
     C                   EVAL      M6EACTYP = GetARcat(XXCPCD)
     C                   EVAL      M6EAARTY = GetArType(XXCPCD)
     C                   EVAL      M6EADESC = GetArDesc(XXCPCD)
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M6EATRDT
     C                   MOVEL     SRDTO         M6EAPODT
     C                   MOVEL     SRDTO         M6EADT
     C                   EVAL      M6EAPGM  = Q1PGM
     C                   eval      M6EAUSER = Q1USER
     C                   MOVEL     DSTIME        M6EATIME
      *
     C                   WRITE     mea0l6
      *
      * Offset collateral paid by writing a -ve accumulated collateral
      * paid A/R with same transaction code
      *
     C                   IF        XXCOTA > 0
      *
     C                   EVAL      M6EAAMT  = (XXCOTA) * -1
     C                   EVAL      M6EAARCD = F1COTA
     C                   EVAL      M6EACTYP = GetARcat(F1COTA)
     C                   EVAL      M6EAARTY = GetArType(F1COTA)
     C                   EVAL      M6EADESC = GetArDesc(F1COTA)
      *
     C                   WRITE     MEA0L6
      *
     C                   ENDIF
      *
     C                   IF        XXACOT > 0
      *
     C                   EVAL      M6EAAMT  = (XXACOT) * -1
     C                   EVAL      M6EAARCD = F1ACOT
     C                   EVAL      M6EACTYP = GetARcat(F1ACOT)
     C                   EVAL      M6EAARTY = GetArType(F1ACOT)
     C                   EVAL      M6EADESC = GetArDesc(F1ACOT)
      *
     C                   WRITE     MEA0L6
      *
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     *===============================================================*
     *===============================================================*
     C     WRITERFB      BEGSR
      *
     C                   EXSR      getbat#
      *
     c                   clear                   PRN02
      *
     C                   EVAL      RNCO#  = CO#
     C                   EVAL      RNBAT# = XXBAT#
     C                   EVAL      RNFGRP = GSFND
     C                   EVAL      RNTGRP = GSFND
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RNFAUD
     C                   MOVEL     SRDTO         RNTAUD
     C                   EVAL      RNMIN  = 1
     C                   EVAL      RNBPF  = 'Y'
     C                   EVAL      RNBTYP = 'P'
     C                   EVAL      RNMTYP = 'A'
     C                   EVAL      RNIUSR = Q1USER
     C                   EVAL      RNIPGM = Q1PGM
     C                   EVAL      RNIDT  = RNTAUD
     C                   EVAL      RNITIM = DSTIME
     C                   EVAL      RNUSER = Q1USER
     C                   EVAL      RNPGM  = Q1PGM
     C                   EVAL      RNDATE = RNTAUD
     C                   EVAL      RNTIME = DSTIME
      *
     C                   WRITE     PRN02
      *
     C                   MOVEL     CO#           @@CO#             3
     C                   MOVEL     XXBAT#        ##BAT#            9
      *
     C                   CALL      'W0500C'
     C                   PARM                    @@CO#
     C                   PARM                    ##BAT#
     C                   PARM                    OROUTQ
      *
     C                   ENDSR
     *===============================================================*
     *===============================================================*
     C     GETBAT#       BEGSR
      *
     C                   MOVEL     *ZEROS        XXBAT#            9 0
     C     RNKY          SETGT     PRN01
     C     RNKY          READPE    PRN01
     C                   IF        NOT %EOF
     C                   EVAL      XXBAT# = RNBAT# + 1
     C                   ELSE
     C                   EVAL      XXBAT# = 1
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     *===============================================================*
     *===============================================================*
     C     initialize    Begsr

     C* Time & date
     C                   movel     @@co#         co#
     C                   movel     @@co#         ##co#
     C                   Time                    dsdttm
     C* Company name
     c                   call      comapi        compl
     c                   move      ##conm        CompanyNm
      *
     C* Printer file info
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
     C*** GET THE USER NAME...
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
      *
      *If swcpf/wcfl25 = 'Y'.. then Create return batch at FND break.
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60

     C                   Endsr
     *===============================================================*
     O*  Query Response File Error Report
     OQsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'Route to:'
     O**********             ORDIS1              60
     O**********             UPUSNM              99
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
     O**********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'Route to:'
     O                       ORDIS1              60
     O                       UPUSNM              99
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'Report Title:'
     O                                           61 'Collateral Dissolve Report'
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'Program Name:'
     O**********             ORPNAM              30
     O**********                                 36 'File:'
     O**********             ORFIL#              38
     O**********                                 46 'Copies:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'Hold:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'Output queue:'
     O**********             OROUTQ              30
     O**********                                 37 'Forms:'
     O**********             ORFORM              48
     O**********                                 56 'Save:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O          E            ROUTE          2
     O                                           32 'Company                  :'
     O                       @@CO#               +4
     O          E            ROUTE          2
     O                                           32 'No Open Claims as of date:'
     O                       XXOCDT1             +4 '  /  /    '
     O
     O          E            ROUTE          2
     O                                           32 'No Claims Activity Date  :'
     O                       XXNADT1             +4 '  /  /    '
      *
     O          E            ROUTE          2
     O                                           32 'Report Only              :'
     O                       @@ROFL              +4
     O*
     o          e            route            58
     o                                         +001 '(c)'
     o                       *year             +001
     o                                         +001 'StoneRiver, Inc.'
     o                                         +001 'All rights reserved.'
     o          e            route       1
     o                                         +001 'StoneRiver is a'
     o                                         +001 'trademark of'
     o                                         +001 'StoneRiver, Inc.'
     O          E            Header         1 03
     O                       dstime              09 '  :  :  '
     O                       CompanyNm           80
     O                       dsdate             130 '  /  /    '
     O*
     O          e            Header      0  3
     o                       q1pgm               11
     O                                           72 'Collateral Dissolve Report'
     O                                          126 'Page:'
     O                       page          Z    130
     O          e            Header         1
     O                                           06 'Group'
     O                                          +03 'Employer'
     O                                          +03 'Division'
     O                                          +03 'Year'
     O                                          +03 'Claim Close Date'
     O                                          +03 'No Activity Since Date'
     O                                          +03 'Collateral Amt Transferred'
     O                                          +03 'Return Premium'
     O          e            Header         1
     O                                           92 'to Premium'
     O                                          124 'Batch Number'
     O          e            prtdata        1
     O                       grp#                04
     O                       empl#         J     +2
     O                       Div#          J     +4
     O                       year#               +2
     O
     O                       ##ccdt              +4 '  /  /    '
     O                       ##nasdt            +09 '  /  /    '
     O                       ##colamt      J    +15
     O                       ##retbat      J    +12

     O          e            Disp01         2
     O                                           27 'Total# of Employer       :'
     O                       count_01      J    +12
     O          e            Disp01         4
     O                                           27 'Total Coll. transferred  :'
     O                       Total_01      J    +03
     O          e            Disp51         2
     O                                           27 'Grand Total# of Employer :'
     O                       count_02      J    +12
     O          e            Disp51         4
     O                                           27 'Grand Coll. transferred  :'
     O                       Total_02      J    +03
     O          e            Fot1           1
     O                                          +35 ' *** End  Of  Report *** '
     O          e            Fot2           1
     O                                          +35 ' *** No Records Found *** '
     O          e            Space#         1
     O          e            Skip#                1
