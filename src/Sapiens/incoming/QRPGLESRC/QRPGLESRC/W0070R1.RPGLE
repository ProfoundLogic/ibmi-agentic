     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H actgrp(*caller)
     H/TITLE W0070R1 - EDI R3 Reduced Earning Info
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER           *
     H*               EDI Reduced Earnings Info                       *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   191                                             *
     H* PROJECT:      WC1584               10/19/07                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 03/05/12  C:255 SUPPORT WA146 79435 (Xref - 78623)            *
     H*                 Check parameters passed by calling program    *
     H*                 before using added new parameter under support*
     H*                 74036.                                        *
     H*                                                               *
     H* 12/13/11  C:189 SUPPORT 78563                                 *
     H*                 Display the option irrespective of the        *
     H*                 EDI flag                                      *
     H*                                                               *
     H* 09/01/10  C:246 SUPPORT 74036                                 *
     H*                 Remove editable option in Inq Mode            *
     H*                                                               *
     H* 05/11/10  C:180  SUPPORT 72671
     H*                  REMOVE WEEK 52 EDIT.
     H*
     H* ***************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN61-69  Position Cursor
     H*  *IN71-89  ERROR INDICATORS                                   *
     H* ***************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0070f1   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDR3L     UF A E           K DISK
     F*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D PRDS            S             40    DIM(1) CTDATA
      *
     D XXDONE          S               N
     D XXERROR         S               N
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     C*
     C* Clear Subfile
     C                   EXSR      SRCLRSF
     C* Load Subfile
     C                   EXSR      SRLODSF
     C* Main Program loop
     C                   DOW       NOT *INKC AND NOT *INKL
     C* Display Subfile
     C                   EXSR      SRDSPSF
     C                   SELECT
     C* If F3 or F12 pressed exit program
     C                   WHEN      *INKC
     C                   EVAL      @@RTCD = 'A'
     C                   WHEN      *INKL
     C                   EVAL      @@RTCD = 'B'
     C                   OTHER
     C* Error Subfile
     C                   EXSR      SRERRCK
     C                   IF        not xxerror
     C* If not error then
     C* Update Database
     C                   EXSR      SRUPDAT
     C                   ENDIF                                                  not xxerror
     C* If process done then exit program
     C                   IF        xxdone
     C                   EVAL      @@RTCD = 'B'
     C                   LEAVE
     C                   ENDIF                                                  xxdone
     C*
     C                   ENDSL
     C                   ENDDO                                                  NOT *INKC AND *INKL
     C*
     C                   EVAL      *INLR=*ON
     C**************************************************************************
     C*  SRLODSF - Load Subfile using any data available in WDR3P
     C**************************************************************************
     C     SRLODSF       BEGSR
      ***        Write the subfile records, using any information
      ***        available in file
      *
     C     R3KY          CHAIN     PR301                              60
     C                   DOW       NOT *IN60
     C                   CLEAR                   SCR01S
      *
     C                   EVAL      SCREWK = R3REWK
     C                   EVAL      SCREWH = R3REWK
     C                   EVAL      SCARE = R3ARE
     C                   EVAL      SCDRE = R3DRE
      *
     C                   EVAL      RRN2 = RRN2 + 1
     C                   WRITE     SCR01S                                 95
     C                   IF        *IN95 = *ON OR RRN2 = 52
     C                   LEAVE
     C                   ENDIF
      *
     C     R3KY          READE     PR301                                  60
     C                   ENDDO                                                  NOT *IN66
     C*** Initialize remaining records with blank values
     C                   DOW       RRN2 < 52
     C                   EVAL      SCREWK = *ZEROS
     C                   EVAL      SCREWH = *ZEROS
     C                   EVAL      SCARE = *ZEROS
     C                   EVAL      SCDRE = *ZEROS
     C                   EVAL      RRN2 = RRN2 + 1
     C                   WRITE     SCR01S                                 95
     C                   ENDDO                                                  RRN2<52
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SRUPDAT - Update Database file
     C**************************************************************************
     C     SRUPDAT       BEGSR
     C                   READC     SCR01S                                 62
     C     *IN62         DOWEQ     *OFF
      *
     C                   IF        SCREWK <> *ZEROS
     C                   EVAL      XXREWK = SCREWH
     C     R3KY1         CHAIN     PR301                              60
     C                   EVAL      R3CO# = XXCO#
     C                   EVAL      R3FND = XXFND
     C                   EVAL      R3FYR = XXFYR
     C                   EVAL      R3CASE = XXCASE
     C                   EVAL      R3REWK = SCREWK
     C                   EVAL      R3ARE = SCARE
     C                   EVAL      R3DRE = SCDRE
     C                   IF        *IN60 = *OFF
     C                   UPDATE    PR301
     C                   ELSE
     C                   WRITE     PR301
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  SCREWK<>*BLANKS
     C                   READC     SCR01S                                 62
     C                   ENDDO                                                  *IN62=*OFF
     C                   EVAL      xxdone = *on
      *
     C                   ENDSR
     C**************************************************************************
     C*  SRERRCK - Error checking routine
     C**************************************************************************
     C     SRERRCK       BEGSR
     C                   EVAL      xxdone = *off
     C                   EVAL      xxerror = *off
     C                   READC     SCR01S                                 62
     C     *IN62         DOWEQ     *OFF
     C* Check for duplicate week record
      *
     C                   IF        SCREWK <> SCREWH
     C                   EVAL      XXREWK = SCREWK
     C     R3KY1         CHAIN     PR301                              60
     C                   IF        *IN60 = *OFF
     C                   SETON                                        21
     C                   EVAL      xxerror = *on
     C                   ENDIF                                                  *IN60 = *ON
     C***** BEG DELETE ********** 05/11/10 ****************************
     C* Check for Week > 52
     C****               IF        SCREWK > 52
     C****               SETON                                        22
     C****               EVAL      xxerror = *on
     C****               ENDIF                                                  SCREWK > 52
     C***** END DELETE ********** 05/11/10 ****************************
     C                   ENDIF                                                  SCREWK<>SCREWH
     C* Seton Next change indicator to mark record as changed for next READC cycle
     C                   SETON                                        05
     C                   UPDATE    SCR01S
     C                   READC     SCR01S                                 62
     C                   ENDDO                                                  *IN62=*OFF
     C*
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C*  SRDSPSF - Display Subfile
     C**************************************************************************
     C     SRDSPSF       BEGSR
     C     SCRRN3        IFEQ      0
     C                   Z-ADD     1             SCRRN3
     C                   ENDIF
     C     HRRN          IFEQ      0
     C                   Z-ADD     1             HRRN
     C                   ENDIF                                                  HRRN = 0
     C                   WRITE     SCR01
     C                   MOVE      *ON           *IN95
     C                   EXFMT     SCR01C
     C                   ENDSR
     C**************************************************************************
     C*  SRCLRSF - Clear Subfile
     C**************************************************************************
     C     SRCLRSF       BEGSR
     C                   MOVE      *OFF          *IN01
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      *ON           *IN01
     C                   MOVE      *OFF          *IN04
     C                   ENDSR
     C**************************************************************************
     C*  *INZSR - Initialization
     C**************************************************************************
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@RTCD            1
     C********BEG ADD ******** 09/01/10*******************************
     C                   PARM                    @@MODE            1
     C********END ADD ******** 09/01/10*******************************
     C***
     C* Parameter Lists
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** Define Key Lists
      *
      ***        SMCOL - Company Master
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      ***        WMFNL - Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ***        WMUPL - User Parameter File
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      ***        WMCML - Claim Master File
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     R3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     R3KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXREWK            2 0
      *** Define work fields
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C****
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        RRN2              5 0
     C********BEG ADD ******** 09/01/10*******************************
     C********beg add ******** 03/05/12*******************************
     C                   if        Q1PRMS > 6
     C********end add ******** 03/05/12*******************************
     C                   if        @@mode <> 'D'
     C                   Eval      *In35 = *ON
     C                   Eval      *In36 = *ON
     C                   Endif
     C********beg add ******** 03/05/12*******************************
     C                   endif
     C********end add ******** 03/05/12*******************************
     C********END ADD ******** 09/01/10*******************************
      *** Retrieve the User Parameter File
      *
     C     UPKY          CHAIN     PUP01                              60
     C     XXCO#         IFEQ      0
     C                   MOVE      UPCO#         XXCO#
     C                   ENDIF
      *
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
      *
      *** Retrieve Company Record
      *
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *** Retrieve Fund Record
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *ALL'*'       FNNAME
     C                   ENDIF                                                  *IN60=*ON
      *
     C*** BEG DEL ****** 12/13/11 *************************************
     C************       IF        FNEDI = 'Y'
     C************       EVAL      *INLR = *ON
     C************       RETURN
     C************       ENDIF                                                  FNEDI = 'Y'
     C*** END DEL ****** 12/13/11 *************************************
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C***
      *** Initialize Screen Fields
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      Q1USER        SCUSER
     C                   IF        @@PRDS = *BLANKS
     C                   MOVE      PRDS(1)       SCPRDS
     C                   ELSE
     C                   MOVE      @@PRDS        SCPRDS
     C                   ENDIF                                                  @@PRDS=*BLANKS
      *
     C                   MOVEL     XXFYR         SCYRCL
     C                   MOVE      XXCASE        SCYRCL
     C* Get Claim master record
     C     CMKY          CHAIN     PCM01                              60
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  XXSS1         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS2:0       SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS3:0       SCSS#
     C                   ENDIF                                                  CMSS#<>' '
     C*
     C     CMLSN         IFNE      *BLANKS
     C                   MOVEL     CMLSN         SCCLNM
     C                   ENDIF                                                  CMLSN<>' '
     C*
     C     CMFSN         IFNE      *BLANKS
     C                   CAT       ',':0         SCCLNM
     C                   CAT       CMFSN:1       SCCLNM
     C                   ENDIF                                                  CMFSN<>' '
     C*
     C     CMMID         IFNE      *BLANKS
     C                   CAT       CMMID:1       SCCLNM
     C                   ENDIF                                                  CMMID<>' '
     C     CMACDT        IFNE      *ZEROS
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                          YMD --> MDY
     C                   MOVEL     SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   ENDIF                                                  CMACDT<>*ZEROS
     C                   ENDSR
** PGM DESC
    EDI Reduced Earnings Maintenance
