     H*****************************************************************
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H  StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H*****************************************************************
     H* PROGRAM: F0277R01                                      *
     H* DESCRIPTION: WORK WITH PROVIDERS/SPECIALISTS           *
     H* Add Provider Specialist                                *
     H**********************************************************
     H* INDICATORS:                                            *
     H**********************************************************
     H**REVISIONS:
     H*
     H*04/10/13 C:079 Project: 114MAW RED2064
     H*                Lift to date edit
     H*
     H*03/18/13 C:079 Project: 114MAW RED2064
     H*                Change to require FEIN/Mod for CSI
     H*
     H*05/10/12 C:277 Project: 114MAW Priss #51
     H*                Add 2 working variable to campare begin and end date
     H*  
     H*04/13/12 C:277 Project: 114MAW
     H*                Option to Enter Multiple dates of treatment
     H*                Additional changes
     H*
     H* 12/12/11 C:277 Project: 114MAW
     H*                Option to Enter Multiple dates of treatment
     H*                Dates
     H*
     H* 09/11/06 C:163 PROJECT 114WMF
     H*                Back to Base - Medical Case Information(114MAD)
     H*
     H*  03/02/01 C:62  Project: 114MAD
     H*                 ADDED SEARCH AND CHANGED THE EDIT.
     H*
     H*  02/10/00 C:31  Project: MC227 (BASE ONLY)
     H*                 Case Prov: Rstr comments
     H*                 1. Add restrictions comment
     H*
     H* 07/01/99 C:62 PROJECT:MC207                            *
     H*               -IMPLEMENT SEQUENCE NUMBERS              *
     H*                                                        *
     H*  09/01/98 C:62  Project: 87MFX                         *
     H*                 New screen format.                     *
     H*                                                        *
     H*  07/14/98 C:61  Project: 98MAY                         *
     H*                 Restrict access to provider types      *
     H*                 1 and 4 only.                          *
     H*                                                        *
     H*  06/23/98 C:94  PROJECT: MC145                         *
     H*                 PUT SEARCH IN ON CHANGE MODE AND       *
     H*                 PROTECTED FEIN & MOD IF ALREADY        *
     H*                 HAD A VALUE                            *
     H*                                                        *
     H*  02/01/98 C:62  PROJECT: MC145                         *
     H*                 1) CHANGED OPTNM TO 'CASE PROVIDER -   *
     H*                    INFORMATION'                        *
     H*                 2) ADDED SA001R                        *
     H*                 3) ADDED FA703R AND F14=DARTS.         *
     H*                 4) REPLACED OLD SEARCHES.              *
     H*                       FROM F2141R TO F2335R.           *
     H*                       FROM F0031R TO F0032R.           *
     H*                 5) ADDED MAINTENANCE AND INIT FIELDS.  *
     H*                                                        *
     H* 01/08/98 C:86 PROJECT: MC145                           *
     H*               DISPLAY FILE F0277F01 HAS BEEN CHANGED TO*
     H*               TO PRINT A 2 LINE SUB-HEADING. THIS PRO- *
     H*               GRAM CALLS FA702R TO PRINT IT & USES LIKE*
     H*               DEFINES TO REDEFINE THE OMMITTED SCREEN  *
     H*               FIELDS.                                  *
     H**********************************************************
     H*
     FF0277F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FFAPPP     UF A E             DISK
     F                                     RENAME(PPP01:DBRCDF)
     FFAPPL2    IF   E           K DISK
     F*** BEGIN ADD *** 07/01/99 **************************************
     FFAPPL3    IF   E           K DISK
     F                                     RENAME(PPP01:PPP02)
     F*** END   ADD *** 07/01/99 **************************************
      ***** BEG DEL ******* 07/14/98 **********************************
     F*FMPDL   IF  E           K        DISK
      ***** END DEL ******* 07/14/98 **********************************
      ***** BEG ADD ******* 07/14/98 **********************************
     FFMPDL3    IF   E           K DISK
      ***** END ADD ******* 07/14/98 **********************************
     FFMP1L     IF   E           K DISK
     F*** BEGIN CHG *** 09/11/06 **************************************
     F********* FMCMP   IF  E                    DISK
     FFMCMP     UF   E             DISK
     F*** END   CHG *** 09/11/06 **************************************
     F******** BEGIN DELETE *********************** 01/08/98 **********
     F*************     FMCDL   IF  E           K        DISK
     F******** BEGIN DELETE *********************** 01/08/98 **********
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PPS01)
     FFTCCL1    IF   E           K DISK
     F                                     RENAME(PCC01:PCC02)
     F*** BEGIN DEL *** 03/02/01 **************************************
     F**********FTGTP   IF  E                    DISK
     F**********FTGTL   IF  E           K        DISK
     F**********  PGT01                             KRENAMEPGT02
     F*** END   DEL *** 03/02/01 **************************************
     F*** BEGIN ADD *** 09/11/06 **************************************
     FFAPPL8    IF   E           K DISK
     F                                     RENAME(PPP01:PPP04)
     F*** END   ADD *** 09/11/06 **************************************
     F*** BEG ADD *** 04/13/12 **************************************
     FFAPPL10   IF   E           K DISK
     F                                     RENAME(PPP01:PPP10)
     F*** END ADD *** 04/13/12 ****************************************
     D*
     D*
     D*** BEGIN DEL *** 04/13/12 **************************************
     D*****/COPY SCOPYMT01I
     D*** END   DEL *** 04/13/12 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Provider-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D*
     D                 DS
     D  XXCNTY                 1     15
     D  SCCNTY                 6      9
     D*** BEGIN ADD *** 09/01/98 **************************************
     D                 DS
     D  XX10A1                 1     10
     D  SCAREA                 1      3  0
     D  SCPHNA                 4      6  0
     D  SCPHNB                 7     10  0
     I*** END   ADD *** 09/01/98 **************************************
     D*******Beg Add********04/13/12**********************************
     DdtCheck          PR             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)

     DCvtToDate        PR              d
     D numDate                        8  0 Const
     D
     DDateTo7          PR             7  0
     D date                            d   Const
     D*
     DRtvCltId         pr                  extPgm('RTVCLTID')
     D CltId                          3
     DCVTDATE          pr                  extPgm('SRCVTDAT')
     D indatein                       8  0
     D infromfmt                      8
     D intofmt                        8
     D Outdate                       10
     D outerr                         1
     D indatein        s              8  0
     D infromfmt       s              8
     D intofmt         s              8
     D Outdate         s             10
     D outerr          s              1
     D CltId           s              3
     D xxfrun          s              1    inz('Y')
     D kkbgdt          s                   like(ppbgdt)
     D kkendt          s                   like(ppendt)
     D xxbgdt          s                   like(ppbgdt)
     D xxendt          s                   like(ppendt)
     D x1bgdt          s                   like(ppbgdt)
     D x1endt          s                   like(ppendt)
     D****** Beg Add******* 05/10/12 *********************************
     D x2bgdt          s                   like(ppbgdt)
     D x2endt          s                   like(ppendt)
     D****** End Add******* 05/10/12 *********************************
     D*
     D*******End Add********04/13/12**********************************
     C*******Beg Add********04/13/12**********************************
     C/COPY SCOPYMT01C
     C*******End Add********04/13/12**********************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      ##WCIN        ##WCIN            1
     C                   PARM      ##WCDK        ##WDCK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
     F*** BEGIN ADD *** 12/12/11 **************************************
     C     CASPRV        PLIST
     C                   PARM      *INKC         @@KC
     C                   PARM      *INKL         @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@RRN2
     C                   PARM      PPSEQ#        @@SEQ#            5 0
     C                   PARM      *ZEROS        @@STDT            7 0
     C                   PARM      *ZEROS        @@ENDT            7 0
     F*** END   ADD *** 12/12/11 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*** BEGIN DEL *** 03/02/01 **************************************
     C********** GTKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKTBID
     C**********           KFLD           KKTKEY
     C*** END   DEL *** 03/02/01 **************************************
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C     PPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C     PPKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     PPKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     SPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     CCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCNTY
     C*
     C*******Beg Add******** 04/13/12 **********************************
     C     PPKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKBGDT
     C*
     C     PPKY5         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C********End Add******** 04/13/12 ************************************
     C*** BEGIN ADD *** 12/12/11 **************************************
     C*******Beg Del******** 04/13/12 **********************************
     C*****              CALL      'RTVCLTID'
     C*****              PARM                    CLTID             3
     C*****CLTID         IFEQ      '114'
     C*****              MOVE      *ON           *IN65
     C*****              ELSE
     C*****              MOVE      *OFF          *IN65
     C*****              ENDIF
     C*******End Del******** 04/13/12 **********************************
     C*** END   ADD *** 12/12/11 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    SPKEY         KKKEY
     C     *LIKE         DEFINE    CCKEY         KKCNTY
     C     *LIKE         DEFINE    PPFEIN        KKFEIN
     C     *LIKE         DEFINE    PPMOD         KKMOD
     C     *LIKE         DEFINE    PPFEIN        HDFEIN
     C     *LIKE         DEFINE    PPMOD         HDMOD
     C     *LIKE         DEFINE    PPFRDT        HLFRDT
     C     *LIKE         DEFINE    PPTODT        HLTODT
     C*** BEGIN ADD *** 07/01/99 **************************************
     C     *LIKE         DEFINE    PPSEQ#        KKSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*** BEGIN DEL *** 03/02/01 **************************************
     C********** *LIKE     DEFN GTTBID    ##TBID
     C********** *LIKE     DEFN GTTBID    KKTBID
     C********** *LIKE     DEFN GTKEY     KKTKEY
     C*** END   DEL *** 03/02/01 **************************************
     C*** END   ADD *** 09/11/06 **************************************
     C*** BEGIN ADD *** 03/02/01 **************************************
     C     *LIKE         DEFINE    PPTCUR        XXTCUR
     C*** END   ADD *** 03/02/01 **************************************
     C*
     C*================================================================
     C*
     C*******Beg Del******** 04/13/12 **********************************
     C***/COPY SCOPYMT01C
     C*******End Del******** 04/13/12 **********************************
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEG ADD *** 04/13/12 ***************************************
     C*** BEG DEL *** 03/18/13 ***************************************
     C******
     C***/free*
     C**If**in65<>*on;
     C***/end-free
     C*** END DEL *** 03/18/13 ***************************************
     C*** END ADD *** 04/13/12 ***************************************
     C                   SETOFF                                       3031
     C*
     C*  VALIDATING FMPDP-FEIN
     C*
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C*
     C  N90PDKY          CHAIN     PPD01                              99
     C*
     C     SCFEIN        IFNE      *ZEROS
     C*
     C  N90
     CAN 99              SETON                                        9071
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 02/01/98 **************************************
     C*
     C*  VALIDATING FMPDP-PDSPCD
     C**********
     C**********  N90      SPKY      CHAINPSP01                99
     C**********
     C**********  N90 99             SETON                     9078
     C**********
     C*** END   DEL *** 02/01/98 **************************************
     C*
     C  N90SCFEIN        IFEQ      *ZEROS
     C     SCNAME        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        If  *in65<>*on;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*
     C*  MAKE SURE NOT TO CREATE DUPLICATE RECORDS OF FAPPP
     C*
     C     SCFEIN        IFNE      HDFEIN
     C     SCMOD         ORNE      HDMOD
     C*
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C*
     C  N90PPKY          CHAIN     PPP01                              99
     C*
     C  N90
     CANN99              SETON                                        9078
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
         endif;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*
     C* MAKE SURE THE TREATMENT SITE IS VALID
     C*
     C*** BEGIN CHG *** 03/02/01 **************************************
     C********** *IN90     IFEQ *OFF
     C********** SCTSIT    ANDNE*BLANKS
     C*
     C**********           MOVEL'TSIT'    KKTBID    P
     C**********           MOVELSCTSIT    KKTKEY    P
     C*
     C********** GTKY      CHAINPGT02                99
     C*
     C********** *IN99     IFEQ *ON
     C**********           SETON                     8990
     C**********           ENDIF
     C*
     C**********           ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCTSIT        ANDEQ     *BLANKS
     C                   SETON                                        8990
     C                   ENDIF
     C*** END   CHG *** 03/02/01 **************************************
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*
     C* MAKE SURE THE CURRENT TREATMENT SITE FLAG IS "Y" OR " ".
     C*
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        If  *in65<>*on;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C     *IN90         IFEQ      *OFF
     C     SCTCUR        ANDNE     *BLANKS
     C     SCTCUR        ANDNE     'Y'
     C*
     C                   SETON                                        8890
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        endif;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        If  *in65<>*on;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*
     C* MAKE SURE THERE AREN'T ANY OTHER CASE PROVIDERS WHERE TREATMENT
     C* SITE FLAG IS "Y"
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTCUR        ANDEQ     'Y'
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
     C*
     C     PPKY2         CHAIN     PPP04                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     SCSEQ#        IFNE      PPSEQ#
     C                   ADD       1             COUNT
     C                   ENDIF
     C*
     C     PPKY2         READE     PPP04                                  99
     C*
     C                   ENDDO
     C*
     C     COUNT         IFGT      0
     C*
     C                   SETON                                        8790
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        endif;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*** END   ADD *** 09/11/06 **************************************
     C*** BEGIN ADD *** 09/01/98 **************************************
     C*
     C* CHECK INITIAL TREATING PROV TO Y/N
     C*
     C  N90SCFL01        IFNE      'Y'
     C     SCFL01        ANDNE     'N'
     C                   SETON                                        9072
     C                   ENDIF
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C* CHECK PRIM CARE EQUAL TO Y/N
     C*
     C  N90SCPCP         IFNE      'Y'
     C     SCPCP         ANDNE     'N'
     C                   SETON                                        9073      09/01/98
     C                   ENDIF
     C*** BEG DEL *** 03/18/13 ***************************************
     C*******
     C**CHECK RESTRICTIONS EQUAL TO Y/N
     C*******
     C**N90SCRSTR        IFNE      'Y'
     C*****SCRSTR        ANDNE     'N'
     C*******            SETON                                        9077      09/01/98
     C*******            ENDIF
     C*******
     C**CHECK ACTIVE/INACTIVE
     C*******
     C**N90SCSTS         IFNE      'A'
     C*****SCSTS         ANDNE     'I'
     C*******            SETON                                        9080      09/01/98
     C*******            ENDIF
     C*** END DEL *** 03/18/13 ***************************************
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        If  *in65<>*on;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*
     C* IF SCFRDT MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFRDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        9074      09/01/98
     C*
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  *IN90 EQ *OFF
     C*
     C* IF SCTODT NOT ZERO THEN DATE MUST BE VALID
     C*
     C     SCTODT        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        9077      09/01/98
     C*
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCTODT NE 0
     C*
     C* IF SCTODT NOT ZERO THEN SCTODT MUST BE GE SCFRDT
     C*
     C     SCTODT        IFNE      *ZEROS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTODT
     C*
     C     HLTODT        IFLT      HLFRDT
     C*
     C                   SETON                                        9078      09/01/98
     C*
     C                   ENDIF                                                  TODT LT FRDT
     C*
     C                   ENDIF                                                  SCTODT NE 0
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
      /free
        endif;
      /end-free
     C*** END ADD *** 03/18/13 ***************************************
     C*** BEG ADD *** 04/13/12 ***************************************
     C*** BEG CHG *** 03/18/13 ***************************************
     C***/free
     C***   Else;
     C*
      /free
        If  *in65 = *on;
        //*** END CHG *** 03/18/13 ***************************************
        //Set all err indocator
           *In41 = *Off;
           *In42 = *Off;
           *In43 = *Off;
           *In45 = *Off;
           *In49 = *Off;
           *In50 = *Off;
        //Begin treatment date must be required

        //*** Beg Chg *** 03/18/13 ****************************************
        //*** If Scfrdt = *zeros;
         If Scfrdt = *zeros and *in90 = *off;
        //*** End Chg *** 03/18/13 ****************************************
           *In41 = *On;
           *In42 = *On;
           *In90 = *On;
           Leavesr;
         Endif;

        //Begin treatment date must be a valid date

        // **** beg chg 07/11/12 *****************************************

        //*** Beg Chg *** 03/18/13 ****************************************
        //*** If Sctodt = *zeros;
        //*** Beg Chg *** 04/10/13 ****************************************
        //*** If Sctodt = *zeros and *in90 = *off;
         If Sctodt <> *zeros and *in90 = *off;
        //*** End Chg *** 04/10/13 ****************************************
        //*** End Chg *** 03/18/13 ****************************************

        // **** end chg 07/11/12 *****************************************

         indatein = scfrdt;
         infromfmt = *Blanks;
         intofmt = *Blanks;
         Outdate = *Blanks;
         Outerr = *Blanks;
           callp  CVTDATE(indatein:infromfmt:intofmt:Outdate:
                  Outerr);
             If Outerr = '1';
               *In74 = *On;
               *In41 = *On;
               *In90 = *On;
               LeaveSr;
             Else;
               Scfrdt = %Int(OutDate);
             Endif;

        // **** beg chg 07/11/12 *****************************************

             Endif;

        // **** end chg 07/11/12 *****************************************

         //End treatment date must be required

        // **** beg dlt 07/11/12 *****************************************

        // ******** If Sctodt = 0 ;
        // ********   *In90 = *On;
        // ********   *In43 = *On;
        // ********    *In49 = *On;
        // ********  LeaveSr;
        // ********  EndIf;

        // **** end dlt 07/11/12 *****************************************

         //End treatment date must be a valid date

        // **** beg chg 07/11/12 *****************************************

        //*** Beg Chg *** 03/18/13 ****************************************
        //*** If Sctodt = *zeros;
        //*** Beg Chg *** 04/10/13 ****************************************
        //*** If Sctodt = *zeros and *in90 = *off;
         If Sctodt <> *zeros and *in90 = *off;
        //*** End Chg *** 04/10/13 ****************************************
        //*** End Chg *** 03/18/13 ****************************************

        // **** end chg 07/11/12 *****************************************

         indatein = sctodt;
         infromfmt = *Blanks;
         intofmt = *Blanks;
         Outdate = *Blanks;
         Outerr = *Blanks;
           callp  CVTDATE(indatein:infromfmt:intofmt:Outdate:
                  Outerr);
             If Outerr = '1';
               *In75 = *On;
               *In49 = *On;
               *In90 = *On;
               LeaveSr;
             ElSe;
               Sctodt = %Int(OutDate);
             Endif;

        // **** beg chg 07/11/12 *****************************************

         Endif;

        // **** end chg 07/11/12 *****************************************

        //End date must be greater that or eq to Start date

        //*** Beg Add ****** 05/10/12 *********************************
         x2bgdt = dateto7(cvttodate(Scfrdt));

        // **** beg chg 07/11/12 *****************************************

            x2endt = *zeros;

         If Sctodt <> *zeros;
            x2endt = dateto7(cvttodate(Sctodt));
         Endif;

        // **** end chg 07/11/12 *****************************************
        //*** End Add ****** 05/10/12 *********************************
        //*** Beg Chg ****** 05/10/12 *********************************
        //If sctodt < scfrdt;

        // **** beg chg 07/11/12 *****************************************

        If sctodt <> *zeros;

        // **** end chg 07/11/12 *****************************************
        //*** Beg Chg *** 03/18/13 ****************************************
        //*** If x2endt < x2bgdt;
        If x2endt < x2bgdt and *in90 = *off;
        //*** End Chg *** 03/18/13 ****************************************
        //*** End Chg ****** 05/10/12 *********************************
          *In50 = *on;
          *In90 = *on;
          LeaveSr;
        Endif;

        // **** beg chg 07/11/12 *****************************************

        Endif;

        // **** end chg 07/11/12 *****************************************

        //Date Must not be duplicate
        // set key list and working variables

          kkbgdt = dateto7(cvttodate(Scfrdt));

        // **** beg chg 07/11/12 *****************************************

            kkendt = *zeros;

         If Sctodt <> *zeros;
            kkendt = dateto7(cvttodate(Sctodt));
         Endif;

        // **** end chg 07/11/12 *****************************************

          xxbgdt = kkbgdt;
          xxendt = kkendt;

        //Add Mode
        If @@mode = 'A';

          chain ppky4 ppp10;
        //*** Beg Chg *** 03/18/13 ****************************************
        //**If %Found(FappL10) and xxbgdt = ppbgdt
        //**          and xxendt = ppendt;
            If %Found(FappL10) and xxbgdt = ppbgdt
                      and xxendt = ppendt and *in90 = *off;
        //*** End Chg *** 03/18/13 ****************************************
              *In78 = *On;
              *In90 = *On;
              LeaveSr;
            Else;
              *In78 = *Off;
            Endif;

        // Check Overlapping

        // **** beg chg 07/11/12 *****************************************

              If sctodt <> *Zeros;

        // **** end chg 07/11/12 *****************************************

          Chain ppky5 ppp10;
            Dow Not %Eof(Fappl10);
        //*** Beg Chg *** 03/18/13 ****************************************
        //*** If xxbgdt >= ppbgdt and xxbgdt <= ppendt
        //***   or xxendt >= ppbgdt and xxendt <= ppendt;
              If xxbgdt >= ppbgdt and xxbgdt <= ppendt
                or xxendt >= ppbgdt and xxendt <= ppendt and *in90=*off;
        //*** End Chg *** 03/18/13 ****************************************
                *in45 = *On;
                *in90 = *On;
                Leave;

              Else;
                *in45 = *Off;
              Endif;
              Reade ppky5 ppp10;
E01         Enddo;

        // **** beg chg 07/11/12 *****************************************
            Endif;
        // **** end chg 07/11/12 *****************************************

        Else;

        //Change Mode

        //*** Beg Chg *** 03/18/13 ****************************************
        //***If xxfrun = 'Y';
          If xxfrun = 'Y' and *in90 = *off;
        //*** End Chg *** 03/18/13 ****************************************
             x1bgdt = ppbgdt;
             x1endt = ppendt;
             xxfrun = 'N';
          Endif;
          Chain ppky5 ppp10;
            Dow Not %Eof(FappL10);
              If xxbgdt >= ppbgdt and xxbgdt <= ppbgdt
                 or xxendt >= ppendt and xxendt <= ppendt;
                   If x1bgdt = ppbgdt and x1endt = ppendt;
                   Reade ppky5 ppp10;
                   Iter;
E01                Endif;
                   *In45 = *On;
                   *In90 = *On;
                   Leave;
              Else;
              *In45 = *Off;
E01           Endif;
         // Read Next record
              Reade ppky5 ppp10;
            Enddo;
E01
        Endif;
        Endif;
      /end-free
     C*** END ADD *** 04/13/12 ***************************************
     C*** BEG ADD *** 03/18/13 ***************************************
     C*
     C* CHECK RESTRICTIONS EQUAL TO Y/N
     C*
     C  N90SCRSTR        IFNE      'Y'
     C     SCRSTR        ANDNE     'N'
     C                   SETON                                        9077      09/01/98
     C                   ENDIF
     C*
     C* CHECK ACTIVE/INACTIVE
     C*
     C  N90SCSTS         IFNE      'A'
     C     SCSTS         ANDNE     'I'
     C                   SETON                                        9080      09/01/98
     C                   ENDIF
     C*** END ADD *** 03/18/13 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       3031
     C                   SETON                                        40
     C*** BEGIN ADD *** 02/01/98 **************************************
     C                   EXSR      DATCVT
     C*** END   ADD *** 02/01/98 **************************************
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C     HFLD          OREQ      'SCNAME'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##FEIN            9
     C                   PARM      *ZEROS        ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/14/98 **********************************
     C*
     C     ##RTCD        IFNE      '*'
     C     ##FEIN        OREQ      *ZEROS
     C                   SETON                                        3090
     C                   ELSE
     C*
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C                   MOVE      ##FEIN        KKFEIN
     C                   MOVE      ##MOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C                   MOVEL     PDNAME        SCNAME
     C                   MOVEL     PDWCCE        SCWCCE
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   Z-ADD     PDAREA        SCAREA
     C                   MOVEL     PDPHN#        SCPHNA
     C                   MOVE      PDPHN#        SCPHNB
     C*** END   ADD *** 09/01/98 **************************************
     C                   MOVEL     PDSPCD        SCSPCD
     C*
     C                   MOVEL     SCSPCD        KKKEY
     C*
     C     SPKY          CHAIN     PSP01                              99
     C*
     C  N99              MOVEL     SPDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C*
     C*** BEGIN ADD *** 09/01/98 **************************************
     C     P1KY          CHAIN     PP101                              99
     C*
     C  N99              MOVE      P1CNTY        SCCNTY
     C   99              MOVE      *BLANKS       SCCNTY
     C*
     C                   MOVEL(P)  P1CNTY        KKCNTY
     C*
     C     CCKY          CHAIN     PCC02                              99
     C*
     C                   MOVEL     CCDESC        SCCTNM
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   SETON                                        3190
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   ENDIF                                                  *IN40
     C*** BEGIN ADD *** 09/11/06 **************************************
     C     HFLD          IFEQ      'SCTSIT'
     C*
     C*** BEGIN CHG *** 03/02/01 **************************************
     C**********           CALL 'F2380R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C**********           PARM *ZEROS    ##RRN   90
     C**********           PARM 'TSIT'    ##TBID
     C*
     C********** *INKL     IFEQ *ON
     C********** ##RRN     OREQ *ZEROS
     C*
     C**********           SETON                     31
     C*
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPGT01                99
     C*
     C**********           MOVELGTKEY     SCTSIT    P
     C**********           MOVELGTDESC    SCTSDS    P
     C**********           SETON                     32
     C*
     C**********           ENDIF
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##FEIN            9
     C                   PARM      *ZEROS        ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
     C*
     C     ##RTCD        IFNE      '*'
     C     ##FEIN        OREQ      *ZEROS
     C                   SETON                                        3190
     C                   ELSE
     C*
     C                   MOVE      ##FEIN        KKFEIN
     C                   MOVE      ##MOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C                   MOVEL(P)  PDNAME        SCTSIT
     C*
     C                   SETON                                        3290
     C*
     C                   ENDIF
     C*** END   CHG *** 03/02/01 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C*** BEGIN ADD *** 02/01/98 **************************************
     C                   EXSR      DATCVT
     C*** END   ADD *** 02/01/98 **************************************
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 12/12/11 **************************************
     C*** BEGIN DEL *** 04/13/12 **************************************
     C*================================================================
     C* KIDO - F09=CASE PROVIDERS START/END DATES
     C*================================================================
     C*****KIDO          BEGSR
     C*****
     C*****              CALL      'F0282R'      CASPRV
     C*****
     C*****              SETON                                        90
     C* Current Start date
     C*****              Z-ADD     @@STDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTMDYY
     C*****              MOVEL     SRDTO         SCFRDT
     C* Current End date
     C*****              Z-ADD     @@ENDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTMDYY
     C*****              MOVEL     SRDTO         SCTODT
     C*****
     C*****CLTID         IFEQ      '114'
     C*****@@MODE        ANDEQ     'D'
     C*****@@RRN         CHAIN     DBRCDF                             99
     C******IN99         IFEQ      *OFF
     C*****              MOVE      @@STDT        PPFRDT
     C*****              MOVE      @@ENDT        PPTODT
     C*****              UPDATE    DBRCDF
     C*****              ENDIF
     C*****              ENDIF
     C*****
     C*****              ENDSR
     C*** END   DEL *** 04/13/12 **************************************
     C*** END   ADD *** 12/12/11 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   MOVEL     *ZEROS        XX10A1
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*** BEGIN ADD *** 09/11/06 **************************************
     C                   MOVEL     *BLANKS       SCTSIT
     C                   MOVEL     *BLANKS       SCTCUR
     C*** END   ADD *** 09/11/06 **************************************
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   Z-ADD     *ZEROS        HDFEIN
     C                   Z-ADD     *ZEROS        HDMOD
     C                   Z-ADD     *ZEROS        SCSEQ#
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCSPCD
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPCP
     C                   MOVE      *BLANKS       SCWCCE
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   Z-ADD     *ZEROS        SCAREA
     C                   Z-ADD     *ZEROS        SCPHNA
     C                   Z-ADD     *ZEROS        SCPHNB
     C*** END   ADD *** 09/01/98 **************************************
     C                   MOVE      *BLANKS       SCRSTR
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   MOVE      *BLANKS       SC40A2
     C*** END ADD *** 02/10/00 ****************************************
     C                   MOVE      *BLANKS       SCCNTY
     C                   MOVE      *BLANKS       SCCTNM
     C                   MOVE      'A'           SCSTS
     C                   Z-ADD     *ZEROS        SCFRDT
     C                   Z-ADD     *ZEROS        SCTODT
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   MOVE      *BLANKS       SCFL01
     C                   MOVE      *ZEROS        XX10A1
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   ELSE
     C*** BEGIN ADD *** 09/11/06 **************************************
     C                   MOVEL(P)  PPTSIT        SCTSIT
     C*** END   ADD *** 09/11/06 **************************************
     C*** BEGIN DEL *** 03/02/01 **************************************
     C**********           MOVEL'TSIT'    KKTBID    P
     C**********           MOVELPPTSIT    KKTKEY    P
     C*
     C********** GTKY      CHAINPGT02                99
     C*
     C********** *IN99     IFEQ *OFF
     C**********           MOVELGTDESC    SCTSDS    P
     C**********           ELSE
     C**********           MOVEL*BLANKS   SCTSDS    P
     C**********           ENDIF
     C*** END   DEL *** 03/02/01 **************************************
     C*
     C*** BEGIN ADD *** 09/11/06 **************************************
     C                   MOVEL(P)  PPTCUR        SCTCUR
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C                   Z-ADD     PPFEIN        SCFEIN
     C                   Z-ADD     PPMOD         SCMOD
     C                   Z-ADD     PPFEIN        HDFEIN
     C                   Z-ADD     PPMOD         HDMOD
     C                   Z-ADD     PPFEIN        KKFEIN
     C                   Z-ADD     PPMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL     PDNAME        SCNAME
     C  N99              MOVEL     PDWCCE        SCWCCE
     C*** BEGIN ADD *** 07/01/99 **************************************
     C                   Z-ADD     PPSEQ#        SCSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*** BEGIN ADD *** 09/01/98 **************************************
     C   99              MOVE      PP40A1        SCNAME
     C   99              MOVE      *BLANKS       SCWCCE
     C                   MOVEL     PPFL01        SCFL01
     C*
     C     PP10A1        IFEQ      *BLANKS
     C     PP10A1        OREQ      *ZEROS
     C*
     C  N99              Z-ADD     PDAREA        SCAREA
     C  N99              MOVEL     PDPHN#        SCPHNA
     C  N99              MOVE      PDPHN#        SCPHNB
     C*
     C                   ELSE
     C*
     C                   MOVEL     PP10A1        XX10A1
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C     PPSPCD        IFEQ      *BLANKS
     C*
     C                   MOVEL     PDSPCD        SCSPCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     PPSPCD        SCSPCD
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCSPCD        KKKEY
     C*
     C     SPKY          CHAIN     PSP01                              99
     C*
     C  N99              MOVEL     SPDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C*
     C                   MOVE      PPPCP         SCPCP
     C                   MOVE      PPRSTR        SCRSTR
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   MOVEL(P)  PP40A2        SC40A2
     C*** END ADD *** 02/10/00 ****************************************
     C*
     C     P1KY          CHAIN     PP101                              99
     C*
     C  N99              MOVE      P1CNTY        SCCNTY
     C   99              MOVE      *BLANKS       SCCNTY
     C*
     C                   MOVEL(P)  P1CNTY        KKCNTY
     C*
     C     CCKY          CHAIN     PCC02                              99
     C*
     C                   MOVEL     CCDESC        SCCTNM
     C*
     C                   MOVE      PPSTS         SCSTS
     C*
     C                   Z-ADD     PPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     PPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   ENDIF                                                  *IN40
     C*** BEG ADD *** 06/23/98 ************************************
     C     SCFEIN        IFNE      *ZEROS
     C     SCMOD         ORNE      *ZEROS
     C                   SETOFF                                       10
     C                   ELSE
     C                   SETON                                        10
     C                   ENDIF
     C*** END ADD *** 06/23/98 ************************************
     C*** BEGIN ADD *** 04/13/12 **************************************
      /free
         If *in65 = *on;

           Scfrdt = DTCHECK(ppbgdt);

        // **** beg chg 07/11/12 *****************************************

           ppendt = *zeros;

         If Sctodt <> *zeros;
           ppendt = dateto7(cvttodate(Sctodt));
         Endif;

        // **** beg chg 07/11/12 *****************************************

         Endif;
      /end-free
     C*** END   ADD *** 04/13/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C* ASSIGN SEQUENCE NUMBER
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C*
     C     PPKY3         SETGT     PPP02
     C     PPKY2         READPE    PPP02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     PPSEQ#        ADD       1             SCSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             SCSEQ#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   MOVE      CMCO#         PPCO#
     C                   MOVE      CMSUB#        PPSUB#
     C                   MOVE      CMFND         PPFND
     C                   MOVE      CMFYR         PPFYR
     C                   MOVE      CMCASE        PPCASE
     C                   MOVE      SCSEQ#        PPSEQ#
     C                   MOVE      SCFEIN        PPFEIN
     C                   MOVE      SCMOD         PPMOD
     C*
     C     SCFEIN        IFEQ      *ZEROS
     C*
     C                   MOVEL     SCNAME        PP40A1
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   MOVE      XX10A1        PP10A1
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       PP40A1
     C                   MOVE      *ZEROS        PP10A1
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCPCP         PPPCP
     C                   MOVEL     SCRSTR        PPRSTR
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   MOVEL(P)  SC40A2        PP40A2
     C*** END ADD *** 02/10/00 ****************************************
     C                   MOVEL     SCSTS         PPSTS
     C*** BEGIN ADD *** 09/01/98 **************************************
     C                   MOVEL     SCFL01        PPFL01
     C*** END   ADD *** 09/01/98 **************************************
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPTODT
     C*** BEGIN ADD *** 09/11/06 **************************************
     C                   MOVEL(P)  SCTSIT        PPTSIT
     C                   MOVEL(P)  SCTCUR        PPTCUR
     C                   MOVEL(P)  SCSPCD        PPSPCD
     C*** END   ADD *** 09/11/06 **************************************
     C*** BEGIN ADD *** 04/13/12 **************************************
      /free
         If *in65 = *on;

           ppbgdt = dateto7(cvttodate(Scfrdt));
           ppendt = dateto7(cvttodate(Sctodt));

         Endif;
      /end-free
     C*** END   ADD *** 04/13/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=SEARCH
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C*** BEGIN ADD *** 12/12/11 **************************************
     C*** BEGIN DEL *** 04/13/12 **************************************
     C******INKI         CASEQ     *ON           KIDO                           F09=START/END DATES
     C*** END   DEL *** 04/13/12 **************************************
     C*** END   ADD *** 12/12/11 **************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD *** 09/11/06 **************************************
     C     PPTCUR        IFEQ      'Y'
     C     *IN09         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  PPTSIT        CMTSIT
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   Z-ADD     PPDT          CMDT
     C                   Z-ADD     PPTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 03/02/01 **************************************
     C                   ELSE
     C     XXTCUR        IFEQ      'Y'
     C     *IN09         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  *BLANKS       CMTSIT
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   Z-ADD     PPDT          CMDT
     C                   Z-ADD     PPTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 03/02/01 **************************************
     C*
     C                   ENDIF
     C*** END   CHG *** 09/11/06 **************************************
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* GET CLAIM MASTER RECORD.
     C*
     C*** BEGIN CHG *** 09/11/06 **************************************
     C********** @@RRN2    CHAINPCM01                99
     C     @@RRN2        CHAIN(N)  PCM01                              99
     C*** END   CHG *** 09/11/06 **************************************
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
      *
     C*********** END   CHANGE ******************* 01/08/98 ***********
     C********* END   DELETE ************************ 01/08/98 ********
     C*********  CDKY      CHAINPCD01                99
     C*
     C*********  *IN99     IFEQ '0'
     C*
     C*********            MOVELCDLSN     ##LAST 56
     C*********            MOVELCDFSN     ##FRST 15
     C*********            MOVE '1'       ##TERM  1
     C*********            CALL 'SRNAME'
     C*********            PARM           ##LAST
     C*********            PARM           ##FRST
     C*********            PARM           ##TERM
     C*
     C*********            MOVEL##LAST    SCCLNM
     C*
     C*********            ELSE
     C*
     C*********            MOVEL*BLANKS   SCCLNM
     C*
     C*********            ENDIF                           *IN99
     C********* END   DELETE ************************ 01/08/98 ********
     C*******Beg Add******** 04/13/12 **********************************
      /free
         CallP RtvCltId(CltId);
           If CltId = '114';
             *In65 = *On;
           Else;
             *In65 = *Off;
           Endif;
      /end-Free
     C*******End Add******** 04/13/12 **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C*
     C                   TIME                    DFDTTM
     C*
     C                   MOVEL     DFTIME        PPTIME
     C*
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        PPIUSR
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPIPGM
     C                   MOVEL     Q1PGM         PPPGM
     C*
     C                   TIME                    DFDTTM
     C*
     C                   MOVE      DFTIME        PPITIM
     C                   MOVE      DFTIME        PPTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPIDT
     C                   MOVEL     SRDTO         PPDT
     C                   WRITE     DBRCDF
     C                   FEOD      FAPPP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
