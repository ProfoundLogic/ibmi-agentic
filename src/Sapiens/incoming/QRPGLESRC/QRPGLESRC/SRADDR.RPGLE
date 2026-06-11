     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved. -
     H Sapiens is a trademark of Sapiens, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRADDR - RETRIEVE EMPLOYER NAME/ADDRESS INFORMATION.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   THIS PROGRAM WILL RETURN 7 50 CHAR FIELDS CONTAINING THE   *
     H**   NAME AND ADDRESS OF THE SPECIFIED EMPLOYER.                *
     H**                                                              *
     H**   BASED ON THE FLAG PASSED IN, THE FIELDS WILL EITHER BE     *
     H**   POPULATED FROM THE TOP OR FROM THE BOTTOM (QQODR = W).     *
     H**                                                              *
     H**   BASED ON 2ND FLAG, THE EMPLOYER NAME WILL PRINT FIRST OR   *
     H**   THE FEDERAL NAME WILL PRINT FIRST (QQNODR = F)             *
     H**                                                              *
     H**   BASED ON 3RD FLAG, THE EMPLOYER MAILING, PHYSICAL OR       *
     H**   BILLING ADDRESS WILL PRINT.  IF LEFT BLANK THEN THE NORMAL *
     H**   ORDER OF BILLING IF PRESENT THEN EMPLOYER ADDRESS FLAG.    *
     H**   IF MAILING OR PHYSICAL IS SPECIFIED BUT NOT PRESENT THEN   *
     H**   THE OTHER WILL BE USED.                                    *
     H**                                                              *
     H**   WITHIN BASE, THE ONLY PROGRAM THAT CALLS SRADDR WITH       *
     H**   QQAODR <> *BLANK IS W7413R. ALL OF THE OTHER PROGRAMS      *
     H**   CALL SRAODR WITH QQADDR = *BLANK; THEREFORE, IF QQAODR IS  *
     H**   BLANK OR 'B', SRADDR WILL RETURN THE CONTACT NAME (IF IT   *
     H**   EXISTS) POSITIONED IMMEDIATELY AFTER THE DBA NAME AND THE  *
     H**   FEDERAL NAME WILL BE SUPPRESSED. THE POSITION OF THE       *
     H**   CONTACT NAME WILL BE RETURNED IN QQCONL. IF QQAODR IS 'M'  *
     H**   OR 'P' SRADDR WILL NOT RETURN BILLNG CONTACT INFORMATION.  *
     H**   THE AUDIT REQUEST PROGRAM WOULD REQUIRE A LOT OF RE-WORK   *
     H**   TO IMPLEMENT THIS LOGIC FOR THE CONTACT NAME AND WILL BE   *
     H**   EXCLUDED AT THIS TIME. THE FEDERAL NAME WILL STILL         *
     H**   BE SUPPRESSED IN THE AUDIT REQUEST.                        *
     H**                                                              *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    03/03/05            PROJECT: WC1202               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/14/25 C:65  Project: 117417 (AmWins)                       *
     H*                Remove the concatenation of DBA for Amwins     *
     H*                as they always input the characters into name. *
     H*                                                               *
     H* 08/09/21 C:65  Project: Phw#97169 (Amwins)                    *
     H*                Remove hardcoding for Amwins for output '2'.   *
     H*                                                               *
     H* 12/27/19 C:318 Support: 84648                                 *
     H*                Corrected the name population to QQNAME2 for   *
     H*                the case having length more than 50.           *
     H*                                                               *
     H* 11/05/19 C:65  Project: Phw#84505 (Sunz)                      *
     H*                Reduce the space between PEO acronym and the   *
     H*                client name to one space.                      *
     H*                                                               *
     H* 04/30/18 C:65  Project: 150WAD                                *
     H*                CA policies should prefix the PEO acronym to   *
     H*                the beginning of the client name instead of    *
     H*                appending it to the end of the PEO name.       *
     H*                                                               *
     H* 01/15/18 C:65  Project: 150WCJ                                *
     H*                If a policy has an overriding PEO specified    *
     H*                then pull the PEO name from the indicated      *
     H*                additional named insured record.               *
     H*          C:136 Return Dec Page Fein                           *
     H*                                                               *
     H* 07/17/17 C:65  Project: 150WAP                                *
     H*                Process employers in 'C' type group the same   *
     H*                as 'M' type when employer # is not equal to 1. *
     H*                                                               *
     H* 05/02/17 C:65  Project: 148WAC                                *
     H*                Do not return c/o name for 148 output mode 2.  *
     H*                                                               *
     H* 03/20/17 C:65  Project: 148WAC                                *
     H*                Return PEO address and C/O PEO name if         *
     H*                processing as Amwins and output mode of "2".   *
     H*                                                               *
     H* 03/01/17 C:65  Project: WC2480                                *
     H*                Return 3 120a name fields for scriptura.       *
     H*                                                               *
     H* 02/24/17 C:65  Project: WC2480                                *
     H*                Add flexible field for PEO acronym to be used  *
     H*                when populated in place of L/C/F.              *
     H*                                                               *
     H* 01/14/14 C:136 Project: WC2034                                *
     H*                Add logic for PEO output flag '4':             *
     H*                Client name 'client of' PEO name               *
     H*                                                               *
     H* 01/10/13 C:065 PROJECT: WC2006 (Copied in by C:282)           *
     H*                ADD NEW PARMS TO RETURN FULLY FORMATED OUTPUT. *
     H*                IF THE NEW PARMS ARE PASSED INTO THE PROGRAM,  *
     H*                THEN RETURN ALL INFORMATION IN THE NEW ARRAY   *
     H*                FIELDS ALREADY FORMATTED COMPLETELY FOR        *
     H*                OUTPUT.  ADDITIONALLY, RETURN THE FEDERAL NAME *
     H*                OF THE PEO INSTEAD OF THE DBA IF IT EXISTS.    *
     H*                ALSO, CHANGE PGFL VALUE PASSED IN TO HAVE A    *
     H*                'B' VALUE INSTEAD OF BLANK IF PEO NAME SHOULD  *
     H*                BE INCLUDED IN THE ARRAY. ADD A SECOND SET OF  *
     H*                LENGTH FIELDS TO INDICATE HOW MANY ARRAY SLOTS *
     H*                A PARTICULAR ENTRY IS COMPRISED OF (NAME,      *
     H*                FEDERAL NAME, ETC).                            *
     H*                                                               *
     H* 04/24/12 C:136 SUPPORT: WA139 77700                           *
     H*                RETURN PEO FED NAME IN PREFERENCE TO PEO       *
     H*                DBA NAME                                       *
     H*                                                               *
     H* 03/15/12 C:136 SUPPORT: WA114 78313                           *
     H*                PRINT FED NAME ON BILLING OUTPUT IF CONTACT    *
     H*                NAME DOES NOT EXIST PER GW                     *
     H*                                                               *
     H* 02/03/12 C:136 SUPPORT: WA045 79187                           *
     H*                CORRECTION TO 78313 - THE CONTACT NAME IS      *
     H*                AN OPTIONAL FIELD. USE ADD LINE 1 AS           *
     H*                INDICATOR THAT BILLING ADDRESS EXISTS          *
     H*                                                               *
     H* 10/26/11 C:136 SUPPORT: WA114 78313                           *
     H*                DO NOT PRINT FED NAME AND DISPLAY CONTACT      *
     H*                NAME AFTER DBA NAME IF CALLED FOR BILLING      *
     H*                OR DEFAULT ADDRESS ORDER (##AODR = 'B' OR      *
     H*                *BLANK) PER GW                                 *
     H*                                                               *
     H* 04/04/11  C:255 SUPPORT:76354                                 *
     H*                 Display Contact name on the billing.          *
     H*                                                               *
     H* 10/06/10  C:65  Project: SUPP# 73836/74406                    *
     H*                 CORRECT EMPLOYER # CHECK TO USE VALUE PASSED. *
     H*                                                               *
     H* 08/06/10  C:65  Project: SUPP# 73836                          *
     H*                 ENSURE THE EMPLOYER NAME IS POPULATED FOR     *
     H*                 ALL PEO TYPES FOR EMPLOYER #1.                *
     H*                                                               *
     H* 07/21/07  C:65  Project: 139WCI/139WBU                        *
     H*                 ENSURE THE EMPLOYER NAME IS POPULATED FOR     *
     H*                 SINGLE PEO'S USING PEO OUTPUT MODE OF '3'.    *
     H*                                                               *
     H* 03/21/07  C:65  Project: 114WGH/114WMF/139WBU                 *
     H*                 DO NOT OVERRIDE EMPLR NAME WITH PEO NAME BUT  *
     H*                 INSTEAD PASS OUT ALL THREE NAMES FOR POL PG.  *
     H*                                                               *
     H* 12/27/05  C:65  Project: 139WBU                               *
     H*                 BUILD NAME/ADDRESS ARRAY BASED ON NEW OUTPUT  *
     H*                 CONTROL FLAG FOR PEOS FROM STATISTICAL FILE.  *
     H*                                                               *
     H* 04/18/05  C:53  Project: WC1279                               *
     H*                 Addtl PEO processing.                         *
     H*                                                               *
     H* 04/20/05  C:33  Project: 114WIO                               *
     H*                 Add parms for PEO processing.                 *
     H*****************************************************************
     F***  Begin Add  ***  04/20/05  **************************************
     FWMFNL     IF   E           K DISK
     F***  Begin Add  ***  04/20/05  **************************************
     FWMEML     IF   E           K DISK
     FWMA3L     IF   E           K DISK
      *** BEG ADD ***** 10/26/11 ********************************
     FWMECL1    IF   E           K DISK
      *** END ADD ***** 10/26/11 ********************************
     F*** begin add ***  01/15/18  **************************************
     FWMANL1    IF   E           K DISK
     F*** end add *****  01/15/18  **************************************
      *** beg add ***** 01/14/14 ******************************************
     d b               s              2  0
     d c               s              2  0
      *** end add ***** 01/14/14 ******************************************
     d***  begin add  ***  03/01/17  **************************************
     D                 DS
     D  arrName                1    480
     D                                     DIM(4)
     D  XXname1              001    120
     D  XXname2              121    240
     D  XXname3              241    360
     D  XXname4              361    480
     d***  begin add  ***  03/01/17  **************************************
      ***  BEGIN ADD ****  01/10/13  **************************************
     D                 DS
     D  AMN                    1    540
     D                                     DIM(9)
     D  XXAMN1               001    060
     D  XXAMN2               061    120
     D  XXAMN3               121    180
     D  XXAMN4               181    240
     D  XXAMN5               241    300
     D  XXAMN6               301    360
     D  XXAMN7               361    420
     D  XXAMN8               421    480
     D  XXAMN9               481    540
      ***  END ADD ******  01/10/13  **************************************

     D                 DS
     D  EMN                    1    350
     D                                     DIM(7)
     D  XXEMN1               001    050
     D  XXEMN2               051    100
     D  XXEMN3               101    150
     D  XXEMN4               151    200
     D  XXEMN5               201    250
     D  XXEMN6               251    300
     D  XXEMN7               301    350
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D*
     D***  Begin Add  ***  04/20/05  **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D PEOEXT          C                   CONST('L/C/F')
     D***  begin add  ***  02/24/17  **************************************
     d flexPEO         s             10
     D***  end add ******  02/24/17  **************************************
      *** beg add ***** 01/15/18 **********************
     d xxDecFein       s              9  0
      *** ebd add ***** 01/15/18 **********************

     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  End   Add  ***  04/20/05  **************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** begin add ***  01/15/18  **************************************
     C*
     C*  Additional Named Insured File
     C*
     C     ANKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1EMP#            9 0
     C                   KFLD                    X1DIV             5 0
     C                   KFLD                    XXSEQ#            5 0
     C*** end add *****  01/15/18  **************************************
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQODR             1
     C                   PARM                    QQNODR            1
     C                   PARM                    QQAODR            1
     C                   PARM                    QQEMN1           50
     C                   PARM                    QQEMN2           50
     C                   PARM                    QQEMN3           50
     C                   PARM                    QQEMN4           50
     C                   PARM                    QQEMN5           50
     C                   PARM                    QQEMN6           50
     C                   PARM                    QQEMN7           50
     C***  Begin Add  ***  04/20/05  **************************************
     C                   PARM                    QQADRL            3 0
     C                   PARM                    QQNAML            3 0
     C                   PARM                    QQFEDL            3 0
     C                   PARM                    QQPEO1           57
     C                   PARM                    QQPEO2           57
     C***  End   Add  ***  04/20/05  **************************************
     C***  BEGIN ADD  ***  12/27/05  **************************************
     C                   PARM                    QQPGFL            1
     C***  END ADD ******  12/27/05  **************************************
     C*** BEG ADD ***** 10/26/11 ******************************************
     C                   PARM                    QQCONL            3 0
     C*** END ADD ***** 10/26/11 ******************************************
     C***  BEGIN ADD  ***  01/10/13  **************************************
     C                   PARM                    QQAMN1           60
     C                   PARM                    QQAMN2           60
     C                   PARM                    QQAMN3           60
     C                   PARM                    QQAMN4           60
     C                   PARM                    QQAMN5           60
     C                   PARM                    QQAMN6           60
     C                   PARM                    QQAMN7           60
     C                   PARM                    QQAMN8           60
     C                   PARM                    QQAMN9           60
     C                   PARM                    QQPEOL            3 0
     C                   PARM                    QQDBAF            1
     C                   PARM                    QQAL2             3 0
     C                   PARM                    QQNL2             3 0
     C                   PARM                    QQFL2             3 0
     C                   PARM                    QQPL2             3 0
     C                   PARM                    QQCL2             3 0
     C***  END ADD ******  01/10/13  **************************************
     c***  begin add  ***  02/24/17  **************************************
     C                   PARM                    QQPEOExt         10
     c***  end add ******  02/24/17  **************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   PARM                    QQName1         120
     C                   PARM                    QQName2         120
     C                   PARM                    QQName3         120
     C                   PARM                    QQName4         120
     c***  end add ******  03/01/17  **************************************
     C                   PARM                    QQFein            9 0
     C*
     C*    SET UP AND INITALIZE
     C*
     C*** BEG ADD ***** 10/26/11 ******************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   MOVE      *OFF          AUDREQ
     C                   MOVEL     'W7231R'      ##PGMN           10
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGMN           10
     C                   PARM                    ##PGMX            1
     C     ##PGMX        IFEQ      *ON
     C                   MOVE      *ON           AUDREQ            1
     C                   ENDIF
      *
     C                   MOVE      *OFF          DEDINV
     C                   MOVEL     'W7325R'      ##PGMN           10
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGMN           10
     C                   PARM                    ##PGMX            1
     C     ##PGMX        IFEQ      *ON
     C                   MOVE      *ON           DEDINV            1
     C                   ENDIF
     C*** END ADD ***** 10/26/11 ******************************************
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C*** BEGIN ADD ***  10/06/10  ****************************************
     C                   MOVE      QQEMP#        SVEMP#            9 0
     C*** END ADD *****  10/06/10  ****************************************
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      *BLANKS       QQEMN1
     C                   MOVE      *BLANKS       QQEMN2
     C                   MOVE      *BLANKS       QQEMN3
     C                   MOVE      *BLANKS       QQEMN4
     C                   MOVE      *BLANKS       QQEMN5
     C                   MOVE      *BLANKS       QQEMN6
     C                   MOVE      *BLANKS       QQEMN7
     c***  begin add  ***  03/01/17  **************************************
     C                   Clear                   arrName
     C                   Z-add     1             index             1 0
     c***  end add ******  03/01/17  **************************************
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      18
     C                   MOVE      *BLANKS       QQPEO1
     C                   MOVE      *BLANKS       QQPEO2
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C***  BEGIN ADD  ***  12/27/05  **************************************
     C                   MOVE      *BLANK        XXPGFL            1
     C     Q1PRMS        IFGE      20
     C                   MOVE      QQPGFL        XXPGFL
     C                   ENDIF                                                  QQPGFL IFEQ
     C***  END ADD ******  12/27/05  **************************************
     C*** begin add ***  04/30/18  ****************************************
     C                   Move      *blank        xxCltAcro         1
     C                   If        xxPGFL = 'C'
     C                   Move      'Y'           xxCltAcro
     C                   Move      'B'           xxPGFL
     C                   Endif
     C                   If        xxPGFL = 'E'
     C                   Move      'Y'           xxCltAcro
     C                   Move      '3'           xxPGFL
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     c***  begin add  ***  02/24/17  **************************************
     C                   Clear                   flexPEO
     C     Q1PRMS        IFGE      38
     C                   If        QQPEOExt <> ' '
     C                   Eval      flexPEO = QQPEOExt
     C                   Endif
     C                   Endif
     c*
     C                   If        flexPEO = ' '
     C                   Eval      flexPEO = PEOExt
     C*** begn add ****  04/30/18  ****************************************
     C     Q1PRMS        Ifge      38
     C                   Eval      QQPEOExt = flexPEO
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     C                   Endif
     c***  end add ******  02/24/17  **************************************
     C***  BEGIN ADD ****  01/10/13  **************************************
     C                   MOVE      *BLANKS       AMN
     C                   Z-ADD     1             E                 2 0
     C***  END ADD ******  01/10/13  **************************************
     C*** begin add ***  01/15/18  **************************************
     C     EMKY          CHAIN     PEM01                              60
     C                   Z-add     EMPEONMIN     xxSeq#
     C                   Z-add     1             x1emp#
     C                   Z-add     0             x1div
     C*** end add *****  01/15/18  **************************************
     C*
     C                   MOVE      *BLANKS       EMN
     C                   MOVE      *BLANKS       XXEN1            50
     C                   MOVE      *BLANKS       XXEN2            50
     C                   MOVE      *BLANKS       XXFN1            50
     C                   MOVE      *BLANKS       XXFN2            50
     C                   MOVE      *BLANKS       XXAD1            50
     C                   MOVE      *BLANKS       XXAD2            50
     C                   MOVE      *BLANKS       XXAD3            50
     d*** begin add ***  03/20/17  ****************************************
     C*** begin del ***  08/09/21  ****************************************
     C*****              MOVE      *BLANKS       svadr1           50
     C*****              MOVE      *BLANKS       svadr2           50
     C*****              MOVE      *BLANKS       svadr3           50
     C*** end del *****  08/09/21  ****************************************
     d*** end add *****  03/20/17  ****************************************
     C*** BEG ADD ***** 10/26/11 ******************************************
     C                   MOVE      *BLANKS       XXCONT           50
     C*** END ADD ***** 10/26/11 ******************************************
     C***  BEGIN ADD  ***  07/21/07  **************************************
     C     FNKY          CHAIN     PFN01                              60
     C***  END ADD ******  07/21/07  **************************************
     C*
     C***  Begin Add  ***  04/20/05  **************************************
     C     XXEMP#        IFNE      1
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        ANDEQ     'Y'
     C*** BEG ADD *** 04/22/05 ****************************
     C     *IN60         OREQ      *OFF
     C     FNPEOF        ANDEQ     'M'
     C*** END ADD *** 04/22/05 ****************************
     C*** BEG ADD *** 07/17/17 ****************************
     C     *IN60         OREQ      *OFF
     C     FNPEOF        ANDEQ     'C'
     C*** END ADD *** 07/17/17 ****************************
     C                   EXSR      GETPEO
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C*
     C     EMKY          CHAIN     PEM01                              60
     C  N60EMKY          CHAIN     PA301                              61
     C     *IN60         IFEQ      *OFF
     C*
     C* ASSEMBLE NAMES.
     C*
     C*** BEGIN ADD ***  03/21/07  ************************************
     C     XXPGFL        IFNE      '3'
     C*** BEGIN ADD ***  07/21/07  ************************************
     C     FNPEOF        OREQ      'S'
     C     FNPEOF        OREQ      ' '
     C*** BEGIN ADD ***  08/06/10  ************************************
     C     FNPEOF        ORNE      ' '
     C*** BEGIN CHG ***  10/06/10  ****************************************
     C*********  XXEMP#    ANDEQ1
     C     SVEMP#        ANDEQ     1
     C*** END CHG *****  10/06/10  ****************************************
     C*** END ADD *****  08/06/10  ************************************
     C*** END ADD *****  07/21/07  ************************************
     C*** END ADD *****  03/21/07  ************************************
     C                   MOVEL     EMNAME        XXEN1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXEN1
     C                   MOVE      EMNAM2        XXEN2
     C*
     C*** BEGIN ADD *** 12/27/05 **************************************
     C*** BEGIN ADD ***  03/21/07  ************************************
     C*********  XXPGFL    IFNE '3'
     C*** END DEL *****  03/21/07  ************************************
     C*** END   ADD *** 12/27/05 **************************************
     C                   MOVEL     EMFDNM        XXFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        XXFN1
     C                   MOVE      EMFDN2        XXFN2
     C                   Eval      xxDecFein = EMfein
     C*** begin add ***  01/15/18  **************************************
     C*
     C* Override PEO name with Additional Named Insured from PEO.
     C*
     C                   If        xxSeq# <> 0 and xxpgfl = '1' and
     C                             (FNPEOF = 'C' or FNPEOF = 'M') and
     C                             svemp# <> 1
     C     anky          Chain     pan01                              60
     C                   If        *in60 = *off
     C                   Clear                   xxen1
     C                   Clear                   xxen2
     C                   Clear                   xxfn1
     C                   Clear                   xxfn2
     C                   Eval      xxen1 = ANname + ANnam2
     C                   Eval      xxen2 = %Subst(ANnam2:11:30)
     C                   Eval      xxDecFein = ANfein
     C                   Endif
     C                   Endif
     C*** end add *****  01/15/18  **************************************

     C*** BEGIN ADD *** 12/27/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/27/05 **************************************
     C*
     C* ASSEMBLE ADDRESS.
     C*
     C     QQAODR        IFEQ      'B'
     C     *IN61         ANDEQ     *ON
     C     QQAODR        OREQ      'B'
     C     *IN61         ANDEQ     *OFF
     C     A3STAT        ANDNE     'A'
     C     QQAODR        OREQ      'B'
     C     *IN61         ANDEQ     *OFF
     C     A3ADR1        ANDEQ     *BLANKS
     C                   MOVE      *BLANK        QQAODR
     C                   ENDIF                                                  QQAODR IFEQ
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVE      *BLANKS       XXSTZP           13
     C                   MOVE      *BLANKS       XXZIP4            4
     C                   MOVE      *BLANKS       XXST              2
     C                   MOVE      *BLANKS       XXZZ              9
     C                   MOVE      *BLANKS       XX5               5
     C                   MOVE      *BLANKS       XXST10           10
     C*
     C* SELECT APPROPRIATE ADDRESS.
     C*
     C                   SELECT
     C*
     C* USE BILLING ADDRESS.
     C*
     C     QQAODR        WHENEQ    'B'
     C*
     C                   MOVEL     A3ADR1        XXAD1
     C                   MOVEL     A3ADR2        XXAD2
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL     A3OOC         XXAD3
     C                   ELSE
     C                   MOVEL(P)  A3CTY         XXLAST
     C                   MOVEL     A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZZ
     C                   MOVE      A3ZIP2        XXZZ
     C                   EXSR      SRCSZ
     C                   ENDIF                                                  A3OOC IFNE
     C     A3CONT        IFNE      *BLANKS
     C                   MOVEL     A3CONT        XXCONT
     C                   CLEAR                   XXFN1
     C                   CLEAR                   XXFN2
     C                   ENDIF
     C*
     C* USE MAILING ADDRESS.
     C*
     C     QQAODR        WHENEQ    'M'
     C*
     C     EMMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVEL(P)  EMMCTY        XXLAST
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZZ
     C                   EXSR      SRCSZ
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL(P)  EMPCTY        XXLAST
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZZ
     C                   EXSR      SRCSZ
     C*
     C                   ENDIF                                                  EMMAD1 IFNE
     C*
     C* USE PHYSICAL ADDRESS.
     C*
     C     QQAODR        WHENEQ    'P'
     C*
     C     EMPAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL(P)  EMPCTY        XXLAST
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZZ
     C                   EXSR      SRCSZ
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVEL(P)  EMMCTY        XXLAST
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZZ
     C                   EXSR      SRCSZ
     C*
     C                   ENDIF                                                  EMMAD1 IFNE
     C*
     C* USE DEFAULT ADDRESS.
     C*
     C                   OTHER                                                  QQAODR = *BLANK
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C     AUDREQ        ANDEQ     *ON
     C                   EXSR      RTVMAD                                       RTV MAIL ADD FIRST
     C     AUDREQ        WHENEQ    *ON
     C                   EXSR      RTVDFA                                       RTV DEFAULT ADDRESS
     C     DEDINV        WHENEQ    *ON
     C                   EXSR      RTVBCA                                       RTV BILLING CONTACT
      *** BEG CHG ***** 02/03/12 **************************
     C***********XXCONT    IFEQ *BLANKS
     C     XXAD1         IFEQ      *BLANKS
      *** END CHG ***** 02/03/12 **************************
     C                   EXSR      RTVECN                                       RTV EMPLOYER CONTAT
     C                   EXSR      RTVDFA                                       RTV DEFAULT ADDRESS
     C                   ENDIF                                                  XXCONT = *BLANKS
     C                   OTHER
     C                   EXSR      RTVBCA
      *** BEG CHG ***** 02/03/12 **************************
     C***********XXCONT    IFEQ *BLANKS
     C     XXAD1         IFEQ      *BLANKS
      *** END CHG ***** 02/03/12 **************************
     C                   EXSR      RTVDFA                                       RTV DEFAULT ADDRESS
     C                   ENDIF                                                  XXCONT = *BLANKS
     C                   ENDSL
      *** END CHG ***** 10/26/11 ************************************
     C*
     C                   ENDSL
     C*
     C* ASSEMBLE NAMES AND ADDRESS INTO ARRAY FORMAT.
     C*
     C                   Z-ADD     1             A                 2 0
     C*** BEGIN ADD ***  01/10/13  ****************************************
     C*
     C* NEW ARRAY POPULATION.
     C*
     C     Q1PRMS        IFGE      32
     C*
     C     XXPGFL        IFEQ      'B'
     C     XXPGFL        OREQ      '3'
     C* PEO NAME
     C     XXPEO1        IFNE      *BLANKS
     C*
     C                   Z-ADD     E             QQPEOL
     C                   Z-ADD     1             QQPL2
     C                   MOVEL     XXPEO1        AMN(E)
     C     XXPEO2        IFEQ      *BLANKS
     c***  begin chg ****  02/24/17  **************************************
     C**********         CAT       PEOEXT:2      AMN(E)
     C*** begin add ***  04/30/18  ****************************************
     C                   If        xxCltAcro <> 'Y'
     C*** end add *****  04/30/18  ****************************************
     C*** begin chg ***  11/05/19  ****************************************
     C******             Eval      AMN(E) = %trim(AMN(E)) + '  ' +
     C******                       %trim(flexPEO)
     C                   Eval      AMN(E) = %trim(AMN(E)) + ' ' +
     C                             %trim(flexPEO)
     C*** end chg *****  11/05/19  ****************************************
     C*** begin add ***  04/30/18  ****************************************
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     c***  end chg ******  02/24/17  **************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ENDIF
     C                   ADD       1             E
     C*
     C     XXPEO2        IFNE      *BLANKS
     C                   MOVEL     XXPEO2        AMN(E)
     c***  begin chg ****  02/24/17  **************************************
     C**********         CAT       PEOEXT:2      AMN(E)
     C*** begin add ***  04/30/18  ****************************************
     C                   If        xxCltAcro <> 'Y'
     C*** end add *****  04/30/18  ****************************************
     C*** begin chg ***  11/05/19  ****************************************
     C******             Eval      AMN(E) = %trim(AMN(E)) + '  ' +
     C******                       %trim(flexPEO)
     C                   Eval      AMN(E) = %trim(AMN(E)) + ' ' +
     C                             %trim(flexPEO)
     C*** end chg *****  11/05/19  ****************************************
     C*** begin add ***  04/30/18  ****************************************
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     c***  end chg ******  02/24/17  **************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
      ***  begin chg  ***  01/15/18  **************************************
     C**************     Eval      arrName(index) = %trim(amn(e-1)) + ' ' +
     C**************               %trim(amn(e))
      ***  begin chg  ***  12/27/19  **************************************
     C**************     Eval      arrName(index) =
     C**************               %Subst(amn(e-1):1:50) + amn(e)
     C                   Eval      arrName(index) = %trim(amn(e-1))+ amn(e)
      ***  End chg  ***  12/27/19  **************************************
      ***  end   chg  ***  01/15/18  **************************************
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             QQPL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN2 IFNE
     C*
     C                   ENDIF                                                  XXPEO1 IFNE
     C*
     C                   ENDIF                                                  XXPGFL IFEQ
     C*
     C                   ENDIF                                                  Q1PRMS IFGE 32
     C*** END ADD *****  01/10/13  ****************************************
     C*
     C* COPY NAME FIELDS INTO ARRAY.
     C*
     C     QQNODR        IFEQ      'F'
     C     XXFN1         ANDNE     *BLANKS
     C*** BEGIN ADD *** 12/27/05 **************************************
     C*** BEGIN ADD ***  03/21/07  ************************************
     C*********  XXPGFL    ANDNE'3'
     C*** END DEL *****  03/21/07  ************************************
     C*** END   ADD *** 12/27/05 **************************************
     C*
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      17
     C                   Z-ADD     A             QQFEDL
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C                   MOVEL     XXFN1         EMN(A)
     C                   ADD       1             A
     C*
     C     XXFN2         IFNE      *BLANKS
     C                   MOVEL     XXFN2         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXFN2 IFNE
     C*
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      16
     C                   Z-ADD     A             QQNAML
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C                   MOVEL     XXEN1         EMN(A)
     C                   ADD       1             A
     C*
     C     XXEN2         IFNE      *BLANKS
     C                   MOVEL     XXEN2         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXFN2 IFNE
     C*
      *** BEG ADD ***** 10/26/11 ******************************************
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        EMN(A)
     C     Q1PRMS        IFGE      21
     C                   Z-ADD     A             QQCONL
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF                                                  XXCONT IFNE
      *** END ADD ***** 10/26/11 ******************************************

     C*** BEGIN ADD ***  01/10/13  ****************************************
     C*
     C* NEW ARRAY POPULATION.
     C*
     C     Q1PRMS        IFGE      32
     C* FEDERAL NAME
     C                   Z-ADD     E             QQFEDL
     C                   Z-ADD     1             QQFL2
     C                   MOVEL     XXFN1         AMN(E)
     C     XXFN2         IFEQ      *BLANKS
     C     QQDBAF        ANDEQ     'Y'
     C*** begin add ***  07/14/25  ****************************************
     C     cltid         ANDNE     '148'
     C*** end add *****  07/14/25  ****************************************
     C                   CAT       'DBA':2       AMN(E)
     C                   ENDIF
     C*** begin add ***  04/30/18  ****************************************
     C                   If        xxCltAcro = 'Y'
     C*** begin chg ***  11/05/19  ****************************************
     C******             Eval      amn(e) = %trim(flexPEO) + '  ' +
     C******                       %trim(amn(e))
     C                   Eval      amn(e) = %trim(flexPEO) + ' ' +
     C                             %trim(amn(e))
     C*** end chg *****  11/05/19  ****************************************
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42 and xxfn2 = ' '
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             E
     C*
     C     XXFN2         IFNE      *BLANKS
     C                   MOVEL     XXFN2         AMN(E)
     C     QQDBAF        IFEQ      'Y'
     C*** begin add ***  07/14/25  ****************************************
     C     cltid         ANDNE     '148'
     C*** end add *****  07/14/25  ****************************************
     C                   CAT       'DBA':2       AMN(E)
     C                   ENDIF
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
      ***  begin chg  ***  01/15/18  **************************************
     C**************     Eval      arrName(index) = %trim(amn(e-1)) + ' ' +
     C**************               %trim(amn(e))
      ***  begin chg  ***  12/27/19  **************************************
     C**************     Eval      arrName(index) =
     C**************               %Subst(amn(e-1):1:50) + amn(e)
     C                   Eval      arrName(index) =%trim(amn(e-1)) + amn(e)
      ***  End chg  ***  12/27/19  **************************************
      ***  end   chg  ***  01/15/18  **************************************
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             QQFL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN2 IFNE
     C* DBA NAME
     C                   Z-ADD     E             QQNAML
     C                   Z-ADD     1             QQNL2
     C                   MOVEL     XXEN1         AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42 and xxen2 = ' '
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             E
     C*
     C     XXEN2         IFNE      *BLANKS
     C                   MOVEL     XXEN2         AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
      ***  begin chg  ***  01/15/18  **************************************
     C**************     Eval      arrName(index) = %trim(amn(e-1)) + ' ' +
     C**************               %trim(amn(e))
      ***  begin Chg  ***  12/27/19  **************************************
     C**************     Eval      arrName(index) =
     C**************               %Subst(amn(e-1):1:50) + amn(e)
     C                   Eval      arrName(index) =%trim(amn(e-1)) + amn(e)
      ***  End Chg  ***  12/27/19  **************************************
      ***  end   chg  ***  01/15/18  **************************************
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             QQNL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN2 IFNE
     C* CONTACT NAME
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   Z-ADD     E             QQCONL
     C                   Z-ADD     1             QQCL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXCONT IFNE

     C                   ENDIF                                                  Q1PRMS IFGE 32
     C*** END ADD *****  01/10/13  ****************************************
     C*
     C                   ELSE
     C*
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      16
     C                   Z-ADD     A             QQNAML
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C                   MOVEL     XXEN1         EMN(A)
     C                   ADD       1             A
     C*
     C     XXEN2         IFNE      *BLANKS
     C                   MOVEL     XXEN2         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXEN2 IFNE
     C*
      *** BEG ADD ***** 10/26/11 ******************************************
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        EMN(A)
     C     Q1PRMS        IFGE      21
     C                   Z-ADD     A             QQCONL
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF                                                  XXCONT IFNE
      *** END ADD ***** 10/26/11 ******************************************
     C*
     C     XXFN1         IFNE      *BLANKS
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      17
     C                   Z-ADD     A             QQFEDL
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C                   MOVEL     XXFN1         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXFN1 IFNE
     C*
     C     XXFN2         IFNE      *BLANKS
     C                   MOVEL     XXFN2         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXFN2 IFNE
     C*
     C*** BEGIN ADD ***  01/10/13  ****************************************
     C*
     C* NEW ARRAY POPULATION.
     C*
     C     Q1PRMS        IFGE      32
     C* DBA NAME
     C                   Z-ADD     E             QQNAML
     C                   Z-ADD     1             QQNL2
     C                   MOVEL     XXEN1         AMN(E)
     C*** begin add ***  04/30/18  ****************************************
     C                   If        xxCltAcro = 'Y'
     C*** begin chg ***  11/05/19  ****************************************
     C******             Eval      amn(e) = %trim(flexPEO) + '  ' +
     C******                       %trim(amn(e))
     C                   Eval      amn(e) = %trim(flexPEO) + ' ' +
     C                             %trim(amn(e))
     C*** end chg *****  11/05/19  ****************************************
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42 and xxen2 = ' '
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             E
     C*
     C     XXEN2         IFNE      *BLANKS
     C                   MOVEL     XXEN2         AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
      ***  begin chg  ***  01/15/18  **************************************
     C**************     Eval      arrName(index) = %trim(amn(e-1)) + ' ' +
     C**************               %trim(amn(e))
      ***  begin Chg  ***  12/27/19  **************************************
     C**************     Eval      arrName(index) =
     C**************               %Subst(amn(e-1):1:50) + amn(e)
     C                   Eval      arrName(index) = %trim(amn(e-1)) + amn(e)
      ***  End Chg  ***  12/27/19  **************************************
      ***  end   chg  ***  01/15/18  **************************************
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             QQNL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN2 IFNE
     C* FEDERAL NAME
     C     XXFN1         IFNE      *BLANKS
     C                   Z-ADD     E             QQFEDL
     C                   Z-ADD     1             QQFL2
     C                   MOVEL     XXFN1         AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42 and xxfn2 = ' '
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN1 IFNE
     C*
     C     XXFN2         IFNE      *BLANKS
     C                   MOVEL     XXFN2         AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
      ***  begin chg  ***  01/15/18  **************************************
     C**************     Eval      arrName(index) = %trim(amn(e-1)) + ' ' +
     C**************               %trim(amn(e))
      ***  begin Chg  ***  12/27/19  **************************************
     C**************     Eval      arrName(index) =
     C**************               %Subst(amn(e-1):1:50) + amn(e)
     C                   Eval      arrName(index) = %trim(amn(e-1)) + amn(e)
      ***  End Chg  ***  12/27/19  **************************************
      ***  end   chg  ***  01/15/18  **************************************
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   ADD       1             QQFL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXFN2 IFNE
     C*
     C* CONTACT NAME
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        AMN(E)
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
     C                   Eval      arrName(index) = amn(e)
     C                   Eval      index = index + 1
     C                   Endif
     c***  end add ******  03/01/17  **************************************
     C                   Z-ADD     E             QQCONL
     C                   Z-ADD     1             QQCL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXCONT IFNE
     C*
     C                   ENDIF                                                  Q1PRMS IFGE 32
     C*** END ADD *****  01/10/13  ****************************************

     C                   ENDIF                                                  QQNODR IFEQ

      *** beg add ***** 01/14/14 ******************************************
      /free
       If xxPgfl = '4' and Q1prms >= 32;
          amn(e-1) = %trim(amn(e-1)) + ' client of';
          amn(e) = xxPeo1;
          qqPeol = e;
          qqPl2 = 1;
          e += 1;
          If xxPeo2 <> *blanks;
             amn(e) = xxPeo2;
             qqPl2 += 1;
             e +=1;
          Endif;
       Endif;
      /end-free
      *** end add ***** 01/14/14 ******************************************
     C*** begin add ***  03/20/17  ****************************************
     C*** begin del ***  08/09/21  ****************************************
     C*****              If        svadr1 <> ' '
     C*****              Eval      xxad1 = svadr1
     C*****              Eval      xxad2 = svadr2
     C*****              Eval      xxad3 = svadr3
     C*****              Endif
     C*** end del *****  08/09/21  ****************************************
     C*** begin add ***  03/20/17  ****************************************
     C*
     C* COPY ADDRESS FIELDS INTO ARRAY.
     C*
     C***  Begin Add  ***  04/20/05  **************************************
     C     Q1PRMS        IFGE      15
     C                   Z-ADD     A             QQADRL
     C                   ENDIF
     C***  End   Add  ***  04/20/05  **************************************
     C*
     C                   MOVEL     XXAD1         EMN(A)
     C                   ADD       1             A
     C*
     C     XXAD2         IFNE      *BLANKS
     C                   MOVEL     XXAD2         EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXAD2 IFNE
     C*
     C                   MOVEL     XXAD3         EMN(A)
     C                   ADD       1             A
     C*** BEGIN ADD ***  01/10/13  ****************************************
     C*
     C* NEW ARRAY POPULATION.
     C*
     C     Q1PRMS        IFGE      32
     C* ADDRESS
     C                   Z-ADD     E             QQADRL
     C                   Z-ADD     1             QQAL2
     C*
     C                   MOVEL     XXAD1         AMN(E)
     C                   ADD       1             E
     C*
     C     XXAD2         IFNE      *BLANKS
     C                   MOVEL     XXAD2         AMN(E)
     C                   ADD       1             QQAL2
     C                   ADD       1             E
     C                   ENDIF                                                  XXAD2 IFNE
     C*
     C                   MOVEL     XXAD3         AMN(E)
     C                   ADD       1             QQAL2
     C                   ADD       1             E
     C*
     C                   ENDIF                                                  Q1PRMS IFGE 32
     C*** END ADD *****  01/10/13  ****************************************
     C*
     C* RESHUFFLE ARRAY TO FILL FROM BOTTOM IF WINDOW ORDER SPECIFIED.
     C*

      /free
       Select;
       When qqOdr <> 'W';
       When Q1prms < 32 and a <> 8;
          a -= 1;
          b = 7;
          c = b - a;
          If Q1prms >= 15;
            qqAdrl += c;
          Endif;
          If Q1prms >= 16;
            qqNaml += c;
          Endif;
          If Q1prms >=17 and xxfn1 <> *blank;
            qqFedl += c;
          Endif;
          If Q1prms >=21 and xxCont <> *blank;
            qqConl += c;
          Endif;
          Dow a > 0;
             emn(b) = emn(a);
             emn(a) = *blanks;
             a -= 1;
             b -= 1;
          Enddo;
       When Q1prms >= 32 and e <> 10;
          e -= 1;
          b = 9;
          c = b - e;
            qqAdrl += c;
            qqNaml += c;
          If xxfn1 <> *blank;
            qqFedl += c;
          Endif;
          If xxCont <> *blank;
            qqConl += c;
          Endif;
          If qqPeol <> *zero;
            qqPeol += c;
          Endif;
          Dow e > 0;
             amn(b) = amn(e);
             amn(e) = *blanks;
             e -= 1;
             b -= 1;
          Enddo;
       Endsl;
      /end-free
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* MOVE INFORMATION TO ENTRY PARAMETERS TO RETURN NAME/ADDRESS.
     C*
     C                   MOVE      XXEMN1        QQEMN1
     C                   MOVE      XXEMN2        QQEMN2
     C                   MOVE      XXEMN3        QQEMN3
     C                   MOVE      XXEMN4        QQEMN4
     C                   MOVE      XXEMN5        QQEMN5
     C                   MOVE      XXEMN6        QQEMN6
     C                   MOVE      XXEMN7        QQEMN7
     C*** BEGIN ADD ***  01/10/13  ****************************************
     C     Q1PRMS        IFGE      32
     C                   MOVE      XXAMN1        QQAMN1
     C                   MOVE      XXAMN2        QQAMN2
     C                   MOVE      XXAMN3        QQAMN3
     C                   MOVE      XXAMN4        QQAMN4
     C                   MOVE      XXAMN5        QQAMN5
     C                   MOVE      XXAMN6        QQAMN6
     C                   MOVE      XXAMN7        QQAMN7
     C                   MOVE      XXAMN8        QQAMN8
     C                   MOVE      XXAMN9        QQAMN9
     C                   ENDIF
     C*** END ADD *****  01/10/13  ****************************************
     c***  begin add  ***  03/01/17  **************************************
     C                   If        Q1prms >= 42
     C                   Eval      QQName1 = xxName1
     C                   Eval      QQName2 = xxName2
     C                   Eval      QQName3 = xxName3
     C                   Eval      QQName4 = xxName4
     C                   Endif
     c***  end add ******  03/01/17  **************************************
      *** beg add ****** 01/15/18 **************************
     C                   If        Q1prms >= 43
     C                   Eval      qqFein = xxDecFein
     C                   Endif
      *** end add ****** 01/15/18 **************************
     C*
     C                   MOVE      '1'           *INLR
     C***  Begin Add  ***  04/20/05  **************************************
     C*====================================================================
     C* Gather PEO employer information
     C*====================================================================
     C*
     C     GETPEO        BEGSR
     C*
     C                   Z-ADD     1             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     Q1PRMS        ANDGE     18
     C*
     C* ASSEMBLE NAMES.
     C*
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL     EMFDNM        XXPEO1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        XXPEO1           50
     C                   MOVE      EMFDN2        XXPEO2           50
     C                   ELSE
     C                   MOVEL     EMNAME        XXPEO1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXPEO1           50
     C                   MOVE      EMNAM2        XXPEO2           50
     C                   ENDIF
     C*** begin add ***  01/15/18  **************************************
     C*
     C* Override PEO name with Additional Named Insured from PEO.
     C*
     C                   Eval      xxDecFein = EMfein
     C                   If        xxSeq# <> 0
     C     ANKY          CHAIN     PAN01                              60
     C                   If        *in60 = *off
     C                   Clear                   xxpeo1
     C                   Clear                   xxpeo2
     C                   Eval      xxPeo1 = ANname + ANnam2
     C                   Eval      xxPeo2 = %Subst(ANnam2:11:30)
     C                   Eval      xxDecFein = ANfein
     C                   Endif
     C                   Endif
     C*** end add *****  01/15/18  **************************************
     C*** begin add ***  03/20/17  ****************************************
     C*** begin del ***  08/09/21  ****************************************
     C*****              If        cltid = '148' and
     C*****                        xxpgfl = '2'  and
     C*****                        qqaodr = 'M'
     C*** begin del ***  05/02/17  ***************************************
     C*********          Eval      xxCont = 'c/o ' + %trim(xxpeo1)
     C*** end del *****  05/02/17  ***************************************
     C*****              Eval      svadr1 = emmad1
     C*****              Eval      svadr2 = emmad2
     C*****              MOVEL(P)  EMMCTY        XXLAST
     C*****              MOVEL     EMMST         XXST
     C*****              MOVEL     EMMZIP        XXZZ
     C*****              EXSR      SRCSZ
     C*****              Eval      svadr3 = xxad3
     C*****              Endif
     C*** end del *****  08/09/21  ****************************************
     C*** end add *****  03/20/17  ****************************************
     C*
     C                   MOVEL(P)  XXPEO1        QQPEO1
     C                   MOVEL(P)  XXPEO2        QQPEO2
     C*
      *** beg chg ***** 01/15/18 **************************
     C***     ' '           CHECKR    EMNAM2        LEN               3 0
     C***                   SELECT
     C***     LEN           WHENGT    10
     c******  begin chg ****  02/24/17  **************************************
     C*************         CAT       PEOEXT:2      QQPEO2
     C***                   Eval      QQPEO2 = %trim(QQPEO2) + '  ' +
     C***                             %trim(flexPEO)
     c******  end chg ******  02/24/17  **************************************
     C***                   OTHER
     c******  begin chg ****  02/24/17  **************************************
     C*************         CAT       PEOEXT:2      QQPEO1
     C***                   Eval      QQPEO1 = %trim(QQPEO1) + '  ' +
     C***                             %trim(flexPEO)
     c******  end chg ******  02/24/17  **************************************
     C***                   ENDSL
     C*** begin add ***  04/30/18  ****************************************
     C                   If        xxCltAcro <> 'Y'
     C*** end add *****  04/30/18  ****************************************
      /Free
       If qqPeo2 <> *Blanks;
       //*** begin chg ***  11/05/19  ****************************************
       //*****   qqPeo2 = %Trim(qqPeo2) + '  ' + %Trim(flexPeo);
       //*****Else;
       //*****   qqPeo1 = %Trim(qqPeo1) + '  ' + %Trim(flexPeo);
          qqPeo2 = %Trim(qqPeo2) + ' ' + %Trim(flexPeo);
       Else;
          qqPeo1 = %Trim(qqPeo1) + ' ' + %Trim(flexPeo);
       //*** end chg *****  11/05/19  ****************************************
       Endif;
      /End-Free
      *** end chg ***** 01/15/18 **************************
     C*** begin add ***  04/30/18  ****************************************
     C                   Endif
     C*** end add *****  04/30/18  ****************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/27/05 **************************************
     C     XXPGFL        IFNE      '1'
     C*** END   ADD *** 12/27/05 **************************************
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*** BEGIN ADD *** 12/27/05 **************************************
     C     XXPGFL        IFEQ      '3'
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   MOVEL     EMNAME        XXEN1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXEN1
     C                   MOVE      EMNAM2        XXEN2
     C*
     C                   MOVEL     EMFDNM        XXFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        XXFN1
     C                   MOVE      EMFDN2        XXFN2
     C*
     C                   Z-ADD     1             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/27/05 **************************************
     C*
     C                   ENDSR
     C***  End   Add  ***  04/20/05  **************************************
     C*
     C*****************************************************************
     C*
     C* SUBROUTINE TO CONCATENATE CITY, STATE, AND ZIP.
     C*
     C     SRCSZ         BEGSR
     C*
     C                   MOVEL     XXST          XXSTZP
     C                   MOVE      XXZZ          XXSTZP
     C                   MOVE      XXSTZP        XXZIP4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5
     C                   MOVEL     XXZZ          XXST10
     C                   MOVE      XX5           XXST10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4 IFEQ
     C*
     C                   MOVEL     XXSTZP        XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM            1
     C                   MOVEL     XXLAST        XXAD3
     C                   ENDSR                                                  SRCSZ
      *** BEG ADD ***** 10/26/11 *********************************
     C*===========================================================
      * RTV MAILING ADDRESS FIRST
     C*===========================================================
     C     RTVMAD        BEGSR
      *
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVEL(P)  EMMCTY        XXLAST
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZZ
     C                   EXSR      SRCSZ
     C                   ELSE
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL(P)  EMPCTY        XXLAST
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZZ
     C                   EXSR      SRCSZ
     C                   ENDIF                                                  EMMAD1 IFNE
      *
     C                   ENDSR                                                  RTVMAD
     C*===========================================================
      * RTV DEFAULT ADDRESS AS FLAGGED IN EMPLOYER MASTER
     C*===========================================================
     C     RTVDFA        BEGSR
      *
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMAD1        XXAD1
     C                   MOVEL     EMMAD2        XXAD2
     C                   MOVEL(P)  EMMCTY        XXLAST
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZZ
     C                   EXSR      SRCSZ
     C                   ELSE                                                   EMMPFL IFEQ
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C                   MOVEL(P)  EMPCTY        XXLAST
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZZ
     C                   EXSR      SRCSZ
     C                   ENDIF                                                  EMMPFL IFEQ
      *
     C                   ENDSR                                                  RTVDFA
     C*===========================================================
      * RTV BILLING CONTACT ADDRESS
     C*===========================================================
     C     RTVBCA        BEGSR
      *
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C                   MOVEL     A3ADR1        XXAD1
     C                   MOVEL     A3ADR2        XXAD2
      *
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL     A3OOC         XXAD3
     C                   ELSE
     C                   MOVEL(P)  A3CTY         XXLAST
     C                   MOVEL     A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZZ
     C                   MOVE      A3ZIP2        XXZZ
     C                   EXSR      SRCSZ
     C                   ENDIF                                                  A3OOC IFNE
      *
     C     A3CONT        IFNE      *BLANKS
     C                   MOVEL     A3CONT        XXCONT
      *** BEG ADD ***** 03/15/12 *************************
     C                   CLEAR                   XXFN1
     C                   CLEAR                   XXFN2
      *** END ADD ***** 03/15/12 *************************
     C                   ENDIF                                                  A3CONT
      *
     C                   ENDIF                                                  *IN61
      *
     C                   ENDSR                                                  RTVBCA
     C*===========================================================
      * RTV EMPLOYER CONTACT NAME
     C*===========================================================
     C     RTVECN        BEGSR
      *
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPCNT            1
      *
     C                   MOVE      '1'           XXPCNT
     C     ECKY          CHAIN     PEC01                              62
     C     *IN62         IFEQ      *OFF
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCONT
     C                   ELSE
     C     ECTITL        CAT       ECLSN:1       XXCONT
     C                   ENDIF                                                  ECFSN <> *BLANKS
     C                   ENDIF                                                  *IN62
      *
      *** BEG ADD ***** 03/15/12 *************************
     C     XXCONT        IFNE      *BLANKS
     C                   CLEAR                   XXFN1
     C                   CLEAR                   XXFN2
     C                   ENDIF
      *** END ADD ***** 03/15/12 *************************
      *
     C                   ENDSR                                                  RTVECN
      *** END ADD ***** 10/26/11 *********************************
