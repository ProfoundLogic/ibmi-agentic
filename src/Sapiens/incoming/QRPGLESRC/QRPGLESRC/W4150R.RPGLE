     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4150R - WORK WITH AGENT QUOTE STATUS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF AGENT QUOTE*
     H*                STATUS RECORDS BY NAME OR EMP #.  EACH RECORD  *
     H*                CAN BE ACCESSED FOR UPDATE, DISPLAY OR         *
     H*                SELECTION DEPENDING UPON THE CALLING PROGRAM.  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0553R   - WORK WITH AGENT HISTORY                         *
     H*    W0553R02 - AGENT HISTORY INFORMATION                       *
     H*  CALLS TO:                                                    *
     H*    W4150R02 - AGENT QUOTE STATUS INFORMATION (OPT 2/4/5)      *
     H*                                                               *
     H*  CREATED: 11/04/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/16/10  C:255 Support: 73460                                *
     H*                 Added file WMEML, Check flag EMFQLK for the   *
     H*                 record on which Option at the screen is being *
     H*                 taken.                                        *
     H*                                                               *
     H* 11/25/09  C:198 Project: 133WHW                               *
     H*                 Added option 25.                              *
     H*                                                               *
     H* 07/08/09  C:62  Project: 133WHN                               *
     H*                 Add a new quote tracking type 'A' - Duplicate *
     H*                                                               *
     H* 05/28/09  C:33  Project: 133WGV                               *
     H*                 Add a new quote tracking type 'I' - Waiting   *
     H*                                                               *
     H* 10/19/07 C:193 PROJECT: WC1561                                *
     H*                ADD A NEW QUOTE TRACKING TYPE 'S' CLOSED       *
     H*                                                               *
     H* 08/16/05 C:33  Seq#: WA114/48475                              *
     H*                Use new key structure when determining if a    *
     H*                 policy has been cancelled/reinstated.         *
     H*                                                               *
     H* 08/15/05 C:33  Seq#: WA045/48147                              *
     H*                Include 'T' status when determining if a policy*
     H*                 has been cancelled/reinstated.                *
     H*                                                               *
     H* 11/18/04 C:33  Project: 132WCB                                *
     H*                Add option 50                                  *
     H*                                                               *
     H* 04/06/04 C:61  SUPPORT 41930                                  *
     H*                Do not allow opt "33=Transfer" if called from  *
     H*                QUOTE.                                         *
     H*                                                               *
     H* 10/10/03 C:33  Project: WC1017                                *
     H*                Add options 52 & 53                            *
     H*                                                               *
     H* 03/28/02 C:90  SUPPORT: WA107/31830                           *
     H*                Include 'W' status when determining if a       *
     H*                policy has been cancelled/reinstated.          *
     H*                                                               *
     H* 03/14/02 C:61  SUPPORT WA122 31632                            *
     H*                Allow lower case text entry "Position to".     *
     H*                                                               *
     H* 10/17/00 C:33  PROJECT: WC577                                 *
     H*                DO NOT ALLOW OPTION 31, 36 OR 45 ('W', 'C' OR  *
     H*                'R') IF CALLED FROM THE QUOTE ENVIRONMENT.     *
     H*                                                               *
     H* 07/13/00 C:33  SEQ#: WA102/23358                              *
     H*                ALLOW FOR 'T' RECORDS TO BE TRANSFERED.        *
     H*                                                               *
     H* 03/17/00 C:61  PROJECT WC976                                  *
     H*              - Enable F6=Add.  Provide for entry of employer  *
     H*                without employer number.                       *
     H*              - Adjust scrolling.                              *
     H*                                                               *
     H*  03/03/98  C:33  PROJECT: WC395                               *
     H*                  ADD OPTIONS: 28=BOUND, 29=BOUND W/MONEY,     *
     H*                               33=TRANSFER                     *
     H*                                                               *
     H*  11/04/97  C:33  PROJECT: WC577                               *
     H*                  V3 REWRITE (NO COMMENTS)                     *
     H*****************************************************************
     FW4150F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWAQTP     IF   E             DISK
     F                                     RENAME(PQT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWAQTL8    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PQT01:DBRCDF)
     FWAQTL9    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PQT01:DBRCDF2)
     FWMAAP     IF   E             DISK
     F***  Begin Chg  ***  08/16/05  *************************************
     F**********WAQTL7  IF  E           K        DISK
     FWAQTL16   IF   E           K DISK
     F***  End   Chg  ***  08/16/05  *************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F***  Begin Add  ***  05/28/09  *************************************
     FWTQSL1    IF   E           K DISK
     F***  Begin Add  ***  05/28/09  *************************************
     F*** Beg Add *** 11/25/09 *******************************************
     FWTQSP     IF   E             DISK
     F                                     RENAME(PQS01:PQS02)
     F*** End Add *** 11/25/09 *******************************************
     F***  Begin Add  ***  08/16/10  *************************************
     FWMEML     IF   E           K DISK
     F***  Begin Add  ***  08/16/10  *************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings
     D***  BEGIN CHG  ***  03/03/98  *********************************
     D**********          OF      1  19  1   OFD    35   Options, Function
     D***  Begin Chg  ***  10/10/03  **************************************
     D**********          OF      1  22  1   OFD    35   Options, Function
     D***  Begin Chg  ***  11/18/04  **************************************
     D**********          OF      1  24  1   OFD    35   Options, Function
     D***  Begin Chg  ***  07/08/09  **************************************
     D**********          OF      1  30  1   OFD    35   Options, Function
     D*** Beg Chg *** 11/25/09 *******************************************
     D************        OF      1  31  1   OFD    35   Options, Function
     D OF              S              1    DIM(32) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(32) ALT(OF)
     D*** End Chg *** 11/25/09 *******************************************
     D***  End   Chg  ***  07/08/09  **************************************
     D***  End   Chg  ***  11/18/04  **************************************
     D***  End   Chg  ***  10/10/03  **************************************
     D***  END   CHG  ***  03/03/98  *********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Quote Status')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W4150R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(3)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(11)
     D*
     D*================================================================
     D*  Data structures
     D*================================================================
     D*
     D                 DS
     D  VIEW1                  1     41
     D  V1NAME                 1     25
     D  V1EMP#                27     35
     D  V1DIV                 37     41
     D                 DS
     D  VIEW2                  1     41
     D  V2EMP#                 1      9
     D  V2DIV                 11     15
     D  V2NAME                17     41
     D                 DS
     D  INFO1                  1     31
     D  I1TYPE                 2      2
     D  I1EFDT                 5     14
     D  I1PREM                17     28
     D                 DS
     D  INFO2                  1     31
     D  I2TYPE                 2      2
     D  I2WKDT                 5     14
     D  I2ORDT                17     26
     D                 DS
     D  INFO3                  1     31
     D  I3TYPE                 2      2
     D  I3NBDT                 5     14
     D  I3FXDT                17     26

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   PARM                    ##QTE             1
     C***  END   ADD  ***  03/03/98  *********************************
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   PARM      @@RRN         ##RRN2            9 0          WMAAP
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    AACO#
     C*
     C*  KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFND
     C*
     C*  KEY FOR AGENTS NAME FILE WMAGL
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C*
     C*  KEY LIST FOR THE QUOTE STATUS FILE
     C*
     C     QTKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C***  Begin Add  ***  05/28/09  *************************************
     C     QSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C***  Begin Add  ***  05/28/09  *************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    QTCO#         KKCO#
     C     *LIKE         DEFINE    QTFND         KKFND
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
     C     *LIKE         DEFINE    QTNAME        KKNAME
     C     *LIKE         DEFINE    QTEMP#        KKEMP#
     C     *LIKE         DEFINE    QTDIV         KKDIV
      ***** BEG ADD ******* 03/17/00 **********************************
     C     *LIKE         DEFINE    QTIDT         KKIDT
     C     *LIKE         DEFINE    QTITM         KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C     *LIKE         DEFINE    QTEFDT        XXFRDT
     C     *LIKE         DEFINE    QTEFDT        XXTODT
     C     *LIKE         DEFINE    QTTYPE        XXTYPE
     C     *LIKE         DEFINE    QTWKDT        XXCRDT
     C***  Begin Add  ***  05/28/09  *************************************
     C     *LIKE         DEFINE    QSKEY         XXKEY
     C***  End   Add  ***  05/28/09  *************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSTYPE        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C     SSDTSL        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSTYPE<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
      ***** BEG ADD ******* 03/17/00 **********************************
      *
      *  QUOTE system.
      *
     C     SCEMP#        IFEQ      *ZERO
      *
      *     Do not allow options that change type
      * zzz
     C     SCSEL         IFEQ      '21'
     C***  Beg Add *** 11/25/09 *******************************************
     C     SCSEL         OREQ      '25'
     C***  End Add *** 11/25/09 *******************************************
     C     SCSEL         OREQ      '26'
     C***  Begin Add  ***  05/28/09  *************************************
     C     SCSEL         OREQ      '27'
     C***  End   Add  ***  05/28/09  *************************************
     C     SCSEL         OREQ      '28'
     C     SCSEL         OREQ      '29'
     C***  Begin Add  ***  07/08/09  **************************************
     C     SCSEL         OREQ      '30'
     C***  End   Add  ***  07/08/09  **************************************
     C     SCSEL         OREQ      '31'
     C     SCSEL         OREQ      '33'
     C     SCSEL         OREQ      '36'
     C     SCSEL         OREQ      '41'
     C     SCSEL         OREQ      '46'
     C     SCSEL         OREQ      '51'
     C***  Begin Add  ***  11/18/04  **************************************
     C     SCSEL         OREQ      '50'
     C***  End   Add  ***  11/18/04  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     SCSEL         OREQ      '52'
     C     SCSEL         OREQ      '53'
     C***  Beg   Add  ***  10/19/07  **************************************
     C     SCSEL         OREQ      '54'
     C***  End   Add  ***  10/19/07  **************************************
     C***  End   Add  ***  10/10/03  **************************************
     C                   SETON                                        76  90
     C                   ENDIF
      *
     C                   ENDIF
     C***  Begin Add  ***  08/16/10  *************************************
     C  N90EMKY          CHAIN     PEM01                              60
     C     EMFQLK        IFEQ      'Y'
      *     Do not allow options that change type
     C  N90SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C                   SETON                                        90  50
     C                   ENDIF                                                  SCSEL
      *    "DISPLAY Only" message window
     C     SCSEL         IFEQ      '21'
     C     SCSEL         OREQ      '25'
     C     SCSEL         OREQ      '26'
     C     SCSEL         OREQ      '27'
     C     SCSEL         OREQ      '28'
     C     SCSEL         OREQ      '29'
     C     SCSEL         OREQ      '30'
     C     SCSEL         OREQ      '31'
     C     SCSEL         OREQ      '33'
     C     SCSEL         OREQ      '36'
     C     SCSEL         OREQ      '41'
     C     SCSEL         OREQ      '46'
     C     SCSEL         OREQ      '51'
     C     SCSEL         OREQ      '50'
     C     SCSEL         OREQ      '52'
     C     SCSEL         OREQ      '53'
     C     SCSEL         OREQ      '54'
     C                   EXFMT     WDW01
     C   KL              SETON                                        90
     C                   SETOFF                                       KL
     C                   ENDIF
      *
     C                   ENDIF
     C***  End   Add  ***  08/16/10  *************************************
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   SELECT
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     SCSEL         WHENEQ    '33'
     C*
     C     SCTYPE        IFNE      'W'
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'Q'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'M'
     C***  BEGIN ADD  ***  07/13/00  *********************************
     C     SCTYPE        ANDNE     'T'
     C***  END   ADD  ***  07/13/00  *********************************
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCTYPE<>W/P/Q/B/M
     C***  END   ADD  ***  03/03/98  *********************************
     C*_______________________________________________________________
     C*
     C     SCSEL         WHENEQ    '36'
     C     SCSEL         OREQ      '41'
     C*
     C*  CHECK FOR PRIOR CANCELLATION/REINSTATEMENT
     C*
     C*  READ ALL WAQTP RECORDS FOR THIS EMPLOYER, IF THE LAST
     C*  RECORDED ENTRY IS A 'C'/'R' (REGARDLESS OF THE AGENT NUMBER
     C*  ASSIGNED TO THE WAQTP RECORD) THEN THE CURRENT STATUS
     C*  IS CANCELLED/REINSTATED
     C*
     C                   MOVE      *BLANKS       XXTYPE
     C                   Z-ADD     *ZEROS        XXCRDT
     C*
     C     QTKY          CHAIN     PQT01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
      **** BEGIN ADD **** 03/28/02 ************************************
     C     QTTYPE        OREQ      'W'
      **** END   ADD **** 03/28/02 ************************************
     C***  Begin Add  ***  08/15/05  **************************************
     C     QTTYPE        OREQ      'T'
     C***  End   Add  ***  08/15/05  **************************************
     C*
     C     QTWKDT        IFGE      XXCRDT
     C                   MOVE      QTTYPE        XXTYPE
     C                   Z-ADD     QTWKDT        XXCRDT
     C                   ENDIF                                                  QTWKDT>=XXCRDT
     C*
     C                   ENDSL
     C*
     C     QTKY          READE     PQT01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C  N90XXTYPE        IFEQ      'C'
     C     SCSEL         ANDEQ     '36'
     C     XXTYPE        OREQ      'R'
     C     SCSEL         ANDEQ     '41'
     C                   SETON                                        71  90
     C                   ENDIF                                                  XXTYPE=C/SCSEL=36
     C*
     C  N90SCSEL         IFEQ      '41'
     C     XXTYPE        ANDNE     'C'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCSEL=41/XXTYPE<>C
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C  N90SSTYPE        IFNE      *BLANK
     C***  Begin Chg  ***  05/28/09  *************************************
     C********** SSTYPE    ANDNE'W'
     C********** SSTYPE    ANDNE'Q'
     C********** SSTYPE    ANDNE'L'
     C********** SSTYPE    ANDNE'C'
     C********** SSTYPE    ANDNE'P'
     C********** SSTYPE    ANDNE'D'
     C********** SSTYPE    ANDNE'R'
     C*****BEGIN ADD  ***  03/03/98  *********************************
     C********** SSTYPE    ANDNE'B'
     C********** SSTYPE    ANDNE'M'
     C*****END** ADD  ***  06/03/98  *********************************
     C*****Begin Add  ***  10/10/03  **************************************
     C********** SSTYPE    ANDNE'F'
     C********** SSTYPE    ANDNE'G'
     C*****End** Add  ***  10/10/03  **************************************
     C*****Begin Add  ***  11/18/04  **************************************
     C********** SSTYPE    ANDNE'U'
     C*****End** Add  ***  11/18/04  **************************************
     C*****Begin Add  ***  10/19/07  **************************************
     C********** SSTYPE    ANDNE'S'
     C*****End** Add  ***  10/19/07  **************************************
     C                   MOVEL     SSTYPE        XXKEY
     C     QSKY          CHAIN     PQS01                              60
     C     *IN60         IFEQ      *ON
     C***  End   Chg  ***  05/28/09  *************************************
     C                   SETON                                        71  90
     C***  Begin Add  ***  05/28/09  *************************************
     C                   ENDIF
     C***  End   Add  ***  05/28/09  *************************************
     C                   ENDIF                                                  SSTYPE<>*BLANK
     C*
     C  N90SSFRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSFRDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ELSE
     C*
     C                   CLEAR                   XXFRDT
     C*
     C                   ENDIF                                                  SSFRDT<>*ZERO
     C*
     C  N90SSTODT        IFNE      *ZERO
     C*
     C                   Z-ADD     SSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSTODT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SSFRDT<>*ZERO
     C*
     C  N90SSTODT        IFNE      *ZERO
     C     SSDTSL        ANDEQ     *BLANK
     C     SSDTSL        ORNE      *BLANK
     C     SSTODT        ANDEQ     *ZERO
     C                   SETON                                        74  90
     C                   ENDIF                                                  SSTODT<>*ZERO
     C*
     C  N90SSFRDT        IFNE      *ZERO
     C*
     C     SSTODT        IFEQ      *ZERO
     C     SSDTSL        OREQ      *BLANK
     C                   SETON                                        74  90
     C                   ENDIF                                                  SSTODT=*ZERO
     C*
     C                   ENDIF                                                  SSFRDT<>*ZERO
     C*
     C  N90XXFRDT        IFGT      XXTODT
     C                   SETON                                        75  90
     C                   ENDIF                                                  XXFRDT>XXTODT
     C*
     C*** Beg Add *** 11/25/09 ****************************************
     C                   EXSR      OTHFKS
     C*
     C*** End Add *** 11/25/09 ****************************************
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSTYPE
     C                   CLEAR                   SSSTAT
     C                   CLEAR                   SSFRDT
     C                   CLEAR                   SSTODT
     C                   CLEAR                   SSDTSL
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     AACO#         KKCO#
     C                   Z-ADD     AAFND         KKFND
     C                   Z-ADD     AAAGT#        KKAGT#
     C                   CLEAR                   KKNAME
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKNAME
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPST2        KKEMP#
     C                   MOVE      SCPSTA        KKDIV
     C                   ENDSL
     C*
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCNAME        KKNAME
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   Z-ADD     SCIDT         KKIDT
     C                   Z-ADD     SCITM         KKITM
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   SELECT
     C*
     C     VIEW#         WHENEQ    1
     C*
     C                   CLEAR                   VIEW1
     C*
     C                   MOVEL(P)  QTNAME        V1NAME
     C*
     C                   MOVEL(P)  QTEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1EMP#
     C*
     C                   MOVEL(P)  QTDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1DIV
     C*
     C                   MOVEL(P)  VIEW1         SCDATA
     C*
     C**
     C     VIEW#         WHENEQ    2
     C*
     C                   CLEAR                   VIEW2
     C*
     C                   MOVEL(P)  QTNAME        V2NAME
     C*
     C                   MOVEL(P)  QTEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V2EMP#
     C*
     C                   MOVEL(P)  QTDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V2DIV
     C*
     C                   MOVEL(P)  VIEW2         SCDATA
     C*
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C                   SELECT
     C*
     C     INFO#         WHENEQ    1
     C*
     C                   CLEAR                   INFO1
     C*
     C                   MOVE      QTTYPE        I1TYPE
     C*
     C                   Z-ADD     QTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   MOVEL(P)  XXEFDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1EFDT
     C*
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   MOVEL(P)  QTPREM        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1PREM
     C*
     C                   MOVEL(P)  INFO1         SCDAT2
     C*
     C**
     C     INFO#         WHENEQ    2
     C*
     C                   CLEAR                   INFO2
     C*
     C                   MOVE      QTTYPE        I2TYPE
     C*
     C                   Z-ADD     QTWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWKDT            8 0
     C*
     C                   Z-ADD     QTORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXORDT            8 0
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C*
     C                   MOVEL(P)  XXWKDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I2WKDT
     C*
     C                   MOVEL(P)  XXORDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I2ORDT
     C*
     C                   MOVEL(P)  INFO2         SCDAT2
     C*
     C**
     C     INFO#         WHENEQ    3
     C*
     C                   CLEAR                   INFO3
     C*
     C                   MOVE      QTTYPE        I3TYPE
     C*
     C                   Z-ADD     QTNBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNBDT            8 0
     C*
     C                   Z-ADD     QTFXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFXDT            8 0
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C*
     C                   MOVEL(P)  XXNBDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I3NBDT
     C*
     C                   MOVEL(P)  XXFXDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I3FXDT
     C*
     C                   MOVEL(P)  INFO3         SCDAT2
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  QTNAME        SCNAME
     C                   Z-ADD     QTEMP#        SCEMP#
     C                   Z-ADD     QTDIV         SCDIV
     C                   Z-ADD     QTTIME        SCTIME
     C                   Z-ADD     QTDT          SCDT
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   Z-ADD     QTIDT         SCIDT
     C                   Z-ADD     QTITM         SCITM
      ***** END ADD ******* 03/17/00 **********************************
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C                   MOVE      QTTYPE        SCTYPE
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OPT21 - SUBROUTINE FOR OPTION 21
     C*================================================================
     C*
     C* OPTION 21=QUOTED
     C*
     C     OPT21         BEGSR
     C*
     C                   MOVEL     'Q'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT21
     C*
     C*================================================================
     C* OPT26 - SUBROUTINE FOR OPTION 26
     C*================================================================
     C*
     C* OPTION 26=PENDED
     C*
     C     OPT26         BEGSR
     C*
     C                   MOVEL     'N'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT26
     C*
     C***  Begin Add  ***  05/28/09  *************************************
     C*================================================================
     C* OPT27 - SUBROUTINE FOR OPTION 27
     C*================================================================
     C*
     C* OPTION 27=Waiting
     C*
     C     OPT27         BEGSR
     C*
     C                   MOVEL     'i'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT27
     C***  End   Add  ***  05/28/09  *************************************
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C*================================================================
     C* OPT28 - SUBROUTINE FOR OPTION 28
     C*================================================================
     C*
     C* OPTION 28=BOUND
     C*
     C     OPT28         BEGSR
     C*
     C                   MOVEL     'B'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT28
     C*
     C*================================================================
     C* OPT29 - SUBROUTINE FOR OPTION 29
     C*================================================================
     C*
     C* OPTION 29=BOUND W/MONEY
     C*
     C     OPT29         BEGSR
     C*
     C                   MOVEL     'M'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT29
     C***  END   ADD  ***  03/03/98  *********************************
     C***  Begin Add  ***  07/08/09  **************************************
     C*================================================================
     C* OPT30 - SUBROUTINE FOR OPTION 30
     C*================================================================
     C*
     C* OPTION 30=Duplicate Submissions
     C*
     C     OPT30         BEGSR
     C*
     C                   MOVEL     'a'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT30
     C***  End   Add  ***  07/08/09  **************************************
     C*
     C*================================================================
     C* OPT31 - SUBROUTINE FOR OPTION 31
     C*================================================================
     C*
     C* OPTION 31=WRITTEN
     C*
     C     OPT31         BEGSR
     C*
     C                   MOVEL     'W'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT31
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C*================================================================
     C* OPT33 - SUBROUTINE FOR OPTION 33
     C*================================================================
     C*
     C* OPTION 33=TRANSFER
     C*
     C     OPT33         BEGSR
     C*
     C                   MOVEL     'T'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C***  BEGIN ADD  ***  08/25/98  *********************************
     C                   MOVE      *ON           AUTORF
     C***  END   ADD  ***  08/25/98  *********************************
     C*
     C                   ENDSR                                                  OPT33
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C*================================================================
     C* OPT36 - SUBROUTINE FOR OPTION 36
     C*================================================================
     C*
     C* OPTION 36=CANCELLED
     C*
     C     OPT36         BEGSR
     C*
     C                   MOVEL     'X'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT36
     C*
     C*================================================================
     C* OPT41 - SUBROUTINE FOR OPTION 41
     C*================================================================
     C*
     C* OPTION 41=REINSTATED
     C*
     C     OPT41         BEGSR
     C*
     C                   MOVEL     'R'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT41
     C*
     C*================================================================
     C* OPT46 - SUBROUTINE FOR OPTION 46
     C*================================================================
     C*
     C* OPTION 46=LOST
     C*
     C     OPT46         BEGSR
     C*
     C                   MOVEL     'L'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT46
     C***  Begin Add  ***  11/18/04  **************************************
     C*
     C*====================================================================
     C* OPT50 - SUBROUTINE FOR OPTION 50
     C*====================================================================
     C*
     C* OPTION 50=Submitted
     C*
     C     OPT50         BEGSR
     C*
     C                   MOVEL     'U'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT50
     C***  End   Add  ***  11/18/04  **************************************
     C*
     C*================================================================
     C* OPT51 - SUBROUTINE FOR OPTION 51
     C*================================================================
     C*
     C* OPTION 51=DECLINED
     C*
     C     OPT51         BEGSR
     C*
     C                   MOVEL     'Y'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT51
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*====================================================================
     C* OPT52 - SUBROUTINE FOR OPTION 52
     C*====================================================================
     C*
     C* OPTION 52=Renewal Pending
     C*
     C     OPT52         BEGSR
     C*
     C                   MOVEL     'F'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT52
     C*
     C*====================================================================
     C* OPT53 - SUBROUTINE FOR OPTION 53
     C*====================================================================
     C*
     C* OPTION 53=Renewal Quote
     C*
     C     OPT53         BEGSR
     C*
     C                   MOVEL     'G'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT53
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C***  Beg   Add  ***  10/19/07  **************************************
     C*====================================================================
     C* OPT54 - SUBROUTINE FOR OPTION 54
     C*====================================================================
     C*
     C* OPTION 54=Closed
     C*
     C     OPT54         BEGSR
     C*
     C                   MOVEL     'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR                                                  OPT54
     C***  End   Add  ***  10/19/07  **************************************
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*** Beg Add *** 11/25/09 ****************************************
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*** End Add *** 11/25/09 ****************************************
     C                   ENDSR                                                  OTHFKS
     C*
     C*** Beg Add *** 11/25/09 ****************************************
     C*================================================================
     C* OPT25  - Process option 25
     C*================================================================
     C*
     C     OPT25         BEGSR
     C*
     C                   MOVEL     'V'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT25
     C*
     C*================================================================
     C* KJDO   - Process F10 function key
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSTYPE'
     C*
     C                   CALL      'W2150R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PQS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     QSKEY         SSTYPE
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C                   MOVE      *ON           *IN77
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  ##RRN = *ZEROS
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*** End Add *** 11/25/09 ****************************************
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '21'          OPT21
     C*** Beg Add *** 11/25/09 ****************************************
     C     SCSEL         CASEQ     '25'          OPT25
     C*** End Add *** 11/25/09 ****************************************
     C     SCSEL         CASEQ     '26'          OPT26
     C***  Begin Add  ***  05/28/09  *************************************
     C     SCSEL         CASEQ     '27'          OPT27
     C***  End   Add  ***  05/28/09  *************************************
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     SCSEL         CASEQ     '28'          OPT28
     C     SCSEL         CASEQ     '29'          OPT29
     C***  END   ADD  ***  03/03/98  *********************************
     C***  Begin Add  ***  07/08/09  **************************************
     C     SCSEL         CASEQ     '30'          OPT30
     C***  End   Add  ***  07/08/09  **************************************
     C     SCSEL         CASEQ     '31'          OPT31
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     SCSEL         CASEQ     '33'          OPT33
     C***  END   ADD  ***  03/03/98  *********************************
     C     SCSEL         CASEQ     '36'          OPT36
     C     SCSEL         CASEQ     '41'          OPT41
     C     SCSEL         CASEQ     '46'          OPT46
     C     SCSEL         CASEQ     '51'          OPT51
     C***  Begin Add  ***  11/18/04  **************************************
     C     SCSEL         CASEQ     '50'          OPT50
     C***  End   Add  ***  11/18/04  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     SCSEL         CASEQ     '52'          OPT52
     C     SCSEL         CASEQ     '53'          OPT53
     C***  End   Add  ***  10/10/03  **************************************
     C***  Begin Add  ***  10/19/07  **************************************
     C     SCSEL         CASEQ     '54'          OPT54
     C***  End   Add  ***  10/19/07  **************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     Q1PRMS        IFGT      4
     C                   MOVE      @@QTE         ##QTE
     C                   ENDIF                                                  Q1PRMS>4
     C***  END   ADD  ***  03/03/98  *********************************
      ***** BEG ADD ******* 03/17/00 **********************************
      *
      *  QUOTE system.
      *
     C***  BEGIN CHG  ***  10/17/00  *********************************
     C********** @@QTE     IFEQ 'Q'
     C     ##QTE         IFEQ      'Q'
     C***  END   CHG  ***  10/17/00  *********************************
      *
      *  Enable F6=Add when called from QUOTE system.
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C***  BEGIN ADD  ***  10/17/00  *********************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '31'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      ***** BEG ADD ******* 04/06/04 **********************************
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '33'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      ***** END ADD ******* 04/06/04 **********************************
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '36'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '41'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C***  END   ADD  ***  10/17/00  *********************************
      *
     C                   ENDIF
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C*  GET THE AGENT HISTORY RECORD
     C*
     C     @@RRN         CHAIN     WMAAP                              60
     C*
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   Z-ADD     AACO#         KKCO#
     C                   Z-ADD     AAFND         KKFND
     C                   Z-ADD     AAAGT#        KKAGT#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNNM
     C*
     C                   Z-ADD     AACMMT        SCCMMT
     C*
     C                   Z-ADD     AACMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCMDT
     C*
     C                   MOVEL     *BLANKS       SCANAM
     C     AGKY          CHAIN     PAG01                              60
     C  N60              MOVEL(P)  AGNAME        SCANAM
     C*
     C                   MOVE      *ON           *IN31
     C*
     C     SCCMMT        IFEQ      *ZERO
     C     SCCMDT        ANDEQ     *ZERO
     C                   MOVE      *OFF          *IN31
     C                   ENDIF                                                  SCCMMT=*ZERO=SCCMDT
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C                   MOVEL     *BLANKS       SCDAT2
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSTYPE        IFNE      *BLANK
     C     SSTYPE        ANDNE     QTTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSTYPE<>*BLANK
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     SSTYPE        IFEQ      'W'
     C     QTTYPE        ANDEQ     'T'
     C                   MOVE      *ON           MEETSS
     C                   ENDIF                                                  SSTYPE=W/QTTYPE=T
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C***  Begin Add  ***  05/28/09  *************************************
     C* Check Quote Status File to determine if code is used as another
     C     SSTYPE        IFNE      *BLANK
     C                   MOVEL     QTTYPE        XXKEY
     C                   CLEAR                   QSUSED
     C     QSKY          CHAIN     PQS01                              60
     C     SSTYPE        IFEQ      QSUSED
     C                   MOVE      *ON           MEETSS
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  05/28/09  *************************************
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     QTSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT<>*BLANK
     C*
     C                   SELECT
     C*
     C     SSDTSL        WHENEQ    'E'
     C     XXFRDT        IFGT      QTEFDT
     C     XXTODT        ORLT      QTEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  XXFRDT>QTEFDT>XXTODT
     C*
     C**
     C     SSDTSL        WHENEQ    'F'
     C     XXFRDT        IFGT      QTFXDT
     C     XXTODT        ORLT      QTFXDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  XXFRDT>QTFXDT>XXTODT
     C*
     C**
     C     SSDTSL        WHENEQ    'N'
     C     XXFRDT        IFGT      QTNBDT
     C     XXTODT        ORLT      QTNBDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  XXFRDT>QTNBDT>XXTODT
     C*
     C**
     C     SSDTSL        WHENEQ    'O'
     C     XXFRDT        IFGT      QTORDT
     C     XXTODT        ORLT      QTORDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  XXFRDT>QTORDT>XXTODT
     C*
     C**
     C     SSDTSL        WHENEQ    'T'
     C     XXFRDT        IFGT      QTWKDT
     C     XXTODT        ORLT      QTWKDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  XXFRDT>QTWKDT>XXTODT
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt  Name                       Employer   Div Tp    Eff Date      Premium
Opt   Employer   Div Name                      Tp    Eff Date      Premium
Opt  Name                       Employer   Div Tp   Chg Tp Dt  Org Sbm Dt
Opt   Employer   Div Name                      Tp   Chg Tp Dt  Org Sbm Dt
Opt  Name                       Employer   Div Tp    New Busi   Fax Date
Opt   Employer   Div Name                      Tp    New Busi   Fax Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12121=Quoted                YNN
12525=Underwriter Review    YNN
12626=Pended                YNN
12727=Waiting               YNN                               05/28/09
12828=Bound                 YNN
12929=Bound w/money         YNN
13030=Duplicate Submissions YNN
13131=Written               YNN
13333=Transfer              YNN
13636=Cancelled             YNN
14141=Reinstated            YNN
14646=Lost                  YNN
15050=Submitted             YNN                             11/18/04
15151=Declined              YNN
15252=Renewal pending       YNN                             10/10/03
15353=Renewal quote         YNN                             10/10/03
15454=Closed                YNN                             10/19/07
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change Info           YYY
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
2*** Future use             NNN
2*** Future use             NNN
2*** Future use             NNN
3
