     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/20/13 C:287 Project: CI126                                *
     H*                 Add division to CIEMPCHG program call         *
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H* 04/28/09 C:189 SUPPORT: 67599                                 *
     H*                DIARY SHOULD GO TO THE ACTUAL USER ASSIGNED    *
     H*                IN ADMIN/DART CONTROL FILE AND NOT TO USER     *
     H*                WHO IS SUBMITTIG THE JOB                       *
     H*                                                               *
     H* 09/03/08 C:136 SUPPORT: WA132 64919                           *
     H*                DO NOT CHANGE LOCATION NAMES WHEN DBA AND      *
     H*                FED NAME IN EMP MASTER IS CHANGED.             *
     H*                                                               *
     H* 05/28/08  C:33  Project: 132WJP                               *
     H*                 Make sure that *IN90 is set correctly.        *
     H*                                                               *
     H* 05/23/08 C:33  Project: 132DKH                                *
     H*                For name and/or FEIN changes report them to    *
     H*                 CiCenter.                                     *
     H*                                                               *
     H* 11/23/07  C:189 Project 132WJP
     H*                Produce an Auto Diary on Employer name change  *
     H*                                                               *
     H* 04/25/06 C:65  PROJECT: 132WAY                                *
     H*                ADD NEW FIELDS TO DESIGNATE EMPLOYER NAMES AS  *
     H*                PERSONAL NAMES.  FLAGGED FOR CLIENTS 132 AND   *
     H*                136 AS THIS IS A CA REQUIREMENT.               *
     H*                                                               *
     H* 09/22/04 C:33  Project: WC1202                                *
     H*                Add processing to expand employer name fields  *
     H*                 to two 50 character fields each.              *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Remove Employer Contacts from Prompt screen.   *
     H*                                                               *
     H* 03/10/00 C:118 SUPPORT: WA048/21972                           *
     H*                Update ALTPOB when employer type of business   *
     H*                is changed.                                    *
     H*                                                               *
     H* 07/28/99 C:113 PROJECT: SUPPORT(WA091/SEQ#18847)              *
     H*                Corrected to update Quote Tracking file.       *
     H*                                                               *
     H* 05/05/99 C:09 PROJECT: SUPPORT (WA048/SEQ# 17497)             *
     H*               CORRECT PROGRAM TO LOAD THE NEW EMPLOYER NAME   *
     H*               PROPERLY (WAS LOADING FUND NAME).               *
     H*                                                               *
     H* 04/28/99 C:09 PROJECT: SUPPORT (WA048/SEQ# 17497)             *
     H*                                (WA081/SEQ# 17874)             *
     H*               THERE IS NO NEED TO PRINT CONTACT NAMES ON      *
     H*               "NAME CHANGE" ENDORSEMENT.                      *
     H*                                                               *
     H* 04/15/99 C:09 PROJECT: CM027                                  *
     H*               MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE.      *
     H*               DO NOT ALLOW OWNER/OFFICER CHANGES ON A NAME    *
     H*               CHANGE END, OWN/OFF END MUST BE DONE SEPARATE.  *
     H*                                                               *
     H* 03/29/99 C:09 PROJECT: SUPPORT (WA091/SEQ# 17292)             *
     H*               MODIFY PROGRAM TO ENSURE THE "CHANGED" ELEMENTS *
     H*               ARE LOADED INTO THE EN**2 FIELDS IN WMENP.      *
     H*                                                               *
     H* 02/02/99 C:09 PROJECT: SUPPORT (WA096/SEQ# 16359)             *
     H*               ON NAME CHANGE ENDORSEMENT, ANY LOCATIONS       *
     H*               (WMALP/ALELTP = "L") SHOULD ALSO HAVE THE NAME  *
     H*               CHANGED TO THAT OF THE INSURED.                 *
     H*                                                               *
     H* 02/01/99 C:09 PROJECT: CM027                                  *
     H*               ADDED CARRIER POLICY #. POLICY FROM DATE AND    *
     H*               POLICY TO DATE (IN APPROPRIATE PRINT FORMAT)    *
     H*               TO DOL (WCPOLSV3/ENDORSE) ENDORSEMENT.          *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*****************************************************************
     FW0051F11  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF   E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F***  Begin Add  ***  05/23/08  **************************************
     F                                     INFDS(DBFDS)
     F***  End   Add  ***  05/23/08  **************************************
     FWDELP     IF   E             DISK
     FWTMYP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 02/02/99 *********************************************
     FWMALL     UF   E           K DISK
      **** END   ADD **** 02/02/99 *********************************************
      *** BEGIN ADD *** 07/28/99 ***********************************************
     FWAQTL12   UF   E           K DISK
      *** END   ADD *** 07/28/99 ***********************************************
      **** BEG ADD ****** 11/23/07 *********************************************
     FSTCTL1    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
      **** END ADD ****** 11/23/07 *********************************************
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D***  Begin Add  ***  05/23/08  **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***  End   Add  ***  05/23/08  **************************************
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D                 DS
     D  XXFEIN                 1      9
     D  XXFEI2                 1      2
     D  XXFEI7                 3      9
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Endorsement-
     D                                     s - Name Changes')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D DOTS            C                   CONST(' . . . . . . . . . -
     D                                     . . . . . . . . .')
     D CNT1            C                   CONST('Contact')
     D CNT2            C                   CONST('Attention')
     D CNT3            C                   CONST('Contact 3')
     D #OFSCR          C                   CONST(1)
      ***** BEG ADD ******* 11/23/07 **********************************
     D                 DS
     D  QCKEY1                 1     40
     D  EMCO#                  1      3  0
     D  EMFND                  4      6  0
     D  EMEMP#                 7     15  0
     D  EMDIV                 16     20  0
     D  QCQTE1                40     40
      ***** END ADD ******* 11/23/07 **********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@EEDT            8 0          MMDDCCYY
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Owner/Officer Work With parameter list
     C*
     C     OOPL          PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      *BLANK        ##ELFL            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  COMPANY MASTER
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*  ADMIN/DARTS CONTROL DEFINITION
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** END   ADD **** 10/14/98 *********************************************
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      **** BEGIN ADD **** 02/02/99 *********************************************
     C*
     C*  ENTITY/LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
      **** END ADD ****** 02/02/99 *********************************************
     C*
      *** BEGIN ADD *** 07/28/99 ***********************************************
     C*
     C*  Agent Quote Tracking file
     C*
     C     QTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
      *** END   ADD *** 07/28/99 ***********************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ELCO#         ##CO#
     C     *LIKE         DEFINE    ELFND         ##FND
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    ELFYR         ##FYR
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    EMATTN        SVATTN
     C     *LIKE         DEFINE    EMCNT3        SVCNT3
     C     *LIKE         DEFINE    EMCONT        SVCONT
     C***  Begin Chg  ***  09/22/04  **************************************
     C********** *LIKE     DEFN EMNAME    SVENAM
     C********** *LIKE     DEFN EMFDNM    SVFDNM
     C     *LIKE         DEFINE    SCNAM1        SVNAM1
     C     *LIKE         DEFINE    SCNAM2        SVNAM2
     C     *LIKE         DEFINE    SCFDN1        SVFDN1
     C     *LIKE         DEFINE    SCFDN2        SVFDN2
     C***  End   Chg  ***  09/22/04  **************************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C     *LIKE         DEFINE    EMNAMP        SVNAMP
     C     *LIKE         DEFINE    EMFDNP        SVFDNP
     C*** END ADD *****  04/25/06  ************************************
     C     *LIKE         DEFINE    EMFEIN        SVFEIN
     C     *LIKE         DEFINE    EMTPOB        SVTPOB
     C     *LIKE         DEFINE    SCTPBD        SVTPBD
     C*** BEG ADD *****  11/23/07  ************************************
     C     *LIKE         DEFINE    EXCNDR        XXCNDR
     C     *LIKE         DEFINE    EXCNST        XXCNST
     C     *LIKE         DEFINE    EMNAME        HNAME
     C     *LIKE         DEFINE    EMNAM2        HNAM2
     C     *LIKE         DEFINE    EMFDNM        HFDNM
     C     *LIKE         DEFINE    EMFDN2        HFDN2
     C*** END ADD *****  11/23/07  ************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C*====================================================================
     C* Update CiCenter with changes
     C*====================================================================
     C     CICNTR        BEGSR
     C*
     C     *LIKE         DEFINE    EMFEIN        SVFID
     C     *LIKE         DEFINE    SVFID         ##OFID
     C     *LIKE         DEFINE    SVDBA         ##ODBA
     C     *LIKE         DEFINE    XX100         SVDBA
     C*** Begin Add ** 08/20/13 ****************************
     C     *LIKE         DEFINE    EMDIV         SVDIV
     C     *LIKE         DEFINE    SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   CLEAR                   #2TYPE
     C     EMNAME        CAT(P)    EMNAM2        XX100           100
     C*
     C                   SELECT
     C     SVDBA         WHENNE    XX100
     C                   MOVEL     'DBA'         #2TYPE
     C     SVFID         WHENNE    EMFEIN
     C                   MOVEL     'FEIN'        #2TYPE
     C                   ENDSL
     C*
     C     #2TYPE        IFNE      *BLANK
     C                   CALL      'CIEMPCHG'                           99
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    #2TYPE           10
     C                   PARM      SVFID         ##OFID
     C                   PARM      SVDBA         ##ODBA
     C*** Begin Add ** 08/20/13 ****************************
     C                   PARM      SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C                   ENDIF
     C*
     C     EMNAME        CAT(P)    EMNAM2        SVDBA
     C                   MOVE      EMFEIN        SVFID
     C*
     C                   ENDSR
     C***  End   Add  ***  05/23/08  **************************************
     C*
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
     C* DBA CANNOT BE BLANK
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C***  Begin Chg  ***  09/22/04  **************************************
     C********** SCENAM    ANDEQ*BLANK
     C     SCNAM1        ANDEQ     *BLANK
     C***  End   Chg  ***  09/22/04  **************************************
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCENAM
      ***** BEGIN ADD ***** 04/25/06 **********************************
      *
      * PERSONAL NAME FIELDS MUST BE VALID IF ENTERED.
      *
     C     *IN17         IFEQ      *ON
      *
     C  N90SCNAMP        IFNE      *BLANK
     C     SCNAMP        ANDNE     'Y'
     C                   SETON                                        18  90
     C                   ENDIF                                                  SCNAMP IFNE
      *
     C  N90SCFDNP        IFNE      *BLANK
     C     SCFDNP        ANDNE     'Y'
     C                   SETON                                        19  90
     C                   ENDIF                                                  SCNAMP IFNE
      *
     C                   ENDIF                                                  *IN17 IFEQ
      ***** END ADD ******* 04/25/06 **********************************
     C*
     C*  EDIT THE FEDERAL ID NUMBER
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     EXFIFG        ANDEQ     'Y'
     C     SCFEIN        ANDEQ     *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
     C*
     C*  VALIDATE TYPE OF BUSINESS
     C*
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
     C*
     C                   MOVEL     *BLANKS       SCTPBD
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        73  90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
      **** BEGIN DEL **** 04/15/99 *********************************************
     C************
     C**VALIDATE*THE OWNER/OFFICER CHANGE FLAG
     C************
     C************IN90     IFEQ *OFF                       NO ERRORS SO FAR
     C***********SCOOCG    ANDNE'N'
     C***********SCOOCG    ANDNE'Y'
     C***********          SETON                     74  90
     C***********
     C***********          ENDIF                           *IN90 = *OFF
      **** END DEL ****** 04/15/99 *********************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EMDT          IFGT      SAVEDT
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
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
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10 (SEARCHES)
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C*  TYPE OF BUSINESS
     C*
     C     HFLD          IFEQ      'SCTPOB'
     C*
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C                   MOVE      *ON           *IN31
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL(P)  XXKEY         SCTPOB
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  XXKEY<>*BLANKS
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELEMNAME    SCENAM
     C**********           MOVELEMNAME    SVENAM
     C**********
     C**********           MOVELEMFDNM    SCFDNM
     C**********           MOVELEMFDNM    SVFDNM
     C                   MOVEL     EMNAME        SCNAM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        SCNAM1
     C                   MOVE      EMNAM2        SCNAM2
     C*
     C                   MOVEL     SCNAM1        SVNAM1
     C                   MOVEL     SCNAM2        SVNAM2
     C*
     C                   MOVEL     EMFDNM        SCFDN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        SCFDN1
     C                   MOVE      EMFDN2        SCFDN2
     C*
     C                   MOVEL     SCFDN1        SVFDN1
     C                   MOVEL     SCFDN2        SVFDN2
     C***  End   Chg  ***  09/22/04  **************************************
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      EMNAMP        SCNAMP
     C                   MOVE      EMFDNP        SCFDNP
     C                   MOVE      EMNAMP        SVNAMP
     C                   MOVE      EMFDNP        SVFDNP
      ***** END ADD ******* 04/25/06 **********************************
      ***** BEG ADD ******* 11/23/07 **********************************
     C                   MOVE      EMNAME        HNAME
     C                   MOVE      EMNAM2        HNAM2
     C                   MOVE      EMFDNM        HFDNM
     C                   MOVE      EMFDN2        HFDN2
      ***** END ADD ******* 11/23/07 **********************************
     C*
     C                   MOVE      EMFEIN        SCFEIN
     C                   MOVE      EMFEIN        SVFEIN
     C*
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELEMCONT    SCCONT
     C*********            MOVELEMCONT    SVCONT
     C*********
     C*********            MOVELEMATTN    SCATTN
     C*********            MOVELEMATTN    SVATTN
     C*********
     C*********            MOVELEMCNT3    SCCNT3
     C*********            MOVELEMCNT3    SVCNT3
      ***** END DEL ******* 01/31/02 **********************************
     C*
     C                   MOVEL     EMTPOB        SCTPOB
     C                   MOVEL     EMTPOB        SVTPOB
     C*
      **** BEGIN DEL **** 04/15/99 *********************************************
     C*********************MOVEL'N'       SCOOCG
      **** END DEL ****** 04/15/99 *********************************************
     C*
     C*  TYPE OF BUSINESS DESCRIPTION
     C*
     C                   CLEAR                   SCTPBD
     C                   MOVEL     'BUSI'        XXTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCTPBD
     C                   MOVEL     GTDESC        SVTPBD
     C                   ENDIF
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C     EMNAME        CAT(P)    EMNAM2        SVDBA
     C                   MOVE      EMFEIN        SVFID
     C***  End   Add  ***  05/23/08  **************************************
     C*** Begin Add ** 08/20/13 ****************************
     C                   MOVE      EMDIV         SVDIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C***  Begin Chg  ***  09/22/04  **************************************
     C********** SCENAM    IFNE SVENAM
     C********** SCFDNM    ORNE SVFDNM
     C**********           MOVELSCENAM    EMNAME
     C****BEGIN*ADD *** 07/28/99 ***********************************************
     C**********           MOVELSCENAM    QTNAME
     C****END***ADD *** 07/28/99 ***********************************************
     C**********           MOVELSCENAM    XXAF01 50 P
     C**********           MOVELSVENAM    XXB401 50 P
     C**********           Z-ADD1         XXRSN   30
     C**********           EXSR WRTPEN
     C**********
     C**********           MOVELSCFDNM    EMFDNM
     C**********           MOVELSCFDNM    XXAF01    P
     C**********           MOVELSVFDNM    XXB401    P
     C**********           Z-ADD2         XXRSN
     C**********           EXSR WRTPEN
     C**********           ENDIF
     C     SCNAM1        IFNE      SVNAM1
     C     SCNAM2        ORNE      SVNAM2
     C     SCFDN1        ORNE      SVFDN1
     C     SCFDN2        ORNE      SVFDN2
     C*** BEGIN ADD ***  04/25/06  ************************************
     C     SCNAMP        ORNE      SVNAMP
     C     SCFDNP        ORNE      SVFDNP
     C*** END ADD *****  04/25/06  ************************************
     C*
     C                   MOVEL     SCNAM1        EMNAME
     C                   MOVE(P)   SCNAM1        XXNM10           10
     C                   MOVEL     XXNM10        EMNAM2
     C                   MOVE      SCNAM2        EMNAM2
     C*** BEGIN ADD ***  04/25/06  ************************************
     C                   MOVE      SCNAMP        EMNAMP
     C*** END ADD *****  04/25/06  ************************************
     C                   MOVEL     SCNAM1        QTNAME
     C                   MOVEL(P)  SCNAM1        XXAF01           50
     C                   MOVEL(P)  SVNAM1        XXB401           50
     C                   Z-ADD     1             XXRSN             3 0
     C                   EXSR      WRTPEN
     C*
     C                   MOVEL     SCFDN1        EMFDNM
     C                   MOVE(P)   SCFDN1        XXNM10           10
     C                   MOVEL     XXNM10        EMFDN2
     C                   MOVE      SCFDN2        EMFDN2
     C*** BEGIN ADD ***  04/25/06  ************************************
     C                   MOVE      SCFDNP        EMFDNP
     C*** END ADD *****  04/25/06  ************************************
     C                   MOVEL(P)  SCFDN1        XXAF01
     C                   MOVEL(P)  SVFDN1        XXB401
     C                   Z-ADD     2             XXRSN
     C                   EXSR      WRTPEN
     C*
     C                   ENDIF
     C***  End   Chg  ***  09/22/04  **************************************
     C*
     C     SCFEIN        IFNE      SVFEIN
     C                   MOVE      SCFEIN        EMFEIN
     C                   MOVE      SCFEIN        XXFEIN
     C     XXFEI2        CAT(P)    '-':0         XXAF01
     C                   CAT       XXFEI7:0      XXAF01
     C                   MOVE      SVFEIN        XXFEIN
     C     XXFEI2        CAT(P)    '-':0         XXB401
     C                   CAT       XXFEI7:0      XXB401
     C                   Z-ADD     3             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
      ***** BEG DEL ******* 01/31/02 **********************************
      *****BEGIN CHG **** 04/28/99 *********************************************
     C*********  SCCONT    IFNE SVCONT
     C*********            MOVELSCCONT    EMCONT
     C*********************MOVELSCCONT    XXAF01    P
     C*********************MOVELSVCONT    XXB401    P
     C*********************Z-ADD4         XXRSN
     C*********************EXSR WRTPEN
     C*********            ENDIF
     C*********************
     C*********  SCATTN    IFNE SVATTN
     C*********            MOVELSCATTN    EMATTN
     C*********************MOVELSCATTN    XXAF01    P
     C*********************MOVELSVATTN    XXB401    P
     C*********************Z-ADD5         XXRSN
     C*********************EXSR WRTPEN
     C*********            ENDIF
     C*********************
     C*********  SCCNT3    IFNE SVCNT3
     C*********            MOVELSCCNT3    EMCNT3
     C*********************MOVELSCCNT3    XXAF01    P
     C*********************MOVELSVCNT3    XXB401    P
     C*********************Z-ADD6         XXRSN
     C*********************EXSR WRTPEN
     C*********            ENDIF
      *****END*DEL ****** 04/28/99 *********************************************
      ***** END DEL ******* 01/31/02 **********************************
     C*
     C     SCTPOB        IFNE      SVTPOB
     C                   MOVEL     SCTPOB        EMTPOB
     C                   MOVEL(P)  SCTPBD        XXAF01
     C                   MOVEL(P)  SVTPBD        XXB401
     C                   Z-ADD     7             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      **** BEGIN DEL **** 04/15/99 *********************************************
     C***********SCOOCG    IFEQ 'Y'
     C************INKC     ANDEQ*OFF
     C************INKL     ANDEQ*OFF
     C************         CALL 'W1057R'  OOPL
     C************         ENDIF
      **** END DEL ****** 04/15/99 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      ***** BEGIN ADD ***** 04/25/06 **********************************
      *
      * Gets the Id for this client
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      * PRINT PERSONAL NAME FIELDS FOR CA CLIENTS.
      *
     C     CLTID         IFEQ      '132'
     C     CLTID         OREQ      '136'
     C                   MOVE      *ON           *IN17
     C                   ELSE
     C                   MOVE      *OFF          *IN17
     C                   ENDIF
      ***** END ADD ******* 04/25/06 **********************************
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C                   MOVE      *ON           FIRST             1
     C                   MOVEL     'C'           @@MODE            1
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   MOVE      @@EEDT        SCEEDT
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN1        CHAIN     PEL01                              60
     C*
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELFND         ##FND
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELFYR         ##FYR
     C                   MOVE      ELEMP#        ##EMP#
     C                   MOVE      ELDIV         ##DIV
     C*
     C                   MOVEL     *BLANK        ##CONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              99
      **** END   ADD **** 10/14/98 *********************************************
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN58                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN58                          CANCELLED
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
     C*
     C*  GET ADMIN/DARTS INFORMATION
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
      ***** BEG ADD ******* 11/23/07 **********************************
     C                   MOVE      EXCNDR        XXCNDR
     C                   MOVE      EXCNST        XXCNST
      ***** END ADD ******* 11/23/07 **********************************
     C*********  *IN60     IFEQ *ON
     C*********            MOVELCNT1      EXCNT1
     C*********            MOVELCNT2      EXCNT2
     C*********            MOVELCNT3      EXCNT3
     C*********            ENDIF
     C*********
     C*****CONTACT HEADING 1
     C*********
     C*********  ' '       CHEKREXCNT1    XXLEN   30
     C*********            DIV  2         XXLEN
     C*********            MVR            XXLEN
     C*********
     C*********  XXLEN     IFEQ *ZERO
     C*********  EXCNT1    CAT  DOTS:1    SCCHD1
     C*********            ELSE
     C*********  EXCNT1    CAT  DOTS:0    SCCHD1
     C*********            ENDIF                           XXLEN=*ZERO
     C*********
     C*****CONTACT HEADING 2
     C*********
     C*********  ' '       CHEKREXCNT2    XXLEN
     C*********            DIV  2         XXLEN
     C*********            MVR            XXLEN
     C*********
     C*********  XXLEN     IFEQ *ZERO
     C*********  EXCNT2    CAT  DOTS:1    SCCHD2
     C*********            ELSE
     C*********  EXCNT2    CAT  DOTS:0    SCCHD2
     C*********            ENDIF                           XXLEN=*ZERO
     C*********
     C*****CONTACT HEADING 3
     C*********
     C*********  ' '       CHEKREXCNT3    XXLEN   30
     C*********            DIV  2         XXLEN
     C*********            MVR            XXLEN
     C*********
     C*********  XXLEN     IFEQ *ZERO
     C*********  EXCNT3    CAT  DOTS:1    SCCHD3
     C*********            ELSE
     C*********  EXCNT3    CAT  DOTS:0    SCCHD3
     C*********            ENDIF                           XXLEN=*ZERO
      ***** END DEL ******* 01/31/02 **********************************
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
      *** BEGIN ADD *** 07/28/99 ***********************************************
     C     QTKY          CHAIN     PQT01                              98
      *** END   ADD *** 07/28/99 ***********************************************
     C*
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
      *** BEGIN ADD *** 07/28/99 ***********************************************
     C                   MOVEL     Q1USER        QTUSER
     C                   MOVEL     Q1PGM         QTPGM
      *** END   ADD *** 07/28/99 ***********************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   MOVEL     DSTIME        EMTIME
      *** BEGIN ADD *** 07/28/99 ***********************************************
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     DSTIME        QTTIME
      *** END   ADD *** 07/28/99 ***********************************************
     C*
     C                   UPDATE    DBRCDF
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C                   EXSR      CICNTR
     C***  End   Add  ***  05/23/08  **************************************
      *** BEGIN ADD *** 07/28/99 ***********************************************
     C     *IN98         IFEQ      *OFF
     C                   UPDATE    PQT01
     C                   ENDIF                                                  *IN98 = *OFF
      *** END   ADD *** 07/28/99 ***********************************************
E01  C                   ENDIF
      **** BEGIN ADD **** 02/02/99 *********************************************
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     '0'
     C     ALELTP        IFEQ      'L'
      *** BEG DEL ***** 09/03/08 **************************************
     C***********          MOVELEMNAME    ALNAME
     C***********          MOVELEMFDNM    ALFDNM
      *** END DEL ***** 09/03/08 **************************************
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C                   MOVEL     EMTPOB        ALTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C                   Z-ADD     EMFEIN        ALFEIN
     C                   UPDATE    PAL01
     C                   ENDIF
     C     ALKY          READE     PAL01                                  60
     C                   ENDDO
      **** END ADD ****** 02/02/99 *********************************************
     C*** BEGIN ADD ***  12/27/11  ************************************
     C     EMNAME        IFNE      HNAME
     C     EMFEIN        ORNE      SVFEIN
     C                   CALL      'WA085R'
     C                   PARM      'E'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANKS       XXRTN             1
     C                   ENDIF
     C*** END ADD *****  12/27/11  ************************************
     C*
      **** BEG ADD ****** 11/23/07 *********************************************
     C     EMNAME        IFNE      HNAME
     C     EMNAM2        ORNE      HNAM2
     C     EMFDNM        ORNE      HFDNM
     C     EMFDN2        ORNE      HFDN2
     C     XXCNDR        ANDNE     *BLANKS
     C                   EXSR      DARTS
     C                   ENDIF
      **** END ADD ****** 11/23/07 *********************************************
     C                   ENDSR
      **** BEG ADD ****** 11/23/07 *********************************************
     C*================================================================
     C* DARTS  - WRITE DART REMAARKS
     C*================================================================
     C     DARTS         BEGSR
     C*
     C***  Begin Add  ***  05/28/08  **************************************
     C     *LIKE         DEFINE    *IN90         SVIN90
     C                   MOVE      *IN90         SVIN90
     C***  End   Add  ***  05/28/08  **************************************
    C*
     C     XXCNDR        CHAIN     PCT01                              89
     C     XXCNST        CHAIN     PUP01                              90
     C*
     C                   MOVE      *BLANK        QCQTE1
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT
     C*
     C                   MOVEL(P)  CTDESC        ##COMM
     C                   CAT       'STAFF:':5    ##COMM
     C                   CAT       XXCNST:1      ##COMM
     C                   MOVE      'Y'           ##DIFL
     C*
     C     *IN89         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
     C*
     C                   CALL      'DA700R'
     C                   PARM                    QCKEY1           40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'W0050R'      ##PNA1           10
     C                   PARM      '000'         ##SECL            3
      ***** BEG CHG ******* 04/28/09 **********************************
     C***********          PARM Q1USER    ##USER 10
     C                   PARM      UPUSR         ##USER           10
      ***** END CHG ******* 04/28/09 **********************************
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      *BLANKS       ##FL01            1
     C                   PARM      *BLANKS       ##FL02            1
     C                   PARM      XXCNDR        ##RMCD            5
     C                   PARM      '00000'       ##SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   ENDIF
     C*
     C***  Begin Add  ***  05/28/08  **************************************
     C                   MOVE      SVIN90        *IN90
     C***  End   Add  ***  05/28/08  **************************************
     C*
     C                   ENDSR
      **** END ADD ****** 11/23/07 *********************************************
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRTPEN - Write data base record to Endorsement Print Workfile
     C*================================================================
     C     WRTPEN        BEGSR
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        ENNCFL
     C                   MOVE      *BLANK        ENFIFL
     C                   MOVE      *BLANK        ENCNFL
     C                   MOVE      *BLANK        ENATFL
     C                   MOVE      *BLANK        ENC3FL
     C                   MOVE      *BLANK        ENTBFL
     C*
     C                   MOVE      ##CO#         ENCO#
     C                   MOVE      ##FND         ENFND
     C                   MOVE      ##EMP#        ENEMP#
     C                   MOVE      ##DIV         ENDIV
     C                   MOVE      ##FYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C     XXRSN         IFEQ      2
     C                   ADD       1             ENSEQ#
     C                   ELSE
     C                   Z-ADD     *ZERO         ENSEQ#
     C                   ENDIF
     C*
     C                   MOVE      @@EEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELSVENAM    ENNAM1
     C**********           MOVELSVFDNM    ENFDN1
     C                   MOVEL     SVNAM1        ENNAM1
     C                   MOVEL     SVFDN1        ENFDN1
     C                   MOVEL     SVNAM1        ENENM1
     C                   MOVEL     SVNAM2        ENENM2
     C                   MOVEL     SVFDN1        ENEFN1
     C                   MOVEL     SVFDN2        ENEFN2
     C***  End   Chg  ***  09/22/04  **************************************
     C                   MOVEL     SVCONT        ENCON1
     C                   MOVEL     SVATTN        ENATT1
     C                   MOVEL     SVCNT3        ENCN31
     C                   MOVE      SVFEIN        ENFEN1
     C                   MOVEL     SVTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
      **** BEGIN ADD **** 03/29/99 *********************************************
     C*********************MOVELEMNAME    ENNAM2
     C*********************MOVELEMFDNM    ENFDN2
     C*********************MOVELEMCONT    ENCON2
     C*********************MOVELEMATTN    ENATT2
     C*********************MOVELEMCNT3    ENCN32
     C*********************MOVE EMFEIN    ENFEN2
     C*********************MOVELEMTPOB    ENTOB2
      **** BEGIN CHG **** 05/05/99 *********************************************
     C*********************MOVELSCNAME    ENNAM2
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELSCENAM    ENNAM2
     C                   MOVEL     SCNAM1        ENNAM2
     C                   MOVEL     SCNAM1        ENANM1
     C                   MOVEL     SCNAM2        ENANM2
     C***  End   Chg  ***  09/22/04  **************************************
      **** END CHG ****** 05/05/99 *********************************************
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELSCFDNM    ENFDN2
     C                   MOVEL     SCFDN1        ENFDN2
     C                   MOVEL     SCFDN1        ENAFN1
     C                   MOVEL     SCFDN2        ENAFN2
     C***  End   Chg  ***  09/22/04  **************************************
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELSCCONT    ENCON2
     C*********            MOVELSCATTN    ENATT2
     C*********            MOVELSCCNT3    ENCN32
      ***** END DEL ******* 01/31/02 **********************************
     C                   MOVE      SCFEIN        ENFEN2
     C                   MOVEL     SCTPOB        ENTOB2
      **** END ADD ****** 03/29/99 *********************************************
     C*
     C                   SELECT
     C     XXRSN         WHENEQ    1                                            EMNAME
     C                   MOVE      'Y'           ENNCFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    2                                            EMFDNM
     C                   MOVE      'Y'           ENNCFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    3                                            EMFEIN
     C                   MOVE      'Y'           ENFIFL
     C                   MOVEL     DXEMFI        ENRMCD
     C     XXRSN         WHENEQ    4                                            EMCONT
     C                   MOVE      'Y'           ENCNFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    5                                            EMATTN
     C                   MOVE      'Y'           ENATFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    6                                            EMCNT3
     C                   MOVE      'Y'           ENC3FL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    7                                            EMTPOB
     C                   MOVE      'Y'           ENTBFL
     C                   MOVEL     DXEMLS        ENRMCD
     C                   ENDSL
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     @@RRN2        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
      **** BEGIN ADD **** 02/01/99 *********************************************
     C                   Z-ADD     ELPTDT        ENPTD1
      **** END   ADD **** 02/01/99 *********************************************
     C*
     C                   WRITE     PEN01
     C*
     C                   ENDSR
