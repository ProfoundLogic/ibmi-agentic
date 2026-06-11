     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4151R - WORK WITH ACCOUNT STATUS SEARCH
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                EMPLOYERS WITHIN THE QUOTE STATUS FILE.        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 11/20/97  PROGRAMMER: C:33  PROJECT: WC614          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*                                                               *
     H* 07/08/09  C:62  Project: 133WHN                               *
     H*                 Add search to quote type subset.              *
     H*                                                               *
     H* 05/28/09  C:33  Project: 133WGV                               *
     H*                 Add a new quote tracking type 'I' - Waiting   *
     H*                                                               *
     H* 10/19/07 C:193 PROJECT: WC1561                                *
     H*                ADD A NEW QUOTE TRACKING TYPE                  *
     H*                                                               *
     H* 03/20/07 C:136 SUPPORT: WA132 56398                           *
     H*                ENABLE PROGRAM TO SEARCH THE EMPLOYER FILE     *
     H*                LOCATED IN THE 'OTHER' ENVIRONMENT SINCE       *
     H*                WAQTP HOLDS BOTH QUOTED AND LIVE POLICIES.     *
     H*                                                               *
     H* 06/23/06 C:136 PROJECT: WC1342                                *
     H*                CORRECTIONS WITH AGENT SEACH                   *
     H*                CORRECTIONS WITH F9 AND F11                    *
     H*                                                               *
     H* 12/06/05 C:167 Project: 132WDO                                *
     H*                Add new quote type (U)                         *
     H*                                                               *
     H* 11/18/04 C:33  Project: 132WCB                                *
     H*                Add new quote type (U)                         *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*                Add new quote types - RENEWAL PENDING          *
     H*                                      RENEWAL QUOTE            *
     H*                                                               *
     H* 11/01/00  C:33  PROJECT: WC1008                               *
     H*                 USE TRANSFER TYPE (QTXFER) IF VALUED.         *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
     H*                                                               *
     H*  06/25/98  C:33  PROJECT: WC613                               *
     H*                  RESTRICT ACCESS AS PER USER PARMS            *
     H*                                                               *
     H*  06/07/98  C:33  PROJECT: WC395                               *
     H*                  ADD QUOTE STATUS' - BOUND, BOUND W/MONEY,    *
     H*                  TRANSFER                                     *
     H*****************************************************************
     FW4151F    CF   E             WORKSTN
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
     FWAQTL10   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PQT01:DBRCDF)
     F***  BEGIN ADD  ***  06/25/98  *********************************
      *** BEG DEL ***** 06/23/06 *************************************
     F**********  WAQTL11 IF  E           K        DISK
     F**********                                    KINFDS DBFDSA
     F**********  PQT01                             KRENAMEDBRCDFA
      *** END DEL ***** 06/23/06 *************************************
     F*  Secondary logical file definition (for displaying items)
     FWAQTL12   IF   E           K DISK
      *** beg chg ***** 06/23/06 *************************************
     F***********                                   KINFDS DBFDSB
     F                                     INFDS(DBFDS2)
      *** beg chg ***** 06/23/06 *************************************
     F                                     RENAME(PQT01:DBRCDFB)
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *** BEG ADD ***** 06/23/06 **************************************
     F                                     INFDS(FNIFDS)
      *** END ADD ***** 06/23/06 **************************************
     F***  END   ADD  ***  06/25/98  *********************************
     F*** BEG DEL ***** 06/23/06 *************************************
     F****WMAAL1  IF  E           K        DISK
     F****WMAGL   IF  E           K        DISK
     F*** END DEL ***** 06/23/06 *************************************
     FWMEML     IF   E           K DISK
     F*** BEG ADD ***** 03/20/07 *************************************
     FWMEMLQ    IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM01Q)
     F*** END ADD ***** 03/20/07 *************************************
      ***  Begin Chg  ***  12/06/05  **************************************
      *** BEG CHG ***** 06/23/06 ******************************************
     F*********WDELL   IF  E           K        DISK
     F*********WDELL1  IF  E           K        DISK
     F*********   PEL01                             KRENAMEPEL02
     FWDELL2    IF   E           K DISK
      *** END CHG ***** 06/23/06 ******************************************
     FWMRSL     IF   E           K DISK
      * User parms (subset subroutine)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      ***  End   Chg  ***  12/06/05  **************************************
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA0P)
     F***  Begin Add  ***  05/28/09  *************************************
     FWTQSL1    IF   E           K DISK
     F***  Begin Add  ***  05/28/09  *************************************
     F***  Begin Add  ***  07/08/09  **************************************
     FWTQSP     IF   E             DISK
     F                                     RENAME(PQS01:PQS02)
     F***  End   Add  ***  07/08/09  **************************************
      /COPY SCOPYWW01D
     D*
     D***  BEGIN CHG  ***  06/07/98  *********************************
     D**********          QTET    1   7  1   QTED   10   Quote Status
     D***  Begin Chg  ***  10/10/03  **************************************
     D**********          QTET    1  10  1   QTED   12   Quote Status
     D***  Begin Chg  ***  11/18/04  **************************************
     D**********          QTET    1  12  1   QTED   12   Quote Status
     D QTET            S              1    DIM(15) CTDATA PERRCD(1)             Quote Status
     D QTED            S             12    DIM(15) ALT(QTET)
     D***  End   Chg  ***  11/18/04  **************************************
     D***  End   Chg  ***  10/10/03  **************************************
     D***  END   CHG  ***  06/07/98  *********************************
      ***  Begin Chg  ***  12/06/05  **************************************
     D*****               HD      1   1 78               Column headings
     D*****               OF      1  10  1   OFD    35   Options, Function
      ***  Begin Chg  ***  06/23/06  **************************************
     D**************      HD      1   4 78               Column headings
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings
      ***  END   Chg  ***  06/23/06  **************************************
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
      ***  End   Chg  ***  12/06/05  **************************************
      ***  Begin Add  ***  12/06/05  **************************************
     D DTA1            S              1    DIM(78)
     D DTA2            S              1    DIM(68)
     D PH              S              1    DIM(14)
     D CSZ             S              1    DIM(20)
      ***  End   Add  ***  12/06/05  **************************************
     D*** BEG ADD ***** 03/20/07 * ************************************
     D QCA             S             55    DIM(3) CTDATA PERRCD(1)              CMD EXEC/QCAEXEC
     D*** END ADD ***** 03/20/07 * ************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D***  BEGIN ADD  ***  06/25/98  *********************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D***  END   ADD  ***  06/25/98  *********************************
     D*** BEG ADD ***** 06/23/06 *************************************
     D FNIFDS          DS
     D  FNRRN                397    400B 0
     D*** END ADD ***** 06/23/06 *************************************
     D*
      *** BEG ADD ***** 06/23/06 *************************************
     D                 DS
     D  VIEW1                  1     42
     D  V1NAME                 1     25
     D  V1EMP#                27     35
     D  V1DIV                 37     41
      *
     D                 DS
     D  VIEW2                  1     45
     D  V2FND                  1      3
     D  V2EMP#                 5     13
     D  V2DIV                 15     19
     D  V2NAME                21     44
      *
     D                 DS
     D  INFO11                 1     33
     D  I11STS                 1     12
     D  I11FND                14     16
     D  I11AG#                18     22
     D  I11AGN                24     33
      *
     D                 DS
     D  INFO12                 1     33
     D  I12FND                 1      3
     D  I12AG#                 5      9
     D  I12AGN                11     21
     D  I12PRO                23     32
      *
     D                 DS
     D  INFO13                 1     33
     D  I13FND                 1      3
     D  I13EFD                 5     14
     D  I13CXD                16     26
     D                 DS
     D  INFO21                 1     33
     D  I21STS                 1     12
     D  I21AG#                14     18
     D  I21AGN                20     30
      *
     D                 DS
     D  INFO22                 1     33
     D  I22AG#                 1      3
     D  I22AGN                 5     15
     D  I22PRO                17     26
      *
     D                 DS
     D  INFO23                 1     33
     D  I23EFD                 1     10
     D  I23CXD                12     21
      *** END ADD ***** 06/23/06 *************************************
      *** BEG DEL ***** 06/23/06 *************************************
      ***  Begin Chg  ***  12/06/05  **************************************
     D**********  DS
     D**********                              1  20 FED
     D**********                              1   3 YYFND
     D**********                              5  13 YYEMP#
     D**********                             15  19 YYDIV
     D**********  DS
     D**********                              1  52 INFO1
     D**********                              1   9 XXEMP#
     D**********                             11  15 XXDIV
     D**********                             17  28 XXSTS
     D**********                             31  33 XXFND
     D**********                             35  39 XXAGT#
     D**********                             41  52 SCANAM
     D**********  DS
     D**********                              1  32 INFO2
     D**********                              1  12 I2STS
     D**********                             14  18 I2AGT#
     D**********                             21  32 I2ANAM
     D**********  DS
     D**********                              1  19 INFO3
     D**********                              1   9 ZZEMP#
     D**********                             11  15 ZZDIV
     D**********                             17  19 ZZFND
     D**********  DS
     D**********                              1  27 INFO4
     D**********                              1  10 XXEFDT
     D**********                             18  27 XXCXDT
     D**********  DS
     D**********                              1  30 INFO5
     D**********                              1   5 I5AGT#
     D**********                              7  18 I5ANAM
     D**********                             21  30 I5PROD
      *** END DEL ***** 06/23/06 *************************************
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      ***  End   Chg  ***  12/06/05  **************************************
      *** BEG ADD ***** 06/23/06 ******************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *** END ADD ***** 06/23/06 ******************************************
      *** BEG DEL ***** 06/23/06 ******************************************
     D**********  DS
     D**********                              1  78 SCDATA
     D**********                              1  25 SCNAME
      *****Begin Chg  ***  12/06/05  **************************************
     D**********                             27  35 XXEMP#
     D**********                             37  41 XXDIV
      *****End** Chg  ***  12/06/05  **************************************
     D*****BEGIN CHG  ***  06/07/98  *********************************
     D**********                             43  53 XXSTS
     D**********                             55  57 XXFND
     D**********                             59  63 XXAGT#
     D**********                             65  78 SCANAM
      *****Begin Chg  ***  12/06/05  **************************************
     D**********                             43  55 XXSTS
     D**********                             57  59 XXFND
     D**********                             61  65 XXAGT#
     D**********                             67  78 SCANAM
      ***  End   Chg  ***  12/06/05  **************************************
      *** END DEL ***** 06/23/06 ******************************************
     D***  BEGIN CHG  ***  06/07/98  *********************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Account Status Searc-
     D                                     h')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D***  Begin Chg  ***  12/06/05  **************************************
     D********      1                     C         INFOMX
     D********      1                     C         VIEWMX
     D*** beg chg ***** 06/23/06 ******************************************
     D********      2                     C         INFOMX
     D INFOMX          C                   CONST(3)
     D*** end chg ***** 06/23/06 ******************************************
     D VIEWMX          C                   CONST(2)
     D***  End   Chg  ***  12/06/05  **************************************
     D SF1PAG          C                   CONST(15)
      ***  Begin Add  ***  12/06/05  **************************************
     D COMMA           C                   CONST(',')
      ***  End   Add  ***  12/06/05  **************************************
      *** BEG ADD ***** 03/20/07 ******************************************
     D GETLIB          C                   CONST('RTVENVLIB')
      *** END ADD ***** 03/20/07 ******************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C                   PARM                    ##XEMP            1
     C***  END   ADD  ***  06/25/98  *********************************
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   PARM                    ##XDIV            1
     C***  END   ADD  ***  05/10/99  *********************************
     C*
      ***  Begin Add  ***  12/06/05  **************************************
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      QTFND         #2FND             3 0
     C                   PARM      QTEMP#        ##EMP#            9 0
     C                   PARM      QTDIV         ##DIV             5 0
     C                   PARM      XXFYR         ##FYR             3 0
     C                   PARM      QTAGT#        ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM                    ##AREA            3 0
     C                   PARM                    ##PHON            7 0
     C*
      ***  End   Add  ***  12/06/05  **************************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      ***  Begin Add  ***  12/06/05  **************************************
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      3             ##VIEW            1 0
      ***  End   Add  ***  12/06/05  **************************************
      *** BEG ADD ***** 06/23/06 ******************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** END ADD ***** 06/23/06 ******************************************
      *
      *** BEG ADD ***** 03/20/07 ******************************************
     C     LIBPRM        PLIST
     C                   PARM      '*OTHER'      RQSTYP           10
     C                   PARM                    LIBNAM           10
     C                   PARM                    MLIBNM           10
      *** END ADD ***** 03/20/07 ******************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNAME
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
      *** BEG ADD ***** 06/23/06 *************************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKNAME
      *** END ADD ***** 06/23/06 *************************************
     C*
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C     KEYA          KLIST
     C                   KFLD                    KKCO#
      ***  Begin chg  ***  12/06/05  **************************************
     C********             KFLD           KKFND
     C                   KFLD                    SSFND
      ***  End   chg  ***  12/06/05  **************************************
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
      ***  Begin chg  ***  12/06/05  **************************************
     C********             KFLD           KKFND
     C                   KFLD                    SSFND
      ***  End   chg  ***  12/06/05  **************************************
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEYB          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKNAME
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     KEYC          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C***  END   ADD  ***  06/25/98  *********************************
     C*** BEG ADD ***** 06/23/06 *************************************
     C     FNKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C*** END ADD ***** 06/23/06 *************************************
     C*
     C*  KEY FOR AGENT ASSIGNMENT FILE WMAAL
     C*
     C     AAKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTAGT#
     C*
     C*  KEY FOR AGENTS NAME FILE WMAGL
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      ***  Begin Add  ***  12/06/05  **************************************
      *** BEG DEL ***** 06/23/06 ******************************************
     C**********
     C***EMPLOYER DETAIL FILE
     C**********
     C********** ELKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           SCFND
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCDIV
      *** END DEL ***** 06/23/06 ******************************************
      *** BEG ADD *****10/10/03 **************************************
      *** BEG CHG ***** 06/23/06 *************************************
     C*********  ELKY1     KLIST
     C*********            KFLD           ##CO#
     C*********            KFLD           K1FND
     C*********            KFLD           K1EMP#
     C*********
     C*********  ELKY2     KLIST
     C*********            KFLD           ##CO#
     C*********            KFLD           K1FND
     C*********            KFLD           K1EMP#
     C*********            KFLD           K1DIV
     C*********
     C*********  ELKY3     KLIST
     C*********            KFLD           ##CO#
     C*********            KFLD           K1FND
     C*********            KFLD           K1FYR
     C*********            KFLD           K1EMP#
     C*
     C*   PAYROLL CLASSIFICATION FILE (FOR RATE SET CODE) WMRSP
     C*
     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKRTCD
      *
     C     ELKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C                   KFLD                    KKSYDT
      *** END CHG ***** 06/23/06 *************************************
      ***  End   Add  ***  12/06/05  **************************************
     C*
     C*  KEY FOR EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C*
      ***** Beg add ***** 12/06/05 ************************************
     C     RRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    ##RRST            2
      ***** End add ***** 12/06/05 ************************************
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
     C     *LIKE         DEFINE    QTCO#         KKCO#
     C     *LIKE         DEFINE    QTNAME        KKNAME
     C     *LIKE         DEFINE    QTFND         KKFND
     C     *LIKE         DEFINE    QTEMP#        KKEMP#
     C     *LIKE         DEFINE    QTDIV         KKDIV
     C     *LIKE         DEFINE    QTNAME        U1PDSC
     C     *LIKE         DEFINE    QTNAME        U2PDSC
      ***  Begin Add  ***  12/06/05  **************************************
     C     *LIKE         DEFINE    EMFND         K1FND
     C     *LIKE         DEFINE    EMEMP#        K1EMP#
     C     *LIKE         DEFINE    EMDIV         K1DIV
     C     *LIKE         DEFINE    ELFYR         K1FYR
     C     *LIKE         DEFINE    EMMAD1        XXADR1
     C***  Begin Add  ***  05/28/09  *************************************
     C     *LIKE         DEFINE    QSKEY         XXKEY
     C***  End   Add  ***  05/28/09  *************************************
     C                   MOVE      0             *IN15
      ***  End   Add  ***  12/06/05  **************************************
     C/COPY SCOPYWW01C
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
      ***  Begin Del  ***  12/06/05  **************************************
     C*          SSNAME    IFNE *BLANK
     C*          SSFEIN    ORNE *ZERO
     C*          SSFND     ORNE *ZERO
     C*          SSAGT#    ORNE *ZERO
     C*          SSSTAT    ORNE *BLANK
     C*          SSTYPE    ORNE *BLANK
     C*                    MOVE *ON       *IN11
     C*                    ENDIF                           SSNAME<>NAME
      ***  End   Del  ***  12/06/05  **************************************
     C*
      ***  Begin Add  ***  12/06/05  **************************************
     C     *IN34         IFEQ      *OFF
      *
B01  C     SSFND         IFNE      *ZEROS
     C     SSNAME        ORNE      *BLANKS
     C     SSFDNM        ORNE      *BLANKS
     C     SSPAD1        ORNE      *BLANKS
     C     SSPHN#        ORNE      *BLANKS
     C     SSAGT#        ORNE      *ZEROS
     C     SSTYPE        ORNE      *BLANKS
     C     SSJRST        ORNE      *BLANKS
     C     SSUWTR        ORNE      *BLANKS
     C     SSMAD1        ORNE      *BLANKS
     C     SSSEMP        ORNE      *BLANKS
     C     SSRSK#        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSFND<>*ZEROS
     C                   ENDIF
      ***  End   Add  ***  12/06/05  **************************************
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C     SSFND         IFNE      *ZERO
     C     ##XFND        ANDEQ     'Y'
     C                   MOVE      *OFF          *IN11
     C                   ENDIF                                                  SSFND<>*ZERO
     C***  END   ADD  ***  06/25/98  *********************************
     C*
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
      *
      ***  BEG ADD ***** 06/23/06 *****************************************
     C  N90SSFND         IFNE      *ZERO
     C     FNKY1         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90HFLD          IFEQ      'SSAGT#'
     C     SSFND         ANDEQ     *ZEROS
     C     *INKJ         ANDEQ     *ON
     C                   SETON                                        72  90
     C                   ENDIF
      ***  END ADD ***** 06/23/06 *****************************************
      *
     C  N90SSTYPE        IFNE      *BLANK
     C***  Begin Chg  ***  05/28/09  *************************************
     C********** SSTYPE    ANDNE'W'
     C********** SSTYPE    ANDNE'Q'
     C********** SSTYPE    ANDNE'L'
     C********** SSTYPE    ANDNE'C'
     C********** SSTYPE    ANDNE'P'
     C********** SSTYPE    ANDNE'D'
     C********** SSTYPE    ANDNE'R'
     C*****BEGIN ADD  ***  06/07/98  *********************************
     C********** SSTYPE    ANDNE'B'
     C********** SSTYPE    ANDNE'M'
     C*****END** ADD  ***  06/07/98  *********************************
     C*****BEGIN ADD  ***  10/10/03  ***136***************************
     C********** SSTYPE    ANDNE'F'
     C********** SSTYPE    ANDNE'G'
     C*****END** ADD  ***  10/10/03  ***136***************************
     C*****Begin Add  ***  11/18/04  **************************************
     C********** SSTYPE    ANDNE'U'
     C*****End** Add  ***  11/18/04  **************************************
     C*****Begin Add  ***  10/19/07  **************************************
     C********** SSTYPE    ANDNE'S'
     C*****End** Add  ***  10/19/07  **************************************
     C*** BEGIN ADD *** 07/08/09 **************************************
     C                   MOVEL     SSTYPE        XXKEY
     C*** END   ADD *** 07/08/09 **************************************
     C     QSKY          CHAIN     PQS01                              60
     C     *IN60         IFEQ      *ON
     C***  End   Chg  ***  05/28/09  *************************************
     C                   SETON                                        71  90
     C***  Begin Add  ***  05/28/09  *************************************
     C                   ENDIF
     C***  End   Add  ***  05/28/09  *************************************
     C                   ENDIF                                                  SSTYPE<>*BLANK
     C*
      ***  BEG ADD ***** 06/23/06 *****************************************
     C  N90SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     'W'
     C     SSSTAT        ANDNE     'Q'
     C                   SETON                                        73  90
     C                   ENDIF
      *
     C  N90              EXSR      OTHFKS
      ***  END ADD ***** 06/23/06 *****************************************
     C*
     C                   ENDSR                                                  ERRCK2
      ***  Begin Add  ***  12/06/05  **************************************
      *===============================================================
      * GETAGT - SUBROUTINE TO GET AGENT RECORDS
      *===============================================================
     C     GETAGT        BEGSR
     C                   Z-ADD     XXFYR         ##FYR
     C                   CALL      AGTAPI        AGTPL
     C                   MOVEL(P)  ##ANAM        XXAGTN           10
     C                   MOVEL(P)  ##PNAM        XXPROD            9
     C                   ENDSR                                                  GETAGT
      ***  End   Add  ***  12/06/05  **************************************
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   CLEAR                   SSNAME
     C                   CLEAR                   SSFEIN
     C                   CLEAR                   SSFND
     C                   CLEAR                   SSAGT#
     C                   CLEAR                   SSTYPE
     C                   CLEAR                   SSSTAT
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   CLEAR                   SSEMP#
     C                   CLEAR                   SSDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C***  Begin add  ***  12/06/05  *********************************
     C                   CLEAR                   SSSEMP
     C                   CLEAR                   SSRSK#
     C***  End   add  ***  12/06/05  *********************************
     C*
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C   33              Z-ADD     FNFND         SSFND
     C*
     C     ##XEMP        IFEQ      'Y'
     C                   Z-ADD     UPEMP#        SSEMP#
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     ##XDIV        IFEQ      'Y'
     C                   MOVE      UPDIV         SSDIV
     C                   ENDIF                                                  ##XDIV=Y
     C***  END   ADD  ***  05/10/99  *********************************
     C                   ENDIF                                                  ##XEMP=Y
     C***  END   ADD  ***  06/25/98  *********************************
     C*
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C***  BEGIN ADD  ***  06/25/98  *********************************
B01  C     ##XFND        IFEQ      'Y'
B01  C     ##XEMP        OREQ      'Y'
     C     FNKY          CHAIN     WMFNL                              99
     C                   SETON                                        33
E01  C                   ENDIF
     C***  END   ADD  ***  06/25/98  *********************************
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   CLEAR                   KKNAME
     C***  BEGIN CHG  ***  06/25/98  *********************************
     C**********           CLEARKKFND
     C***  Begin Del  ***  12/06/05  *********************************
     C* N33                CLEARKKFND
     C***  End   Del  ***  12/06/05  *********************************
     C   33              Z-ADD     FNFND         KKFND
     C***  END   CHG  ***  06/25/98  *********************************
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
     C*
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C     ##XEMP        IFEQ      'Y'
     C                   Z-ADD     UPEMP#        KKEMP#
     C                   ENDIF                                                  ##XEMP=Y
     C***  END   ADD  ***  06/25/98  *********************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKNAME
     C*
     C***  BEGIN CHG  ***  06/25/98  *********************************
     C**********           CLEARKKFND
     C**********           CLEARKKEMP#
     C     ##XFND        IFNE      'Y'
     C                   CLEAR                   KKFND
     C                   ENDIF                                                  ##XFND<>Y
     C*
     C     ##XEMP        IFNE      'Y'
     C                   CLEAR                   KKEMP#
     C                   ENDIF                                                  ##XFND<>Y
     C***  END   CHG  ***  06/25/98  *********************************
     C*
     C                   CLEAR                   KKDIV
     C*
      ***  Begin Add  ***  12/06/05  **************************************
     C                   MOVEL     SCPST2        KKFND
     C                   MOVEL     SCPST9        KKEMP#
     C                   MOVE      SCPSTB        KKDIV
      ***  End   Add  ***  12/06/05  **************************************
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCNAME        KKNAME
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C*
     C                   ENDSR                                                  KEYSFL
      ***  Begin Add  ***  12/06/05  **************************************
     C*===============================================================
     C* KJDO   - SEARCH SUBROUTINE
     C*===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SSFND'
     C                   MOVE      ##CO#         #1CO#
      *
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           #2CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1FND             3
      *
     C     #1FND         IFNE      *BLANKS
     C                   MOVE      #1FND         SSFND
     C                   SETON                                        86        Position cursor
     C                   ELSE
     C                   SETON                                        85        Position cursor
     C                   ENDIF                                                  #1FND<>*BLANKS
      *
     C     HFLD          WHENEQ    'SSAGT#'
     C     FNKY1         CHAIN     PFN01                              60
     C                   Z-ADD     FNRRN         @@RRN
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@VIEW            1 0
      *** BEG ADD ***** 06/23/06 **********************************
     C     @@RRN         CHAIN     PAA0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AAAGT#        SSAGT#
     C                   SETON                                        82         Position cursor
     C                   ELSE
     C                   SETON                                        81         Position cursor
     C                   ENDIF                                                  *IN60       O
      *** END ADD ***** 06/23/06 **********************************
     C     HFLD          WHENEQ    'SSUWTR'
     C                   CALL      'S0005R'      UPRMPL                 99
      *
     C     ##RRN3        IFNE      *ZEROS
     C     ##RRN3        CHAIN     PUP03                              99
     C                   MOVEL     UPSTAF        SSUWTR
     C                   SETON                                        84         Position cursor
     C                   ELSE
     C                   SETON                                        83         Position cursor
     C                   ENDIF
     C*** BEGIN ADD *** 07/08/09 **************************************
      *
     C     HFLD          WHENEQ    'SSTYPE'
     C                   CALL      'W2150R'                             99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PQS02                              99
     C                   MOVEL     QSKEY         SSTYPE
     C                   SETON                                        32         Position cursor
     C                   ELSE
     C                   SETON                                        31         Position cursor
     C                   ENDIF
     C*** END   ADD *** 07/08/09 **************************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
     C*===============================================================
     C* LDADDR - SUBROUTINE TO LOAD THE ADDRESS LINES
     C*===============================================================
     C     LDADDR        BEGSR
      *
     C                   CLEAR                   DTA2
     C                   CLEAR                   CSZ
      *
     C     ' '           CHECKR    XXADR1        XXLEN
     C                   Z-ADD     XXLEN         X                 3 0
      *
     C     ' '           CHECKR    XXADR2        XXLEN             3 0
     C                   Z-ADD     XXLEN         Y                 3 0
      *
     C     X             ADD       Y             Z                 3 0
      *
     C                   MOVEA     XXADR1        DTA2
      *
B01  C     XXADR2        IFNE      *BLANKS
      *
B02  C     Z             IFGT      44
      *
     C     22            SUB       Y             Y
     C                   MULT      2             Y
     C     22            ADD       1             Y
     C                   MOVEA     COMMA         DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     *BLANK        DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     XXADR2        DTA2(Y)
      *
X02  C                   ELSE
      *
     C                   ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     XXADR2        DTA2(X)
      *
E02  C                   ENDIF                                                  Z>44
      *
E01  C                   ENDIF                                                  XXADR2<>*BLANKS
      *
     C                   MOVEA     XXCTY         CSZ
     C     ' '           CHECKR    XXCTY         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     *BLANK        CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     XXST          CSZ(X)
     C                   ADD       3             X
     C                   MOVEA     XXZIP         CSZ(X)
      *
     C                   MOVEA     DTA2          XDTA2            48
     C     ' '           CHECKR    XDTA2         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     CSZ           DTA2(X)
      *
     C                   MOVEL     *BLANKS       XXADR1
     C                   MOVEL     *BLANKS       XXADR2
     C                   MOVEL     *BLANKS       XXCTY
     C                   MOVEL     *BLANKS       XXST
     C                   MOVEL     *BLANKS       XXZIP
      *
     C                   ENDSR                                                  LDADDR
      *
      ***  End   Add  ***  12/06/05  **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *** beg del ***** 06/23/06 ************************************
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C*********            SELEC
     C*****BEGIN ADD  ***  05/10/99  *********************************
     C*********  SSFND     WHNE *ZEROS
     C*********  SSEMP#    ANDNE*ZEROS
     C*********  SSDIV     ANDNE*BLANKS
     C*********            Z-ADDDBRRNB    SCDBRR
     C*****END*  ADD  ***  05/10/99  *********************************
     C*********  SSFND     WHNE *ZEROS
     C*********  SSEMP#    ANDNE*ZEROS
     C*********            Z-ADDDBRRNB    SCDBRR
     C*********  SSFND     WHNE *ZEROS
     C*********            Z-ADDDBRRNA    SCDBRR
     C*********            ENDSL
     C***  END   ADD  ***  06/25/98  *********************************
      *** end del ***** 06/23/06 ************************************
     C*
     C                   MOVEL(P)  QTNAME        SCNAME
     C                   Z-ADD     QTFND         SCFND
     C                   Z-ADD     QTEMP#        SCEMP#
     C                   Z-ADD     QTDIV         SCDIV
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  QTEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#            9
      ***  Begin Add  ***  12/06/05  **************************************
      ***  Begin DEL  ***  06/23/06  **************************************
     C**********           MOVELXXNUM     YYEMP#
     C**********           MOVELXXNUM     ZZEMP#
      ***  END   DEL  ***  06/23/06  **************************************
      ***  End   Add  ***  12/06/05  **************************************
     C*
     C                   MOVEL(P)  QTDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDIV             5
      ***  Begin Add  ***  12/06/05  **************************************
      ***  Begin DEL  ***  06/23/06  **************************************
     C**********           MOVELXXNUM     YYDIV
     C**********           MOVELXXNUM     ZZDIV
      ***  END   DEL  ***  06/23/06  **************************************
      ***  End   Add  ***  12/06/05  **************************************
     C*
     C                   MOVEL(P)  QTFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFND             3
      ***  Begin Add  ***  12/06/05  **************************************
      ***  Begin DEL  ***  06/23/06  **************************************
     C**********           MOVELXXNUM     YYFND
     C**********           MOVELXXNUM     ZZFND
      ***  END   DEL  ***  06/23/06  **************************************
      ***  End   Add  ***  12/06/05  **************************************
     C*
     C                   MOVEL(P)  QTAGT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAGT#            5
     I***  Begin Chg  ***  12/06/05  **************************************
      ***  Begin DEL  ***  06/23/06  **************************************
     C**********           MOVELXXNUM     I2AGT#
     C**********           MOVELXXNUM     I5AGT#
      ***  END   DEL  ***  06/23/06  **************************************
     I***  End   Chg  ***  12/06/05  **************************************
     C*
     C***  Begin Add  ***  05/28/09  **************************************
     C                   MOVEL     QTTYPE        XXKEY
     C     QSKY          CHAIN     PQS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  QSDESC        XXSTS
     C                   ELSE
     C***  End   Add  ***  05/28/09  **************************************
     C                   Z-ADD     1             X                 3 0
     C                   MOVEL     *BLANKS       XXSTS            13
     C     QTTYPE        LOOKUP    QTET(X)                                60
     C   60              MOVEL     QTED(X)       XXSTS
     C***  Begin Add  ***  05/28/09  **************************************
     C                   ENDIF
     C***  End   Add  ***  05/28/09  **************************************
     C*
     C                   MOVEL     *BLANKS       XXAGTN           10
     C                   MOVEL     *BLANKS       XXPROD            9
      *** BEG DEL ***** 06/23/06 ******************************************
     C*********  AAKY      CHAINPAA01                60
     C**N60****  AGKY      CHAINPAG01                60
     C**N60****            MOVELAGNAME    XXAGTN 10 P
      *** END DEL ***** 06/23/06 ******************************************
      ***  Begin Chg  ***  12/06/05  **************************************
     C**N60********        MOVELAGNAME    I2ANAM    P
     C**N60********        MOVELAGNAME    I5ANAM    P
      *** END DEL ***** 06/23/06 ******************************************
     C                   EXSR      GETAGT
      *
     C                   CLEAR                   SCMADR
     C                   CLEAR                   SCPADR
     C                   CLEAR                   XXEFDT
     C                   CLEAR                   XXCXDT
     C     EMKY          CHAIN     PEM01                              60
      ***  End   Chg  ***  12/06/05  **************************************
     C     *IN60         IFEQ      *OFF
      ***  Begin Add  ***  12/06/05  **************************************
      *  LOAD MAILING ADDRESS
      *
     C                   MOVEL     'Mail:'       SCMAIL
      *
B01  C     EMMAD1        IFNE      *BLANKS
      *
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2           22
     C                   MOVEL     EMMCTY        XXCTY            10
     C                   MOVEL     EMMST         XXST              2
     C                   MOVEL     EMMZIP        XXZIP             5
     C                   MOVE      EMMST         ##RRST
     C                   EXSR      LDADDR
     C                   MOVEA     DTA2          SCMADR
X01  C                   ELSE
     C                   MOVEL     *BLANKS       SCMADR
E01  C                   ENDIF                                                  EMMAD1<>*BLANKS
      *
      *  LOAD PHYSICAL ADDRESS
      *
     C                   MOVEL     'Phys:'       SCPHYS
B01  C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZIP
     C                   MOVE      EMPST         ##RRST
     C                   EXSR      LDADDR
     C                   MOVEA     DTA2          SCPADR
X01  C                   ELSE
     C                   MOVEL     *BLANKS       SCPADR
E01  C                   ENDIF                                                  EMPAD1<>*BLANKS
      *
      *  GET MOST RECENT INCEPTION DATE AND CORRESPONDING CX DATE
      *
     C                   Z-ADD     5             I1                3 0
B01  C     I1            DOWNE     *ZERO
B02  C     EF(I1)        IFNE      *ZERO
     C                   Z-ADD     EF(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZEFDT            8 0
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZCXDT            8 0
     C                   LEAVE
E02  C                   ENDIF                                                  EF,I1<>*ZERO
     C                   SUB       1             I1
E01  C                   ENDDO                                                  I1->5
      *
     C                   MOVEL     ZZEFDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEFDT           10
      *
     C                   MOVEL     ZZCXDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCXDT           10
      *
     C                   ENDIF
      *
      *  LOAD DATA INTO SCREEN FIELD
      *
     C                   CLEAR                   SCDATA
     C                   CLEAR                   DTA1
      *** BEG DEL ***** 06/23/06 **********************************
B01  C***********          SELEC
S01  C***********VIEW#     WHEQ 1
     C***********INFO#     ANDEQ1
     C************         MOVEAQTNAME    DTA1
     C************         Z-ADD27        Z       30
     C************         MOVEAINFO1     DTA1,Z
     C************         MOVEADTA1      SCDATA
      ************
S02  C***********VIEW#     WHEQ 2
     C***********INFO#     ANDEQ1
     C************         MOVEAFED       DTA1
     C************         Z-ADD21        Z
     C************         MOVEAQTNAME    DTA1,Z
     C************         Z-ADD47        Z
     C************         MOVEAINFO2     DTA1,Z
     C************         MOVEADTA1      SCDATA
      ************
S05  C***********VIEW#     WHEQ 1
     C***********INFO#     ANDEQ2
     C************         MOVEAQTNAME    DTA1
     C************         Z-ADD27        Z       30
     C************         MOVEAINFO3     DTA1,Z
     C************         Z-ADD47        Z
     C************         MOVEAINFO5     DTA1,Z
     C************         MOVEADTA1      SCDATA
      ************
S06  C***********VIEW#     WHEQ 2
     C***********INFO#     ANDEQ2
     C************         MOVEAQTNAME    DTA1
     C************         Z-ADD27        Z       30
     C************         MOVEAINFO3     DTA1,Z
     C************         Z-ADD48        Z
     C************         MOVEAINFO4     DTA1,Z
     C************         MOVEADTA1      SCDATA
     C************         ENDSL
      *** BEG DEL ***** 06/23/06 **********************************
      *
      *** BEG ADD ***** 06/23/06 **********************************
     C     VIEW#         IFEQ      1
     C                   MOVEL(P)  QTNAME        V1NAME
     C                   MOVE(P)   XXEMP#        V1EMP#
     C                   MOVE(P)   XXDIV         V1DIV
     C                   MOVEL     VIEW1         SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      2
     C                   MOVE(P)   XXFND         V2FND
     C                   MOVE(P)   XXEMP#        V2EMP#
     C                   MOVE(P)   XXDIV         V2DIV
     C                   MOVEL(P)  QTNAME        V2NAME
     C                   MOVEL     VIEW2         SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXSTS         I11STS
     C                   MOVE(P)   XXFND         I11FND
     C                   MOVE(P)   XXAGT#        I11AG#
     C                   MOVEL(P)  XXAGTN        I11AGN
     C                   MOVE      INFO11        SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      1
     C     INFO#         ANDEQ     2
     C                   MOVE(P)   XXFND         I12FND
     C                   MOVE(P)   XXAGT#        I12AG#
     C                   MOVEL(P)  XXAGTN        I12AGN
     C                   MOVEL(P)  XXPROD        I12PRO
     C                   MOVE      INFO12        SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      1
     C     INFO#         ANDEQ     3
     C                   MOVE(P)   XXFND         I13FND
     C                   MOVE(P)   XXEFDT        I13EFD
     C                   MOVE(P)   XXCXDT        I13CXD
     C                   MOVE      INFO13        SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      2
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXSTS         I21STS
     C                   MOVE(P)   XXAGT#        I21AG#
     C                   MOVEL(P)  XXAGTN        I21AGN
     C                   MOVE      INFO21        SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      2
     C     INFO#         ANDEQ     2
     C                   MOVE(P)   XXAGT#        I22AG#
     C                   MOVEL(P)  XXAGTN        I22AGN
     C                   MOVEL(P)  XXPROD        I22PRO
     C                   MOVE      INFO22        SCDATA
     C                   ENDIF
      *
     C     VIEW#         IFEQ      2
     C     INFO#         ANDEQ     3
     C                   MOVE(P)   XXEFDT        I23EFD
     C                   MOVE(P)   XXCXDT        I23CXD
     C                   MOVE      INFO23        SCDATA
     C                   ENDIF
      *** END ADD ***** 06/23/06 **********************************
      *
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
      *** Begin Add *** 12/06/05 *************************************
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *** End   Add *** 12/06/05 *************************************
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
      *** BEG ADD ***** 03/20/07 ******************************************
     C     LIBNAM        IFNE      *BLANKS
     C                   MOVEL(P)  QCA(3)        CMDSTR
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR           55
     C                   PARM                    LENGTH           15 5
     C                   ENDIF
     C                   CLOSE     WMEMLQ
      *** END ADD ***** 03/20/07 ******************************************
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      *** BEG ADD ***** 06/23/06 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKSYDT            7 0
      *** END ADD ***** 06/23/06 **************************************
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C*
     C***  BEGIN ADD  ***  06/25/98  *********************************
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     ##XEMP        IFEQ      'Y'
     C                   Z-ADD     UPEMP#        KKEMP#
     C                   ENDIF                                                  ##XEMP=Y
     C***  END   ADD  ***  06/25/98  *********************************
     C*
      ***  Begin Chg  ***  12/06/05  **************************************
     C                   MOVE      *ON           *IN12
     C                   CLEAR                   EF
     C                   CLEAR                   CX
      ***  End   Chg  ***  12/06/05  **************************************
      *
      *** BEG ADD ***** 03/20/07 ******************************************
     C                   CALL      GETLIB        LIBPRM
     C     LIBNAM        IFNE      *BLANKS
     C                   MOVEL(P)  QCA(1)        CMDSTR
     C                   CAT(P)    LIBNAM:0      CMDSTR
     C                   CAT(P)    QCA(2):0      CMDSTR
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR           55
     C                   PARM                    LENGTH           15 5
     C                   ENDIF
     C                   OPEN      WMEMLQ
      *** END ADD ***** 03/20/07 ******************************************
      *
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C*** beg del ***** 06/23/06 *************************************
     C***  BEGIN CHG  ***  06/25/98  *********************************
     C***21***** KKCO#     READEDBRCDF                   95
     C**********           SELEC
     C*****BEGIN ADD  ***  05/10/99  *********************************
     C********** SSFND     WHNE *ZERO
     C********** SSEMP#    ANDNE*ZERO
     C********** SSDIV     ANDNE*BLANK
     C**********           MOVE SSDIV     KKDIV
     C***21***** KEYC      READEDBRCDFB                  95
     C*****Begin add  ***  12/06/05  **************************************
     C***22***** KEYC      READEDBRCDFB                  95
     C*****End** add  ***  12/06/05  **************************************
     C*****END** ADD  ***  05/10/99  *********************************
     C********** SSFND     WHNE *ZERO
     C********** SSEMP#    ANDNE*ZERO
     C***21***** KEYB      READEDBRCDFB                  95
     C*****Begin add  ***  12/06/05  **************************************
     C***22***** KEYB      READEDBRCDFB                  95
     C*****End** add  ***  12/06/05  **************************************
     C********** SSFND     WHNE *ZERO
     C***21***** KEYA      READEDBRCDFA                  95
     C*****Begin add  ***  12/06/05  **************************************
     C***22***** KEYA      READEDBRCDFB                  95
     C*****End** add  ***  12/06/05  **************************************
     C**********           OTHER
     C***21***** KKCO#     READEDBRCDF                   95
     C*****Begin add  ***  12/06/05  **************************************
     C***22***** KKCO#     READEDBRCDFB                  95
     C*****End** add  ***  12/06/05  **************************************
     C**********           ENDSL
     C*****END** CHG  ***  06/25/98  *********************************
     C*** end del ***** 06/23/06 *************************************
     C*
     C*** beg add ***** 06/23/06 *************************************
     C   21##CO#         READE     DBRCDF                                 95
     C   22##CO#         READE     DBRCDFB                                95
     C*** end add ***** 06/23/06 *************************************
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   EXSR      XFER
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C***********
     C*** BEG DEL ***** 06/23/06 *************************************
     C***  BEGIN CHG  ***  06/25/98  *********************************
     C***21***** KKCO#     REDPEDBRCDF                   95
     C***********          SELEC
     C*****BEGIN*ADD  ***  05/10/99  *********************************
     C***********SSFND     WHNE *ZERO
     C***********SSEMP#    ANDNE*ZERO
     C***********SSDIV     ANDNE*BLANK
     C***********          MOVE SSDIV     KKDIV
     C***21******KEYC      REDPEDBRCDFB                  95
     C*****END***ADD  ***  05/10/99  *********************************
     C***********SSFND     WHNE *ZERO
     C***********SSEMP#    ANDNE*ZERO
     C***21******KEYB      REDPEDBRCDFB                  95
     C***********SSFND     WHNE *ZERO
     C***21******KEYA      REDPEDBRCDFA                  95
     C***********          OTHER
     C***21******KKCO#     REDPEDBRCDF                   95
     C*****Begin*add  ***  12/06/05  **************************************
     C***22******          READPDBRCDFB                  95
     C*****End***add  ***  12/06/05  **************************************
     C***********          ENDSL
     C***  END   CHG  ***  06/25/98  *********************************
     C*** end del ***** 06/23/06 *************************************
     C*** BEG ADD ***** 06/23/06 *************************************
     C   21##CO#         READPE    DBRCDF                                 95
     C   22##CO#         READPE    DBRCDFB                                95
     C*** END ADD ***** 06/23/06 *************************************
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   EXSR      XFER
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C*** BEG DEL ***** 06/23/06 *************************************
     C***  BEGIN CHG  ***  06/25/98  ********************************
     C***21***** KEY1      SETGTDBRCDF               96
     C**********           SELEC
     C*****BEGIN ADD  ***  05/10/99  *********************************
     C********** SSFND     WHNE *ZERO
     C********** SSEMP#    ANDNE*ZERO
     C********** SSDIV     ANDNE*BLANK
     C**********           MOVE SSDIV     KKDIV
     C***21***** KEY1B     SETGTDBRCDFB              96
     C*****END** ADD  ***  05/10/99  *********************************
     C********** SSFND     WHNE *ZEROS
     C********** SSEMP#    ANDNE*ZEROS
     C***21***** KEY1B     SETGTDBRCDFB              96
     C********** SSFND     WHNE *ZEROS
     C***21***** KEY1A     SETGTDBRCDFA              96
     C**********           OTHER
     C***21***** KEY1      SETGTDBRCDF               96
     C**********           ENDSL
     C***  END   CHG  ***  06/25/98  ********************************
     C*** END DEL ***** 06/23/06 *************************************
     C*** BEG ADD ***** 06/23/06 *************************************
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDFB                            96
     C*** END ADD ***** 06/23/06 *************************************
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C*** BEG DEL ***** 06/23/06 *************************************
     C***  BEGIN CHG  ***  06/25/98  *********************************
     C***21***** KEY1      SETLLDBRCDF               96
     C*********            SELEC
     C*****BEGIN ADD  ***  05/10/99  *********************************
     C*********  SSFND     WHNE *ZERO
     C*********  SSEMP#    ANDNE*ZERO
     C*********  SSDIV     ANDNE*BLANK
     C*********            MOVE SSDIV     KKDIV
     C***21****  KEY1B     SETLLDBRCDFB              96
     C*****Begin add  ***  12/06/05  **************************************
     C***22****  KEY1B     SETLLDBRCDFB              96
     C*****End*  add  ***  12/06/05  **************************************
     C*****END*  ADD  ***  05/10/99  *********************************
     C*********  SSFND     WHNE *ZEROS
     C*********  SSEMP#    ANDNE*ZEROS
     C***21****  KEY1B     SETLLDBRCDFB              96
     C*********  SSFND     WHNE *ZEROS
     C***21****  KEY1A     SETLLDBRCDFA              96
     C*********            OTHER
     C***21****  KEY1      SETLLDBRCDF               96
     C*****Begin add  ***  12/06/05  **************************************
     C***22****  KEY1B     SETLLDBRCDFB              96
     C*****End*  add  ***  12/06/05  **************************************
     C*********            ENDSL
     C***  END   CHG  ***  06/25/98  ********************************
     C*** END DEL ***** 06/23/06 *************************************
     C*** BEG ADD ***** 06/23/06 *************************************
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDFB                            96
     C*** END ADD ***** 06/23/06 *************************************
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      25            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      25            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*  CHECK NAME AGAINST SEARCH STRING
     C*
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  QTNAME        U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
      ***  Begin add  ***  12/06/05  **************************************
     C*
     C     SSFDNM        IFNE      *BLANKS
     C     SSFEIN        ORNE      *ZEROS
     C     SSMAD1        ORNE      *BLANKS
     C     SSPAD1        ORNE      *BLANKS
     C     SSPHN#        ORNE      *BLANKS
     C     SSUWTR        ORNE      *BLANKS
     C     SSSEMP        ORNE      *BLANKS
     C     SSRSK#        ORNE      *ZEROS
     C     EMKY          CHAIN     PEM01                              60
      *
      *** BEG CHG ***** 03/20/07 **********************************
     C     *IN60         IFEQ      *ON
     C     EMKY          CHAIN     PEM01Q                             60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
      *** END CHG ***** 03/20/07 **********************************
     C*
     C*  CHECK EMPLOYERS NAME AGAINST SEARCH STRING
     C*
B01  C     SSFDNM        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  EMFDNM        U1PDSC
     C     LO:UP         XLATE(P)  SSFDNM        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSFDNM<>*BLANKS
     C*
     C*  CHECK EMPLOYERS PHYSICAL STREET ADDR 1 AGAINST SEARCH STRING
     C*
B01  C     SSPAD1        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  EMPAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSPAD1        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPAD1<>*BLANKS
     C*
     C*  CHECK EMPLOYERS PHONE # AGAINST SEARCH STRING
     C*
     C***  BEGIN CHG  ***  12/16/97  *********************************
B01  C********** SSPHN#    IFNE *ZEROS
B01  C     SSPHN#        IFNE      *BLANKS
     C***  END   CHG  ***  12/16/97  *********************************
     C*
     C                   MOVEL(P)  EMPHN#        U1PDSC
     C                   MOVEL(P)  SSPHN#        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPHN#<>*BLANKS
      *
      *  Subset on mailing address line 1
      *
     C     SSMAD1        IFNE      *BLANK
      *
     C     LO:UP         XLATE(P)  EMMAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSMAD1        U2PDSC
     C                   EXSR      STRING
      *
     C                   ENDIF
      ***  End   add  ***  12/06/05  **************************************
     C*
     C*  CHECK EMPLOYER FEIN AGAINST SUBSET FEIN
     C*
B01  C     SSFEIN        IFNE      *ZEROS
     C*
     C****       EMKY      CHAINPEM01                60
     C*
     C     EMFEIN        IFNE      SSFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  EMFEIN<>SSFEIN
     C*
E01  C                   ENDIF                                                  SSFEIN<>*ZEROS
      ***  Begin add  ***  12/06/05  **************************************
      *
      * Subset on underwriter's code
      *
     C     SSUWTR        IFNE      *BLANK
      *
     C     SSUWTR        IFNE      EMUWTR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF                                                  SSUWTR
      *
      * Subset on state risk id
      *
     C     SSSEMP        IFNE      *BLANKS
      *
     C     SSSEMP        IFNE      EMSEMP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF                                                  SSSEMP
      *
      * Subset on NCCI risk id
      *
     C     SSRSK#        IFNE      *ZEROS
      *
     C                   CLEAR                   CHRSK#
     C                   MOVEL     SSRSK#        CHRSK#           15
     C     CHRSK#        IFNE      EMRSK#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF                                                  SSRSK#
      *
     C                   ELSE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  *IN60
      ***  End   add  ***  12/06/05  **************************************
     C*
     C*  CHECK GROUP AGAINST SUBSET GROUP
     C*
B01  C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     QTFND
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  QTFND<>SSFND<>*ZEROS
     C*
     C*  CHECK AGENT # AGAINST SUBSET AGENT #
     C*
B01  C     SSAGT#        IFNE      *ZEROS
     C     SSAGT#        ANDNE     QTAGT#
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  QTAGT#<>SSAGT#
     C*
     C*  CHECK TYPE AGAINST SUBSET TYPE
     C*
B01  C     SSTYPE        IFNE      *BLANK
     C     SSTYPE        ANDNE     QTTYPE
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  QTTYPE<>SSTYPE
     C*
     C***  BEGIN ADD  ***  06/16/98  *********************************
B01  C     SSTYPE        IFEQ      'W'
     C     QTTYPE        ANDEQ     'T'
     C                   MOVE      *ON           MEETSS
E01  C                   ENDIF                                                  SSTYPE=W/QTTYPE=Y
     C***  END   ADD  ***  06/16/98  *********************************
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
     C*  CHECK STATUS AGAINST SUBSET STATUS
     C*
B01  C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     QTSTAT
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  QTSTAT<>SSSTAT
     C***  BEGIN ADD  ***  06/07/98  *********************************
     C*
     C*  CHECK QUOTE TYPE, DON'T DISPLAY TRANSFERED OUT ('X') RECORDS
     C*
B01  C     QTTYPE        IFEQ      'X'
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  QTTYPE=X
     C***  END   ADD  ***  06/07/98  *********************************
      ***  Begin Chg  ***  12/06/05  **************************************
      *** BEG DEL ***** 06/23/06 ******************************************
      *********
      ***Subset on state if primary or incidental coverage
      *********
     C*********  SSJRST    IFNE *BLANK
      *********
      ***Save*RRN to restore after this subset function is done. If any views
      ***are*added/changed, this if statement must be modified.
      *********
      *********
      ***Get*state reporting division
      *********
     C*********            Z-ADDEMFND     K1FND
     C*********            Z-ADDEMEMP#    K1EMP#
     C*********  ELKY1     SETLLWMEML
     C*********  ELKY1     READEWMEML                    60
     C*********  *IN60     DOWEQ*OFF
      *********
     C*********  EMSTRP    IFEQ 'Y'
     C*********            Z-ADDEMDIV     K1DIV
     C*********            LEAVE
     C*********            ENDIF                           EMSTRP = 'Y'
      *********
     C*********  ELKY1     READEWMEML                    60
     C*********            ENDDO                           *IN60
      *********
      ***Get*latest policy year of state reporting division
      *********
     C*********  ELKY2     SETGTPEL01
     C*********  ELKY2     REDPEPEL01                    60
     C*********  *IN60     IFEQ *OFF
     C*********            Z-ADDELFYR     K1FYR
     C*********            ENDIF
      *********
      ***Read*across all divisions that have same year
      *********
     C*********            MOVE *OFF      MEETSS
     C*********  ELKY3     SETLLPEL02
     C*********  ELKY3     READEPEL02                    60
     C*********  *IN60     DOWEQ*OFF
      *********
     C*********  RSKY      CHAINPRS01                60
     C*********  *IN60     IFEQ *OFF
      *********
     C*********  SSJRST    IFEQ RSJRST
     C*********            MOVE *ON       MEETSS
     C*********            LEAVE
     C*********            ENDIF                           SSJRST
      *********
     C*********            ENDIF                           *IN60
      *********
     C*********  ELKY3     READEPEL02                    60
     C*********            ENDDO                           *IN60 = *OFF
      *********
     C*********            ENDIF                           SSJRST
      *********
      *** END DEL ***** 06/23/06 ******************************************
      *** BEG ADD ***** 06/23/06 ************************************
     C     ELKY1         SETGT     PEL01
     C     EMKY          READPE    PEL01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELFYR         XXFYR             3 0
     C                   MOVEL(P)  ELRTCD        KKRTCD            3
     C                   ELSE
     C                   CLEAR                   XXFYR
     C                   CLEAR                   KKRTCD
     C                   ENDIF
      *
     C     SSJRST        IFNE      *BLANK
     C                   CLEAR                   RSJRST
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C     SSJRST        ANDNE     RSJRST
     C     *IN60         OREQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *** END ADD ***** 06/23/06 ******************************************
      ***  End   Chg  ***  12/06/05  **************************************
     C                   ENDSR                                                  SUBSET
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C*================================================================
     C* XFER - Load transfer type if entered
     C*================================================================
     C*
     C     XFER          BEGSR
     C*
     C     QTTYPE        IFEQ      'T'
     C     QTXFER        ANDNE     *BLANKS
     C                   MOVE      QTXFER        QTTYPE
     C                   ENDIF                                                  QTTYPE=T
     C*
     C                   ENDSR                                                  XFER
     C***  END   ADD  ***  11/01/00  *********************************
     C*
** QTE
PPENDING
QQUOTED
WWRITTEN
CCANCELLED
LLOST
DDECLINED
RREINSTATED
BBOUND
MBOUND W/MONEY
TTRANSFER IN
FRENEWAL PENDING                                             10/10/03
GRENEWAL QUOTE                                               10/10/03
USUBMITTED                                                   11/18/04
SCLOSED                                                      10/19/07
 *** Future Use
** Column headings
Employer Name              Employer   Div    Status       Grp  Agt# Agent Name
Grp  Employer   Div Employer Name            Status        Agt# Agent Name
Employer Name              Employer   Div    Grp  Agt# Agent Name  Prod Name
Grp  Employer   Div Employer Name           Agt# Agent Name  Prod Name
Employer Name              Employer   Div    Grp   Eff Date    Cx Date
Grp  Employer   Div Employer Name              Eff Date    Cx Date
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
209F9=Change Info           YYY
211F11=Change View          YYY
212F12=Cancel               NNY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
**  QCAEXEC TABLE
OVRDBF FILE(WMEMLQ) TOFILE(
/WMEMLQ)
DLTOVR FILE(WMEMLQ)
