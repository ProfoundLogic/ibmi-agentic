     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0168R - CANCELLATION HISTORY ALL FYRS
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                as of CX issue date.                           *
     H*                                                               *
     H* 12/21/01 C:61  PROJECT WC577                                  *
     H*                Display current CX reason description.         *
     H*                                                               *
     H* 08/10/99  C:87  PROJECT: CM046                                *
     H*                 Load "Deletd Division" to cancellation reason *
     H*                 description if XHENFL='Y'.                    *
     H*                                                               *
     H* 02/24/99  C:87  PROJECT: SUPPORT TASB #NONE                   *
     H*                 Change to use the current date as opposed to  *
     H*                 the policy eff date when reading the canx     *
     H*                 reason code table file(WTCRP).                *
     H*                                                               *
     H* 11/10/98  C:87  PROJECT: WC852                                *
     H*                 Change to use WTCRL instead of WTABL to get   *
     H*                 the cancellation reason code record.          *
     H*                                                               *
     H* 08/07/98  C:78  SEQ#: DR095/12876                             *
     H*                 MOVE BLANKS TO QCQTE.                         *
     H*                                                               *
     H* 04/03/98  C:78  PROJECT: WC825                                *
     H*                 ALLOW ACCESS TO DARTS. F14 ADDED.             *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*****************************************************************
     FW0168F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDXHP     IF   E             DISK
     F                                     RENAME(PXH01:PFRCDF)
     FWDXHL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PXH01:DBRCDF)
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     FWTCRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F***  END   CHG  ***  11/10/98  **********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              HEADERS
      ******* BEGIN CHG ****** 04/03/98 *******************************
     D****************    OF      1   5  1   OFD    35   Options, Function
     D OF              S              1    DIM(6) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(6) ALT(OF)
      ******* END   CHG ****** 04/03/98 *******************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
      **** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      **** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      ******* BEGIN ADD ***** 04/03/98 ********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      ******* END   ADD ***** 04/03/98 ********************************
     D*
     D OPTNM           C                   CONST('Cancellation History-
     D                                      all Years')
     D OPTNM1          C                   CONST('Cancellation History')
     D DELETD          C                   CONST('*Deleted')
     D***  BEGIN ADD  ***  08/10/99  **********************************
     D DELDIV          C                   CONST('Deletd Division')
     D***  END   ADD  ***  08/10/99  **********************************
     D ACDPGM          C                   CONST('STDEFAULT')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(1)

     I*

     C*
     C     *LIKE         DEFINE    XHFYR         KKFYR
     C     *LIKE         DEFINE    XHREQ#        KKREQ#
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    EMNAME        X1NAME
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          SYSDT
     C     *LIKE         DEFINE    CRKEY         KKKEY
      *** BEGIN ADD *** 04/03/98 ***********************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *** END   ADD *** 04/03/98 ***********************************************
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    ELRTCD        KKRTCD
     C***  END   ADD  ***  11/10/98  **********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WMEMP
     C                   PARM                    @@RRN2            9 0          WDELP
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*  TABLE FILE
     C**********
     C********** TBKY      KLIST
     C**********           KFLD           EMCO#
     C**********           KFLD           KKKEY
     C*
     C*  Rate set code file
     C     RSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    KKRTCD
     C*
     C*  Cancellation reason code file
     C     CRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C*
     C*  Employer detail file
     C     ELKY          KLIST
     C                   KFLD                    XHCO#
     C                   KFLD                    XHFND
     C                   KFLD                    XHEMP#
     C                   KFLD                    XHDIV
     C                   KFLD                    XHFYR
     C***  END   CHG  ***  11/10/98  **********************************
     C*
     C*  EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     KEY2          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY3          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKREQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WMEMP
     C                   PARM                    @@RRN2            9 0          WDELP
     C*
     C                   EXSR      EMPFIL
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* EMPFIL - CHECK TO SEE WHICH LEVEL THE PROGRAM WILL ENTER,
     C*          EMPLOYER MASTER(WMEMP) OR EMPLOYER DETAIL(WDELP).
     C*===============================================================
     C*
     C     EMPFIL        BEGSR
     C*
     C     @@RRN         IFNE      *ZEROS
     C*
     C                   SETON                                        61        WMEMP
     C     @@RRN         CHAIN     PEM01                              99
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       61        WDELP
     C     @@RRN2        CHAIN     PEL01                              99
     C*
     C                   ENDIF                                                  @@RRN=*ON
     C*
     C                   ENDSR                                                  EMPFIL
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C   61              MOVE      *HIVAL        KKFYR
     C  N61              MOVE      ELFYR         KKFYR
     C                   MOVE      *HIVAL        KKREQ#
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C   61              MOVE      SCPST1        KKFYR
     C  N61              MOVE      SCYEAR        KKFYR
     C*
     C     SCPST2        IFNE      *BLANKS
     C*
     C                   MOVE      SCPST2        KKREQ#
     C*
     C                   ELSE
     C*
     C                   MOVE      *HIVAL        KKREQ#
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C   61              MOVE      SCYR          KKFYR
     C  N61              MOVE      SCYEAR        KKFYR
     C                   MOVE      SCREQ#        KKREQ#
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     XHFYR         SCYR
     C                   Z-ADD     XHREQ#        SCREQ#
     C                   MOVEL     XHRVFL        SCRVFL
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     ELKY          CHAIN     PEL02                              60
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C***  BEGIN CHG  ***  02/24/99  **********************************
     C******************** MOVE ELPFDT    KKEFDT
     C                   MOVE      SYSDT         KKEFDT
     C***  END   CHG  ***  02/24/99  **********************************
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C                   MOVEL     XHRSN         KKKEY
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY      CHAINPCR01                99
     C******************
     C** 99 ************   MOVEL'*NONE'   SCRSN
     C* N99 ************   MOVELCRDESC    SCRSN
     C*
     C                   MOVE      *BLANKS       SCRSN
      ***** BEG ADD ******* 12/11/02 **********************************
     C                   Z-ADD     XHCDAT        KKEFDT
      ***** END ADD ******* 12/11/02 **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  99
      ***** BEG ADD ******* 12/21/01 **********************************
      *
     C     *IN99         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  99
     C                   ENDDO
      *
      ***** END ADD ******* 12/21/01 **********************************
     C*
     C  N99              MOVEL     CRDESC        SCRSN
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   CHG  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C     XHENFL        IFEQ      'Y'
     C                   MOVEL(P)  DELDIV        SCRSN
     C                   ENDIF
     C***  END   ADD  ***  08/10/99  **********************************
     C*
     C                   MOVEL     XHERR         SCERR
     C*
     C                   Z-ADD     XHCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   Z-ADD     XHCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C                   Z-ADD     XHRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
     C*
     C                   Z-ADD     XHREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCREDT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
      ***  BEGIN ADD  ***  04/03/98  *********************************
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      ******** BEGIN ADD ****** 08/07/98 ******************************
     C                   MOVE      *BLANKS       QCQTE
      ******** END   ADD ****** 08/07/98 ******************************
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C***  END   ADD  ***  04/03/98  *********************************
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
      ***** BEGIN ADD ***** 04/03/98 **********************************
     C     *INKN         CASEQ     *ON           KNDO
      ***** END   ADD ***** 04/03/98 **********************************
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C     *IN61         IFEQ      *ON
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  OPTNM1        ##DATA
     C*
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRD4
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     *IN61         IFEQ      *ON
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVEL     EMNAME        X1NAME
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C                   Z-ADD     ELFYR         SCYR
     C                   Z-ADD     ELFYR         SCYEAR
     C*
     C     EMKY          CHAIN     PEM02                              99
     C*
     C                   MOVEL     EMNAME        X1NAME
     C*
     C                   ENDIF                                                  *IN61=0
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      SCEMP#        XXEMP#
     C                   PARM      SCDIV         XXDIV
     C                   PARM      X1NAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
      **** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCSCDT
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCSCDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVEL     'D'           ##MODE            1
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   61KEY1          READE     DBRCDF                                 95
     C  N61KEY2          READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   61KEY1          READPE    DBRCDF                                 95
     C  N61KEY2          READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C     KEY3          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C     KEY3          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR                                                  SUBSET
     C*
     C*===***===***===***===***===***===***===***===***===***===***===
** Headers - HD
 Yr  Req#  Rev Cancel reason   Err   Issued    Effective   Issued    Effective
** Description--------------MISN
203F3=Exit                  YYN
205F5=Refresh               YYN
211F11=Change View          NNN
212F12=Cancel               YYN
214F14=Darts                YYN
3
