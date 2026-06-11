     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  PROGRAM SUBROUTINE CR002R - BUILD EMPLOYER NAME PROGRAM
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF PROGRAM AND
     H*  DARTS KEY; RETRIEVE EMPLOYER NAME FROM WORK COMP FILE AND
     H*  RETURN CLAIMANT NAME TO SRKCVT PGM.
     H*****************************************************************
     H*  PROGRAMMER: 41                                               *
     H*  CREATED:    01/15/96             REQ/PRJ: 15/DR013           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     F***
      * ** C:002                                                              **
      * ** DR214                                                              **
      * ** 03/03/07                                                          **
      *    Changes for idarts ADDTIONAL INFO FOR DISPLAY DTA1 DTA2       **
      * **                                                                    **
      * ** C:002                                                              **
      * ** DR214                                                              **
      * ** 10/24/06                                                           **
      *    Changes for idarts compatibility                                   **
      *** RETURN ADDITIONAL INFORMATION TO THE CALLER (2 79 CHR FLDS)     ******
     F*
     H*                                                               *
     H*  08/28/14  C:62   Redmine: 4596 (Inhouse)                     *
     H*                   Remove override.                            *
     H*                                                               *
     H*  05/01/06  C:33   Project: SY082                              *
     H*                   Prepare for PDSDEV.                         *
     H*                                                               *
     H*   3/27/06  C:062  51789                                       *
     H*                   CLOSE THE FILE.                             *
     H*                                                               *
     H*   2/19/06  C:002  VDARTS                                      *
     H*                   CHANGE PROGRAM TO USE RETRN INSTEAD OF *INLR*
     H*                    ALSO, CHANGE SRNAME TO USE RETRN SRTERM='1'*
     H*                        ALSO, ONLY OPEN FILES ONCE
     H*                                                               *
     H*  10/27/04  C:062  Support: DR081/42858                        *
     H*                   Validate key                                *
     H*                                                               *
     H*  11/21/01  C:128  Support: OP109/29973                        *
     H*                   Inz all data structures that contain        *
     H*                   numeric data to prevent data decimal error. *
     H*                                                               *
     H*  11/20/00  C:128  Support: WC110/25090                        *
     H*                   Changed hardwired code to override WMEML    *
     H*                   to PDSQTE. The program now allows for a     *
     H*                   suffix to be added to the PDSQTE library.   *
     H*                                                               *
     H*  09/20/00  C:128  Support: MC091/24414                        *
     H*                   Added code to prevent opening files for     *
     H*                   applications that are no longer installed.  *
     H*                                                               *
     H*                                                               *
     H*  03/20/00  C:15   Support: DR048/22271                        *
     H*                   Added code to prevent data-decimal error.   *
     H*                                                               *
     H*  10/22/99  C:53   PROJ: MC145
     H*                   QUALIFY WCMS FILE W/NON-PDS WCMS FLAG.      *
     H*
     H*  06/22/98  C:33  SEQ#: DR095/12095                            *
     H*                  OVERRIDE WMEML IF IN QUOTE ENVIRONMENT       *
     H*****************************************************************
     F* WORK COMP EMPLOYER MASTER FILE    (PEM01)
     FWMEML     IF   E           K DISK    USROPN
     F*
     F* FEE SCHEDULE EMPLOYER MASTER FILE (PEM01)
     FFMEML     IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM02)

     D*
     D***  BEGIN ADD  ***  06/22/98  *********************************
     D***  Begin Chg  ***  11/20/00  *********************************
     D******        'OVRDBF FILE(WMEML) - C         QCMD1
     D******        'TOFILE(PDSQTE/WMEML)'
     D OVRIDE          C                   CONST('OVRDBF FILE(WMEML)')
     D***  Begin Chg  ***  05/01/06  **************************************
     D**********    ' TOFILE(PDSQTE'      C         TOFILE
     D TOFILE          C                   CONST(' TOFILE(')
     D***  End   Chg  ***  05/01/06  **************************************
     D FILENM          C                   CONST('/WMEML)')
     D***  End   Chg  ***  11/20/00  *********************************
     D QCMD2           C                   CONST('OVRDBF FILE(WMEML) -
     D                                     TOFILE(WCQ.BASFI/WME-
     D                                     ML)')
     D*** Begin Add *** 08/28/14 ******************************************
     D QCMD3           C                   CONST('DLTOVR FILE(WMEML)')
     D*** End   Add *** 08/28/14 ******************************************
     D***  END   ADD  ***  06/22/98  *********************************
     D***  Begin Add  ***  05/01/06  **************************************
     D RTVQTE          C                   CONST('RTVENVLIB')
     D***  End   Add  ***  05/01/06  **************************************
     D**** ADD 3/03/07 ***********************************************
     D*
     D XCUWTR          C                   CONST('Underwriter: ')
     D XCPSRV          C                   CONST('Policy Services: ')
     D XCQSTS          C                   CONST('Status: ')
     D XCAGT           C                   CONST('Agent: ')
     D*
     D**** END ADD 3/03/07 ***********************************************
     D*
     D* DATA STRUCTURE FOR KEY FOR WORK COMP
     D                 DS                  INZ
     D  XZKEY                  1     79
     D*** BEGIN ADD *** 10/27/04 **************************************
     D  XZEKEY                 1     20
     D*** END   ADD *** 10/27/04 **************************************
     D  XZCO#                  1      3  0
     D  XZFND                  4      6  0
     D  XZEMP#                 7     15  0
     D  XZDIV                 16     20  0
     D***  BEGIN ADD  ***  06/22/98  *********************************
     D  XZQTE                 40     40
     D***  END   ADD  ***  06/22/98  *********************************
     D*
     D* DATA STRUCTURE FOR KEY FOR FEE SCHEDULE
     D                 DS                  INZ
     D  X1KEY                  1     79
     D*** BEGIN ADD *** 10/27/04 **************************************
     D  X1EKEY                 1     25
     D*** END   ADD *** 10/27/04 **************************************
     D  X1CO#                  1      3  0
     D  X1SUB#                 4      8  0
     D  X1FND                  9     11  0
     D  X1EMP#                12     20  0
     D  X1DIV                 21     25  0
     D***  ADD 10/24/06 ****************************************************
     D*** Begin Add *** 08/28/14 ******************************************
     D  OVERRIDE       S               N   INZ(*OFF)
     D*** End   Add *** 08/28/14 ******************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     ****  END ADD 10/24/06 *******************************************

     C*****************************************************************
     C*        * * *  CALCULATION SPECIFICATIONS  * * *
     C*****************************************************************
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMSUB#        XXSUB#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*
     C* KEY LIST FOR THE WORK COMP EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* KEY LIST FOR THE FEE SCHEDULE EMPLOYER MASTER FILE
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C*** CHANGE 10/24/06 **************************************************
     C******* DEL ******   PARM           Q1PGM  10        PROGRAM NUMBER   BFR
     C                   PARM                    QQPGM            10            PROGRAM NUMBER   AFT
     C*** END CHANGE 10/24/06 **********************************************
     C                   PARM                    Q1KEY            79            DARTS' KEY
     C                   PARM                    Q1NAME           79            NAME
     I*** BEGIN ADD *** 10/24/06 **************************************
     C                   PARM                    Q1DTA1           79            DTA 1
     C                   PARM                    Q1DTA2           79            DTA 2
     I*** END   ADD *** 10/24/06 **************************************
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       XXPGM             1
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVE      *ZEROS        X1KEY
     C                   MOVE      *ZEROS        XZKEY
     C*** CHANGE 10/24/06 **************************************************
     C***** DEL ******     MOVELQ1PGM     XXPGM            BFR
     C                   MOVEL     QQPGM         XXPGM                          AFTER
     C*** END CHANGE 10/24/06 **********************************************
     C                   MOVE      *BLANKS       CMD
     C                   MOVE      *BLANKS       XXQCMD
     C*** Begin Add *** 08/28/14 ******************************************
     C                   EVAL      OVERRIDE = *OFF
     C*** End   Add *** 08/28/14 ******************************************
     C*
     C*** BEGIN ADD *** 03/20/00 **************************************
     C     Q1KEY         IFNE      *BLANKS
     C*** END ADD ***** 03/20/00 **************************************
     C*
     C*** BEG ADD *** 10/22/99 *************
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 10/22/99 *************
     C*
     C***  BEGIN ADD  ***  06/22/98  *********************************
     C     XXPGM         IFEQ      'W'
     C*
     C                   MOVEL     Q1KEY         XZKEY
     C*
     C     XZQTE         IFEQ      'Q'
     C*
     C                   CALL      'SACHKPDS'
     C                   PARM                    XXPDS             1
     C*
     C***  Begin Chg  ***  05/01/06  **************************************
     C********** XXPDS     IFEQ *ON
     C**********           MOVELQCMD2     XXQCMD    P
     C**********           ELSE
     I*****Begin Chg  ***  11/20/00  *********************************
     C**********           MOVELQCMD1     XXQCMD    P
     C**Call*system API to access client library suffix, if any.
     C**********
     C**********           CALL 'SA030R'
     C**********           PARM           SUFFIX  3        Lib suffix
     C**********           MOVELOVRIDE    CMD    55
     C**********           CAT  TOFILE:0  CMD
     C**********           CAT  SUFFIX:0  CMD
     C**********           CAT  FILENM:0  CMD
     C**********           MOVELCMD       XXQCMD    P
     I*****End** Chg  ***  11/20/00  *********************************
     C**********           ENDIF                           XXPDS=*ON
     C                   CALL      RTVQTE
     C                   PARM      '*OTHER'      ENV              10
     C                   PARM                    QTELIB           10
     C                   PARM                    MQTLIB           10
     C*
     C                   MOVEL(P)  OVRIDE        CMD              55
     C                   CAT       TOFILE:0      CMD
     C                   CAT       QTELIB:0      CMD
     C                   CAT       FILENM:0      CMD
     C*
     C                   MOVEL(P)  CMD           XXQCMD
     C***  End   Chg  ***  05/01/06  **************************************
     C*
     C                   CALL      'QCMDEXC'                              80
     C                   PARM                    XXQCMD           55
     C                   PARM      55            LENGTH           15 5
     C*** Begin Add *** 08/28/14 ******************************************
     C*
     C                   EVAL      OVERRIDE = *ON
     C*** End   Add *** 08/28/14 ******************************************
     C*
     C                   ENDIF                                                  XZQTE=Q
     C*
     C                   MOVE      *ZEROS        XZKEY
     C*
     C                   ENDIF                                                  XXPGM=W
     C***  END   ADD  ***  06/22/98  *********************************
     C*
     C* CHECK THE CALLING PROGRAM
     C* IF 1ST CHARACTER OF CALLING PROGRAM IS NOT A 'W' OPEN FS FILES
     C* ELSE OPEN WC FILES AND EXECUTE APPLICABLE SUBROUTINE
     C*
B01  C     XXPGM         IFNE      'W'
     C*** BEGIN ADD *** 09/20/00 ***************************************
     C*
     C*  Installed applications API
     C*
     C                   MOVE      'FS'          X#APP             2
     C                   MOVE      *BLANK        X#RTN             1
     C                   CALL      'DA400R'
     C                   PARM                    X#APP
     C                   PARM                    X#RTN
B02  C     X#RTN         IFEQ      '0'                                          No App Inst
     C                   GOTO      SKIP
E02  C                   ENDIF
     C*** END ADD *** 09/20/00 *****************************************
     C*** ADD 2/19/06 *************************************************
     C     OPENF         IFEQ      *BLANKS
     C*** END ADD 2/19/06 *************************************************
     C                   OPEN      FMEML
     C*** ADD 2/19/06 *************************************************
     C                   MOVE      '1'           OPENF             1
     C                   ENDIF
     C*** END ADD 2/19/06 *************************************************
     C                   EXSR      SRFS
     C*
X01  C                   ELSE
B02  C     ##WCIF        IFEQ      'Y'                                          ADD 10/22/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 10/22/99
     C*** ADD 2/19/06 *************************************************
     C     OPENW         IFEQ      *BLANKS
     C*** END ADD 2/19/06 *************************************************
     C                   OPEN      WMEML
     C*** ADD 2/19/06 *************************************************
     C                   MOVE      '1'           OPENW             1
     C                   ENDIF
     C*** END ADD 2/19/06 *************************************************
     C                   EXSR      SRWC
E02  C                   ENDIF                                                  ADD 10/22/99
E01  C                   END
     C*** BEGIN ADD *** 09/20/00 ***************************************
     C     SKIP          TAG
     C*** END   ADD *** 09/20/00 ***************************************
     C*
     C*** BEGIN ADD *** 03/20/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 03/20/00 **************************************
     C*
     C* END OF PROGRAM
     C****** ADD 02/19/06 *********************************************
B01  C     OPENW         IFEQ      '1'
     C                   CLOSE     WMEML
     C                   CLEAR                   OPENW
     C                   ENDIF
     C*** Begin Add *** 08/28/14 ******************************************
     C*
     C                   IF        OVERRIDE = *ON
     C                   CALL      'QCMDEXC'                              80
     C                   PARM      qcmd3         xxcmd3           18
     C                   PARM      18            LENGTH           15 5
     C                   ENDIF
     C*** End   Add *** 08/28/14 ******************************************
     C*
     C                   RETURN
     C***END ADD 02/19/06 *********************************************
     C*
     C                   MOVE      '1'           *INLR
     C*******************************************************************
     C* SRFS SUBROUTINE - PROCESS FEE SCHEDULE FILES                    *
     C*******************************************************************
     CSR   SRFS          BEGSR
     C*
     C                   MOVEL     Q1KEY         X1KEY
     C*** BEGIN ADD *** 10/27/04 **************************************
     C                   TESTN                   X1EKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 10/27/04 **************************************
     C*
     C* CHAIN TO EMPLOYER MASTER FILE TO GET NAME
     C*
     C                   Z-ADD     X1CO#         XXCO#
     C                   Z-ADD     X1SUB#        XXSUB#
     C                   Z-ADD     X1FND         XXFND
     C                   Z-ADD     X1EMP#        XXEMP#
     C                   Z-ADD     X1DIV         XXDIV
     C*
     C     EMKY1         CHAIN     PEM02                              60
     C     *IN60         IFEQ      '0'
     C*
     C* FORMAT EMPLOYER NAME
     C*
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM           15
     C                   MOVEL     EMNAME        SRLNAM
     C****** CHG 02/19/06 *********************************************
     C***** RMV *******    MOVEL' '       SRTERM  1
     C                   MOVEL     '1'           SRTERM            1
     C** END CHG 02/19/06 *********************************************
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C*
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     SRLNAM        Q1NAME
     C                   END                                                    *IN60 END
     C*** BEGIN ADD *** 10/27/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   ENDIF
     C*** END   ADD *** 10/27/04 **************************************
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     Q1PRMS        IFGT      3
     **
     C***  THE ADDITIONAL FIELDS FOR DISPLAY WERE REQUESTED (DTA1/DTA2)
     C***    THESE CAN BE FILLED WITH TEXT AND DATA FROM EMPL FILES   INCLUDING
      ***    INFO NOT SURRENTLY DISPLAYED (IF APPLICABLE) SEE CR001R FOR EXAMPLE
      ***    SIMPLY CONCAT THE APPROPRIATE TEXT AND DATA INTO DTA1 AND/OR
      ***    DTA2 AND IT WILL BE DISPLAYED AS PART OF THE HEADING FOR IDARTS
      ***
     **  ADD 03/03/07 ************************************************
     ** GW SUGGESTS THE FOLLOWING:
     **
     ** UNDERWRITER: XXX  POLICY SERVICES: YYY  STATUS: WRITTEN
     ** AGENCY:  WALL INSURANCE AGENCY
     **
     **                   EMUWTR - UNDERWRITER
     **                   EMPSRV - POLICY SVCS
     **                   SCANAM AND SCAGT# -  GET USING SUBROUTINE FROM W0050R
     **                   STATUS - BUILD USING W0050R ROUTINE
     **
     C                   CALL      'CR002C'                               99
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    XZQTE             1
     C                   PARM      *BLANKS       XXQTE            15
     C                   PARM      *BLANKS       SCANAM           60
     C                   PARM      *BLANKS       SCAGT#            5
     C                   PARM      *BLANKS       IRRCOD            1
     C*
     **
     C*
     C                   MOVEL     *BLANKS       Q1DTA1
     C*
     C     EMUWTR        IFNE      *BLANKS
     C                   CAT(P)    XCUWTR:0      Q1DTA1
     C                   CAT(P)    EMUWTR:1      Q1DTA1
     C                   CAT(P)    '  ':0        Q1DTA1
     C*
     C                   ENDIF
     **
     C     EMPSRV        IFNE      *BLANKS
     C                   CAT(P)    XCPSRV:2      Q1DTA1
     C                   CAT(P)    EMPSRV:1      Q1DTA1
     C                   CAT(P)    '  ':0        Q1DTA1
     C**
     C                   ENDIF
     **
     C     XXQTE         IFNE      *BLANKS
     C                   CAT(P)    XCQSTS:2      Q1DTA1
     C                   CAT(P)    XXQTE:1       Q1DTA1
     C**
     C                   ENDIF
     C**
     C                   MOVEL     *BLANKS       Q1DTA2
     **
     C*
     C     SCAGT#        IFNE      *BLANKS
     C     SCANAM        ORNE      *BLANKS
     **
     C*
     C                   CAT(P)    XCAGT:0       Q1DTA2
     **
     C     SCAGT#        IFNE      *BLANKS
     C                   CAT(P)    SCAGT#:1      Q1DTA2
     C                   ENDIF
     **
     C     SCANAM        IFNE      *BLANKS
     C                   CAT(P)    ' - ':1       Q1DTA2
     C                   CAT(P)    SCANAM:1      Q1DTA2
     C                   ENDIF
     **
     C*
     C                   ENDIF
     **
     **
     **  END ADD 03/03/07 ************************************************
      *
     C                   ENDIF
     C*** END   ADD *** 10/24/06 **************************************
     CSR                 ENDSR
     C*******************************************************************
     C* SRWC SUBROUTINE - PROCESS WORK COMP FILES                       *
     C*******************************************************************
     CSR   SRWC          BEGSR
     C*
     C                   MOVEL     Q1KEY         XZKEY
     C*** BEGIN ADD *** 10/27/04 **************************************
     C                   TESTN                   XZEKEY               99
     C     *IN99         IFEQ      *ON
     C*** END   ADD *** 10/27/04 **************************************
     C*
     C* CHAIN TO EMPLOYER MASTER FILE TO GET NAME
     C*
     C                   Z-ADD     XZCO#         XXCO#
     C                   Z-ADD     XZFND         XXFND
     C                   Z-ADD     XZEMP#        XXEMP#
     C                   Z-ADD     XZDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* FORMAT EMPLOYER NAME
     C*
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM           15
     C                   MOVEL     EMNAME        SRLNAM
     C****** CHG 02/19/06 *********************************************
     C***** RMV *******    MOVEL' '       SRTERM  1
     C                   MOVEL     '1'           SRTERM            1
     C** END CHG 02/19/06 *********************************************
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C*
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     SRLNAM        Q1NAME
     C                   END                                                    *IN60 END
     C*** BEGIN ADD *** 10/27/04 **************************************
     C                   ELSE
     C                   MOVEL(P)  Q1KEY         Q1NAME
     C                   ENDIF
     C*** END   ADD *** 10/27/04 **************************************
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     Q1PRMS        IFGT      3
     **
     C***  THE ADDITIONAL FIELDS FOR DISPLAY WERE REQUESTED (DTA1/DTA2)
     C***    THESE CAN BE FILLED WITH TEXT AND DATA FROM EMPL FILES   INCLUDING
      ***    INFO NOT SURRENTLY DISPLAYED (IF APPLICABLE) SEE CR001R FOR EXAMPLE
      ***    SIMPLY CONCAT THE APPROPRIATE TEXT AND DATA INTO DTA1 AND/OR
      ***    DTA2 AND IT WILL BE DISPLAYED AS PART OF THE HEADING FOR IDARTS
      ***
     **  ADD 03/03/07 ************************************************
     ** GW SUGGESTS THE FOLLOWING:
     **
     ** UNDERWRITER: XXX  POLICY SERVICES: YYY  STATUS: WRITTEN
     ** AGENCY:  WALL INSURANCE AGENCY
     **
     **                   EMUWTR - UNDERWRITER
     **                   EMPSRV - POLICY SVCS
     **                   SCANAM AND SCAGT# -  GET USING SUBROUTINE FROM W0050R
     **                   STATUS - BUILD USING W0050R ROUTINE
     **
     C                   CALL      'CR002C'                               99
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    XZQTE             1
     C                   PARM      *BLANKS       XXQTE            15
     C                   PARM      *BLANKS       SCANAM           60
     C                   PARM      *BLANKS       SCAGT#            5
     C                   PARM      *BLANKS       IRRCOD            1
     C*
     **
     C*
     C                   MOVEL     *BLANKS       Q1DTA1
     C*
     C     EMUWTR        IFNE      *BLANKS
     C                   CAT(P)    XCUWTR:0      Q1DTA1
     C                   CAT(P)    EMUWTR:1      Q1DTA1
     C                   CAT(P)    '  ':0        Q1DTA1
     C*
     C                   ENDIF
     **
     C     EMPSRV        IFNE      *BLANKS
     C                   CAT(P)    XCPSRV:2      Q1DTA1
     C                   CAT(P)    EMPSRV:1      Q1DTA1
     C                   CAT(P)    '  ':0        Q1DTA1
     C**
     C                   ENDIF
     **
     C     XXQTE         IFNE      *BLANKS
     C                   CAT(P)    XCQSTS:2      Q1DTA1
     C                   CAT(P)    XXQTE:1       Q1DTA1
     C**
     C                   ENDIF
     C**
     C                   MOVEL     *BLANKS       Q1DTA2
     **
     C*
     C     SCAGT#        IFNE      *BLANKS
     C     SCANAM        ORNE      *BLANKS
     **
     C*
     C                   CAT(P)    XCAGT:0       Q1DTA2
     **
     C     SCAGT#        IFNE      *BLANKS
     C                   CAT(P)    SCAGT#:1      Q1DTA2
     C                   ENDIF
     **
     C     SCANAM        IFNE      *BLANKS
     C                   CAT(P)    ' - ':1       Q1DTA2
     C                   CAT(P)    SCANAM:1      Q1DTA2
     C                   ENDIF
     **
     C*
     C                   ENDIF
     **
     **
     **  END ADD 03/03/07 ************************************************
      *
     C                   ENDIF
     C*** END   ADD *** 10/24/06 **************************************
     CSR                 ENDSR
