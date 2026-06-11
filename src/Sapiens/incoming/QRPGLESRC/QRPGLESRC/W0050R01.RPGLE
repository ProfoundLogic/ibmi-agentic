     Hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0050R01 - DELETION OF EMPLOYER RELATED FILES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DELETE EMPLOYER RELATED FILES*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R02 - EMPLOYER MASTER INFORMATION                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 02/12/98  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/01/23 C:344 Support: PHW113173                             *
     H*                Don't delete contact records for renewal       *
     H*                quotes. Moved the delete of the WMECP Contacts *
     H*                file to the DLTLIV subroutine.                 *
     H*                                                               *
     H* 11/08/16 C:136 Support: 44084                                 *
     H*                Don't delete cross reference records for       *
     H*                renewal quotes                                 *
     H*                                                               *
     H* 06/08/15 C:308 Support: PHW34229                              *
     H*                Delete Cross Reference records from WZEXP      *
     H*                                                               *
     H* 03/10/15 C:136 Support: PHW32566                              *
     H*                Do not delete EFT file in quote                *
     H*                                                               *
     H* 05/24/11 C:189 SUPPORT: 76847                                 *
     H*                DELETE EFT ACCOUNT FILE RECORDS (WMEFP)        *
     H*                                                               *
     H* 03/04/11 C:33  Project: WC1851                                *
     H*                Ensure all detail related files are deleted    *
     H*                 with employer.                                *
     H*                                                               *
     H* 03/22/10 C:136 PROJECT: WC1838 (LARGE DEDUCTIBLES)            *
     H*                DELETE LARGE DEDUCTIBLE FILE (WMLDP)           *
     H*                                                               *
     H* 12/18/09 C:33  Seq#: WA046/70983                              *
     H*                Delete Quote Tracking when deleting employer.  *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*                                                               *
     H* 01/28/09 C:136 SUPPORT: WA045 66091                           *
     H*                DO NOT DELETE WMAUP AND WMNBP WHEN CALLED      *
     H*                FROM THE QUOTE ENVIRONMENT. MOVED THE DELETE   *
     H*                OF PRODUCTION ONLY FILES TO THE DLTLIV         *
     H*                SUBROUTINE.                                    *
     H*                                                               *
     H* 11/26/07 C:33  Seq#: WA109/61142                              *
     H*                Change method of deletion for SMARP.           *
     H*                                                               *
     H* 02/06/07 C:33  Seq#: WA045/54881                              *
     H*               -All employer related file deletions should     *
     H*                 happen in W0050R01X.                          *
     H*                                                               *
     H* 04/12/05 C:61  PROJECT WC1283 135WBW                          *
     H*                Delete records from Employee Payroll (WMEEP)   *
     H*                                                               *
     H* 03/25/04 C:61  PROJECT 114WHL                                 *
     H*                Delete records from the New Business Tracking  *
     H*                WMNBP.                                         *
     H*                                                               *
     H* 01/13/04 C:136 PROJECT: WC1116                                *
     H*                Delete records for Direct Debit History,       *
     H*                WMDBP.                                         *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1115                                *
     H*                Delete records for vendor history. WDVHP       *
     H*                                                               *
     H* 10/02/03 C:61  PROJECT WC577                                  *
     H*                Delete records from the Entity / Location      *
     H*                Detail WDLDP.                                  *
     H*                                                               *
     H* 09/19/03 C:61  SUPPORT WA121 38813                            *
     H*                Delete records from the Audit Tracking file.   *
     H*                WMAUP.                                         *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Delete records from the Employer Contact file. *
     H*                WMECP.                                         *
     H*                                                               *
     H* 08/02/00  C:90   PROJECT: CM027                               *
     H*                  DELETE FROM WMEWP (EXCLUDED WORKPLACES FILE) *
     H*                  DELETE FROM WXFMP1 (ASSESABLE FORMS FILE)    *
     H*                                                               *
     H* 02/15/00  C:33   PROJECT: CM045                               *
     H*                  DELETE FROM WMWVP (SUBROGATION FILE)         *
     H*                                                               *
     H* 12/22/99  C:35   PROJECT: SUPPORT WA101 #21097                *
     H*                  IF CALLED FROM THE QUOTE ENVIRONMENT, DO NOT *
     H*                  DELETE RECORDS FROM FILES WHICH DON'T EXIST  *
     H*                  IN THE QUOTE ENVIRONMENT.                    *
     H*****************************************************************
     F***  Begin Add  ***  06/08/15  **************************************
     FWZEXL     UF   E           K DISK
     FWZEXL1    UF   E           K DISK
     F                                     RENAME(PEX01:PEXL1)
     F***  End   Add  ***  06/08/19  **************************************
     F***  Begin Add  ***  12/18/09  **************************************
     FWAQTL7    UF   E           K DISK
     F                                     RENAME(PQT01:PQT07)
     F***  End   Add  ***  12/18/09  **************************************
     FWMOOL     UF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      ***** BEG CHG ******* 08/01/23 **********************************
     F*WMECL     UF   E           K DISK
     FWMECL     UF   E           K DISK    rename(pec01:pec00)
      ***** END CHG ******* 08/01/23 **********************************
      ***** END ADD ******* 01/31/02 **********************************
      ***** BEG ADD ******* 04/12/05 **********************************
     FWMEEL     UF   E           K DISK
      ***** END ADD ******* 04/12/05 **********************************
      ***** BEG ADD ******* 09/19/03 **********************************
     FWMAUL6    UF   E           K DISK
      ***** END ADD ******* 09/19/03 **********************************
      *
      * Entity / Location
     FWMALL     UF   E           K DISK
      ***** BEG ADD ******* 10/02/03 **********************************
      *
      * Entity / Location Detail
     FWDLDL     UF   E           K DISK
      ***** END ADD ******* 10/02/03 **********************************
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      * New Business Tracking
     FWMNBL     UF   E           K DISK
      ***** END ADD ******* 03/25/04 **********************************
     FWABLL1    UF   E           K DISK
     FWDEHL     UF   E           K DISK
     FWASRL5    UF   E           K DISK
     FWDCXL1    UF   E           K DISK
     FWDXHL2    UF   E           K DISK
     FWMSHL13   UF   E           K DISK
     FWXNAL     UF   E           K DISK
     FWXNAL12   UF   E           K DISK
     F                                     RENAME(PNA01:PNA02)
     FWXNEL     UF   E           K DISK
     FWXNHL     UF   E           K DISK
     FWXNXL     UF   E           K DISK
     FWDDSL1    UF   E           K DISK
     FWMA3L     UF   E           K DISK
     FWMDPL     UF   E           K DISK
     FWMMUL     UF   E           K DISK
     FWMRPL     UF   E           K DISK
     FWMCEL1    UF   E           K DISK
     F***  BEGIN CHG  ***  02/15/00  *********************************
     F**********WMCEL1S UF  E           K        DISK
     F**********  PCE01                             KRENAMEPCE01S
     FWMWVL1    UF   E           K DISK
     F***  END   CHG  ***  02/15/00  *********************************
     FWMENL4    UF   E           K DISK
     F**** BEGIN ADD **** 08/02/00 ************************************
     FWMEWL1    UF   E           K DISK
     FWXFML11   UF   E           K DISK
     F**** END   ADD **** 08/02/00 ************************************
     F**** BEGIN ADD **** 01/09/04 ************************************
     FWDVHL     UF   E           K DISK
     F**** END   ADD **** 01/09/04 ************************************
     F**** BEGIN ADD **** 01/13/04 ************************************
     FWMDBL     UF   E           K DISK
     F**** END   ADD **** 01/13/04 ************************************
     F***  Begin Add  ***  02/06/07  **************************************
     FWDELL     UF   E           K DISK
     FWMEPL     UF   E           K DISK
     FWMEAL     UF   E           K DISK
     F***  Begin Add  ***  11/19/09  **************************************
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F***  End   Add  ***  11/19/09  **************************************
     FWMAHL     UF   E           K DISK
     FSDCMLB    UF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
     F**** BEG ADD ***** 03/22/10 *************************************
     FWMLDL1    UF   E           K DISK
     F                                     RENAME(PLD01:PLD02)
     F**** END ADD ***** 03/22/10 *************************************
     F**** BEG ADD ***** 05/24/11 *************************************
     FWMEFL6    UF   E           K DISK
     F**** END ADD ***** 05/24/11 *************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F***  End   Add  ***  02/06/07  **************************************
      *** beg add ***** 11/08/16 ************************
     fwmeml     if   e           k disk    rename(pem01:pem00)
      *** end add ***** 11/08/16 ************************

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      **** BEGIN ADD **** 12/22/99 *********************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      **** END   ADD **** 12/22/99 *********************************************
     D***  Begin Add  ***  02/06/07  **************************************
     D CLTSPC          C                   CONST('W0050R01X')
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D***  Begin Add  ***  11/19/09  **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     IPCM03
     I              CMKEY                       C3KEY
     I              CMSEQ#                      C3SEQ#
     I              CMSTAT                      C3STAT
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL
     IPAR02
     I              ARKEY                       A2KEY
     I              ARSEQ#                      A2SEQ#
     I              ARSTAT                      A2STAT
     I              ARPGM                       A2PGM
     I              ARTIME                      A2TIME
     I              ARDT                        A2DT

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C*  DARTS REMARK DETAIL FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*  DARTS ADDITIONAL REMARK FILE
     C*
     C     A2KY          KLIST
     C                   KFLD                    C3KEY
     C                   KFLD                    C3SEQ#
     C***  End   Add  ***  02/06/07  **************************************
     C*
     C*  DELETION OF EMPLOYER RELATED FILES
     C*
     C     DLTKY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C*
     C     DLTKYN        KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
     C*
     C     DLTKY2        KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C*
      ***** BEG ADD ******* 03/25/04 **********************************
      *
     C     DLTKY3        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
      ***** END ADD ******* 03/25/04 **********************************
     C*
     C***  Begin Add  ***  12/18/09  **************************************
     C     DLTKY7        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    XXSTAT
     C***  End   Add  ***  12/18/09  **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@EMP#
     C                   PARM                    @@DIV
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   PARM                    @@QTE             1
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C     *LIKE         DEFINE    OOCO#         @@CO#
     C     *LIKE         DEFINE    OOFND         @@FND
     C     *LIKE         DEFINE    OOEMP#        @@EMP#
     C     *LIKE         DEFINE    OODIV         @@DIV
     C***  Begin Add  ***  12/18/09  **************************************
     C     *LIKE         DEFINE    QTSTAT        XXSTAT
     C***  End   Add  ***  12/18/09  **************************************
      **** BEGIN ADD **** 12/22/99 *********************************************
     C     *LIKE         DEFINE    @@QTE         XXQTE
      *
     C                   CLEAR                   XXQTE
      *
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@QTE         XXQTE
     C                   ENDIF                                                  Q1PRMS IFGE 6
      **** END   ADD **** 12/22/99 *********************************************
     C***  Begin Add  ***  11/19/09  **************************************
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C***  End   Add  ***  11/19/09  **************************************

      *** BEG ADD ***** 01/28/09 ***********************************************
      *
      *  DO NOT DELETE FILES THAT EXIST IN PRODUCTION ONLY IF CALLED FROM THE
      *  QUOTE ENVIRONMENT
      *
     C     XXQTE         IFNE      'Q'
     C                   EXSR      DLTLIV
     C                   ENDIF
      *** END ADD ***** 01/28/09 ***********************************************
     C*
     C***  Begin Add  ***  12/18/09  **************************************
     C*  Delete records from the Quote Tracking File
     C                   SELECT
     C     XXQTE         WHENEQ    'Q'
     C                   MOVEL     'Q'           XXSTAT
     C                   OTHER
     C                   MOVEL     'W'           XXSTAT
     C                   ENDSL
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY7        DELETE    PQT07                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  End   Add  ***  12/18/09  **************************************
     C***  Begin Add  ***  06/08/15  **************************************
      *** beg chg ***** 11/08/16 ***************************************
     C******IN60*        DOUEQ     *ON
     C*****DLTKY*        DELETE    PEX01                              60
     C***********        ENDDO                                                  *IN60=*ON
     C***********
     C******IN60*        DOUEQ     *ON
     C*****DLTKY*        DELETE    PEXL1                              60
     C***********        ENDDO                                                  *IN60=*ON
      *
      * Don't delete cross reference records
      *  if this is a renewal quote
      *
     c     dltKy         Chain     Pem00
     c                   If        xxQte = 'Q' and EMrqfl = *blank
      *
      * Records with prior employer
      *
     c     *in60         DouEq     *On
     c     dltKy         Delete    Pex01                              60
     c                   Enddo
      *
      * Records with subsequent employer
      *
     c     *in60         DouEq     *On
     c     dltKy         Delete    Pexl1                              60
     c                   Enddo

     c                   Endif
      *** end chg ***** 11/08/16 ***************************************
     C***  End   Add  ***  06/08/15  **************************************
     F*** BEG ADD ***** 03/22/10 **************************************
     C*
     C*    DELETE RECORDS FROM THE LARGE DEDUCTIBLE FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PLD02                              60
     C                   ENDDO                                                  *IN60=*ON
     F*** END ADD ***** 03/22/10 **************************************
     F*** BEG ADD ***** 05/24/11 **************************************
     F*** BEG DEL ***** 03/10/15 **************************************
     C***********
     C*****DELETE RECORDS FROM THE WMEFP (EFT ACCOUNT FILE)
     C***********
     C******IN60*        DOUEQ     *ON
     C*****DLTKY*        DELETE    PEF01                              60
     C***********        ENDDO                                                  *IN60=*ON
     F*** end DEL ***** 03/10/15 **************************************
     F*** END ADD ***** 05/24/11 **************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *  DELETE RECORDS FROM THE EMPLOYER CONTACTS FILE
      *
      ***** BEG DEL ******* 08/01/23 **********************************
     C*    *IN60         DOUEQ     *ON
     C*    DLTKY         DELETE    PEC01                              60
     C*                  ENDDO                                                  *IN60=*ON
      ***** END DEL ******* 08/01/23 **********************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C***  Begin Add  ***  02/06/07  **************************************
      *
      *  DELETE RECORDS FROM THE EMPLOYER PAYROLL FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEP01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
      *  DELETE RECORDS FROM THE EMPLOYER DETAIL FILE
      *
     C***  Begin Chg  ***  03/04/11  **************************************
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEL01                60
     C**********           ENDDO                           *IN60=*ON
     C     *LIKE         DEFINE    ELFYR         ##FYR
     C     DLTKY         SETLL     PEL01                              60
     C     *IN60         DOUEQ     *ON
     C     DLTKY         READE     PEL01                                  60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'W0051R01'                                    99
     C                   PARM      ELCO#         ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#
     C                   PARM      ELDIV         ##DIV
     C                   PARM      ELFYR         ##FYR
     C                   PARM      @@QTE         ##QTE
     C                   DELETE    PEL01
     C                   ENDIF
     C                   ENDDO
     C***  End   Chg  ***  03/04/11  **************************************
      *
      *  DELETE RECORDS FROM THE EMPLOYER ACCOUNTS RECEIVABLE FILE
      *
     C***  Begin Chg  ***  11/19/09  **************************************
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEA01                60
     C**********           ENDDO                           *IN60=*ON
     C     DLTKY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      HOLDAR
     C                   DELETE    PEA01
     C     DLTKY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60=*ON
     C***  End   Chg  ***  11/19/09  **************************************
      *
      *  DELETE RECORDS FROM THE EMPLOYER AGENT ASSIGNMENT FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PAH01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
      *  EXECUTE THIS CODE ONLY IF DARTS IS INSTALLED
      *
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C     SYSCIF        ANDEQ     'Y'
      *
      *  INACTIVATE DARTS REMARKS AND THE CORRESPONDING ADDT'L RMKS
      *  AND LOAD DIARY COMPLETION DATE TO DIARY ITEMS
      *
      *    ESTABLISH KEY
      *
     C                   CLEAR                   QCKEY
      *
     C                   Z-ADD     @@CO#         QCCO#
     C                   Z-ADD     @@FND         QCFND
     C                   Z-ADD     @@EMP#        QCEMP#
     C                   Z-ADD     @@DIV         QCDIV
     C                   MOVE      *BLANKS       QCQTE
      *
     C     XXQTE         IFEQ      'Q'
     C                   MOVE      'Q'           QCQTE
     C                   ENDIF                                                  @@QTE=Q
      *
      *  CHAIN TO DARTS REMARKS DETAIL FILE
      *
     C     C3KY          CHAIN     PCM03                              60
     C     *IN60         DOWEQ     *OFF
      *
      *  CHAIN TO ADDITIONAL REMARKS FILE
      *
     C***  Begin Chg  ***  11/26/07  **************************************
     C*          A2KY      CHAINPAR02                60
     C********** *IN60     DOWEQ*OFF
      **********
     C**********           DELETPAR02
      **********
     C********** A2KY      READEPAR02                    60
     C**********           ENDDO                           *IN60=*OFF/A2KY
     C     *IN60         DOUEQ     *ON
     C     A2KY          DELETE    PAR02                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  End   Chg  ***  11/26/07  **************************************
      *
     C                   DELETE    PCM03
      *
     C     C3KY          READE     PCM03                                  60
     C                   ENDDO                                                  *IN60=*OFF/C3KY
      *
      *  Delete DARTS Custom Group ID records
      *
     C     *IN60         DOUEQ     *ON
     C     C3KY          DELETE    PHD01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
     C     *IN60         DOUEQ     *ON
     C     C3KY          DELETE    PH101                              60
     C                   ENDDO                                                  *IN60=*ON
      *
     C                   ENDIF                                                  *IN60=*OFF/SYSCIF=Y
     C***  End   Add  ***  02/06/07  **************************************
      ***** BEG ADD ******* 04/12/05 **********************************
      *
      *  DELETE RECORDS FROM THE EMPLOYEE PAYROLL
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEE01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
      ***** END ADD ******* 04/12/05 **********************************
      ***** BEG ADD ******* 03/25/04 **********************************
      *** BEG DEL ***** 01/28/09 **************************************
      **********
      ***DELETE*RECORDS FROM New Business Tracking
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY3    DELETPNB01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ******END*ADD ******* 03/25/04 **********************************
      ******BEG*ADD ******* 09/19/03 **********************************
      **********
      ***DELETE*RECORDS FROM THE AUDIT TRACKING FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPAU01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      *** END DEL ***** 01/28/09 **************************************
      ***** END ADD ******* 09/19/03 **********************************
     C*
     C*  DELETE RECORDS FROM THE OWNER/OFFICER FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    POO01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE ENTITY/LOCATION FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PAL01                              60
     C                   ENDDO                                                  *IN60=*ON
      ***** BEG ADD ******* 10/02/03 **********************************
      *
      *  DELETE RECORDS FROM THE Entity / Location Detail
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PLD01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
      ***** END ADD ******* 10/02/03 **********************************
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER 3RD ADDRESS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PA301                              60
     C                   ENDDO                                                  *IN60=*ON
      *** BEG DEL ***** 01/28/09 ***********************************************
      *****BEGIN ADD **** 12/22/99 *********************************************
     C********** XXQTE     IFNE 'Q'
      *****END** ADD **** 12/22/99 *********************************************
     C**********
     C***DELETE*RECORDS FROM THE DEPARTMENT MASTER FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPDP01                60
     C**********           ENDDO                           *IN60=*ON
      *****BEGIN ADD **** 12/22/99 *********************************************
     C**********           ENDIF                           XXQTE IFNE 'Q'
      **** END   ADD **** 12/22/99 *********************************************
      *** END DEL ***** 01/28/09 ***********************************************
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER BILLING WORKFILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PBL01                              60
     C                   ENDDO                                                  *IN60=*ON
      **** BEGIN ADD **** 12/22/99 *********************************************
      *** BEG DEL ***** 01/28/09 ***********************************************
      ***********
     C***********XXQTE     IFNE 'Q'
      *****END***ADD **** 12/22/99 *********************************************
     C***********
     C***DELETE*RECORDS FROM THE EMPLOYER DETAIL HISTORY FILE
     C***********
     C************IN60     DOUEQ*ON
     C***********DLTKY     DELETPEH01                60
     C***********          ENDDO                           *IN60=*ON
      *****BEGIN*ADD **** 12/22/99 *********************************************
     C***********          ENDIF                           XXQTE IFNE 'Q'
      *** END DEL ***** 01/28/09 ***********************************************
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C*  DELETE RECORDS FROM THE S/R PAYROLL FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PSR01                              60
     C                   ENDDO                                                  *IN60=*ON
      **** BEGIN ADD **** 12/22/99 *********************************************
      *** BEG DEL ***** 01/28/09 ***********************************************
     C********** XXQTE     IFNE 'Q'
      *****END** ADD **** 12/22/99 *********************************************
     C**********
     C***DELETE*RECORDS FROM THE CANCELLATION DETAIL FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPCX01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***DELETE*RECORDS FROM THE CANCELLATION HISTORY FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPXH01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***DELETE*RECORDS FROM THE DISCOUNT DETAIL FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPDS01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***DELETE*RECORDS FROM THE CERTIFICATE HOLDERS FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPCE01                60
     C**********           ENDDO                           *IN60=*ON
      *****BEGIN ADD **** 12/22/99 *********************************************
     C**********           ENDIF                           XXQTE IFNE 'Q'
      *** END DEL ***** 01/28/09 ***********************************************
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C*  DELETE RECORDS FROM THE SUBROGATION FILE
     C*
     C     *IN60         DOUEQ     *ON
     C***  BEGIN CHG  ***  02/15/00  *********************************
     C********** DLTKY     DELETPCE01S               60
     C     DLTKY         DELETE    PWV01                              60
     C***  END   CHG  ***  02/15/00  *********************************
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE ENDORSEMENT FORMS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PMU01                              60
     C                   ENDDO                                                  *IN60=*ON
     C**** BEGIN ADD **** 08/02/00 ************************************
     C*
     C*  DELETE RECORDS FROM THE EXCLUDED WORKPLACES FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEW01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE ASSESABLE FORMS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PFM01                              60
     C                   ENDDO                                                  *IN60=*ON
     C**** END   ADD **** 08/02/00 ************************************
     C*
     C*  DELETE RECORDS FROM THE ENDORSEMENT FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEN01                              60
     C                   ENDDO                                                  *IN60=*ON
      **** BEGIN ADD **** 12/22/99 *********************************************
      *** BEG DEL ***** 01/28/09 ***********************************************
      **********
     C********** XXQTE     IFNE 'Q'
      *****END** ADD **** 12/22/99 *********************************************
     C**********
     C***DELETE*RECORDS FROM THE CARRIER/STATE REPORTING MASTER FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPRP01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***DELETE*RECORDS FROM THE NCCI SUBMISSION HISTORY FILE
     C**********
     C********** DLTKY     CHAINPSH01                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C*****DELETE RECORDS FROM THE NCCI ADDRESS FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKYN    DELETPNA01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C*****DELETE RECORDS FROM THE NCCI NAME/ADDRESS TRANSACTION FILE
     C**********
     C********** DLTKY2    CHAINPNA02                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** NADIV     IFEQ SHDIV
     C**********           DELETPNA02
     C**********           ENDIF                           NADVI=SHDIV
     C**********
     C********** DLTKY2    READEPNA02                    60
     C**********
     C**********           ENDDO                           *IN60=*OFF
     C**********
     C*****DELETE RECORDS FROM THE NCCI ENDORSEMENT FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKYN    DELETPNE01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C*****DELETE RECORDS FROM THE NCCI POLICY PAGE HEADER FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKYN    DELETPNH01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C*****DELETE RECORDS FROM THE NCCI EXPOSURE FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKYN    DELETPNX01                60
     C**********           ENDDO                           *IN60=*ON
     C**********
     C**********           DELETPSH01
     C**********
     C********** DLTKY     READEPSH01                    60
     C**********
     C**********           ENDDO                           *IN60=*ON
     C**********
      *****BEGIN ADD **** 12/22/99 *********************************************
     C**********           ENDIF                           XXQTE IFNE 'Q'
      *** END DEL ***** 01/28/09 ***********************************************
      **** END   ADD **** 12/22/99 *********************************************
     F**** BEGIN ADD **** 01/09/04 ************************************
     C*
     C*    DELETE RECORDS FROM THE VENDOR HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PVH01                              60
     C                   ENDDO                                                  *IN60=*ON
     F**** END   ADD **** 01/09/04 ************************************
     F**** BEGIN ADD **** 01/13/04 ************************************
     C*
     C*    DELETE RECORDS FROM DIRECT DEBIT HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PDB01                              60
     C                   ENDDO                                                  *IN60=*ON
     F**** END   ADD **** 01/13/04 ************************************
     C*
     C***  Begin Add  ***  02/06/07  **************************************
     C     *LIKE         DEFINE    @@CO#         ##CO#
     C     *LIKE         DEFINE    @@FND         ##FND
     C     *LIKE         DEFINE    @@EMP#        ##EMP#
     C     *LIKE         DEFINE    @@DIV         ##DIV
     C     *LIKE         DEFINE    @@QTE         ##QTE
     C*
     C                   CALL      CLTSPC                               99
     C                   PARM      @@CO#         ##CO#
     C                   PARM      @@FND         ##FND
     C                   PARM      @@EMP#        ##EMP#
     C                   PARM      @@DIV         ##DIV
     C                   PARM      XXQTE         ##QTE
     C***  End   Add  ***  02/06/07  **************************************
     C*
     C                   MOVE      *ON           *INLR
      *** BEG ADD ***** 01/28/09 ******************************************
      *====================================================================
      * DELETES FILES THAT EXIST IN PRODUCTION ONLY
      *====================================================================
     C     DLTLIV        BEGSR
     C*
     C*  DELETE RECORDS FROM THE DEPARTMENT MASTER FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PDP01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER DETAIL HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEH01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE CANCELLATION DETAIL FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PCX01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE CANCELLATION HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PXH01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE DISCOUNT DETAIL FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PDS01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE CERTIFICATE HOLDERS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PCE01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE CARRIER/STATE REPORTING MASTER FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PRP01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE NCCI SUBMISSION HISTORY FILE
     C*
     C     DLTKY         CHAIN     PSH01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*    DELETE RECORDS FROM THE NCCI ADDRESS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKYN        DELETE    PNA01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*    DELETE RECORDS FROM THE NCCI NAME/ADDRESS TRANSACTION FILE
     C*
     C     DLTKY2        CHAIN     PNA02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     NADIV         IFEQ      SHDIV
     C                   DELETE    PNA02
     C                   ENDIF                                                  NADVI=SHDIV
     C*
     C     DLTKY2        READE     PNA02                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*    DELETE RECORDS FROM THE NCCI ENDORSEMENT FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKYN        DELETE    PNE01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*    DELETE RECORDS FROM THE NCCI POLICY PAGE HEADER FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKYN        DELETE    PNH01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*    DELETE RECORDS FROM THE NCCI EXPOSURE FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKYN        DELETE    PNX01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C                   DELETE    PSH01
     C     DLTKY         READE     PSH01                                  60
     C                   ENDDO                                                  *IN60=*ON
      *
      *  DELETE RECORDS FROM New Business Tracking
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        DELETE    PNB01                              60
     C                   ENDDO                                                  *IN60=*ON
      *
      *  DELETE RECORDS FROM THE AUDIT TRACKING FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PAU01                              60
     C                   ENDDO                                                  *IN60=*ON
      *** BEG ADD ***** 03/10/15 **************************************
     C*
     C*    DELETE RECORDS FROM THE WMEFP (EFT ACCOUNT FILE)
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEF01                              60
     C                   ENDDO                                                  *IN60=*ON
      *** END ADD ***** 03/10/15 **************************************
      *** beg add ***** 11/08/16 ****************
      *
      *  Delete records for prior employer
      *
     c     *in60         DouEq     *On
     c     dltKy         Delete    Pex01                              60
     c                   Enddo
      *
      *  Delete records for subsequent employer
      *
     c     *in60         DouEq     *On
     c     dltKy         Delete    Pexl1                              60
     c                   Enddo
      *** end add ***** 11/08/16 ***************
      ***  Begin Add  ***  08/01/23  **************************************
       // Delete records from Contact records if this is a renewal quote
       Setll (@@Co#:@@Fnd:@@Emp#:@@Div) pec00;
       Dou %Eof(WMECL);
          Reade (@@Co#:@@Fnd:@@Emp#:@@Div) pec00;
             If %Eof(WMECL);
                Leave;
             Endif;
          Delete pec00;
       Enddo;
      ***  End   Add  ***  08/01/23  **************************************
      *
     C                   ENDSR
      *** END ADD ***** 01/28/09 ******************************************
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     EATRDT        IFLT      SYSDT
     C*
     C                   ADD       1             SYSTM
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1USER        EAUSER
     C                   Z-ADD     SYSDT         EADT
     C                   Z-ADD     SYSTM         EATIME
     C*
     C                   WRITE     PEA0#
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  11/19/09  **************************************
