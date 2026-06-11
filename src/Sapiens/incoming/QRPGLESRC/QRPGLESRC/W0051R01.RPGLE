     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R01 - DELETION OF EMPLOYER DETAIL RELATED FILES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DELETE EMPLOYER DETAIL       *
     H*                RELATED FILES.                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R02 - EMPLOYER DETAIL INFORMATION                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0051R01X - Deletion of Employer Detail Related Files - Clt*
     H*                                                               *
     H*  CREATED: 02/12/98  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/14/22 C:65  Support: 96366 (Amwins)                        *
     H*                Place a Setll before the Reade on deciding     *
     H*                when division is last one to ensure Reade      *
     H*                reads the correct record.                      *
     H*                                                               *
     H* 08/18/20 C:300 Support: 90415 (InHouse)                       *
     H*                New files for policy year delete               *
     H*                                                               *
     H* 05/22/15 C:279 Project: 114WKA                                *
     H*                Delete from Employer Detail File (WXELP)       *
     H*                                                               *
     H* 08/25/10 C:33  Project: WC1851                                *
     H*                Delete from Fee Information file               *
     H*                                                               *
     H* 03/22/10 C:136 PROJECT: WC1838 (LARGE DEDUCTIBLES)            *
     H*                DELETE LARGE DEDUCTIBLE FILE (WMLDP)           *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*                                                               *
     H* 02/14/07 C:33  Seq#: WA045/54881                              *
     H*               -Other files found needing deletion.            *
     H*               -Use W0051R01X to delete client specific files. *
     H*                                                               *
     H* 02/06/07 C:33  Seq#: WA045/54881                              *
     H*               -If the file only exists in Live, do not delete *
     H*                 when in Quote.                                *
     H*                                                               *
     H* 10/17/05 C:156 PROJECT: 114WKY                                *
     H*                Delete Scheduled Rating Informaion(WMRFP)      *
     H*                                                               *
     H* 04/12/05 C:61  PROJECT WC1283 135WBW                          *
     H*                Delete records from Employee Payroll (WMEEP)   *
     H*                                                               *
     H* 03/25/04 C:61  PROJECT 114WHL                                 *
     H*                Delete Renewal Tracking WDRTP records.         *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1115                                *
     H*                Delete records for vendor history. WDVHP       *
     H*                                                               *
     H* 09/19/03 C:61  SUPPORT WA121 38813                            *
     H*                Delete records from the Audit Tracking file.   *
     H*                WMAUP.                                         *
     H*                                                               *
     H* 02/21/02 C:61  SUPPORT WA099 31323                            *
     H*                Delete Additional Entity/Location records.     *
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
     FWMEPL     UF   E           K DISK
     FWMAHL     UF   E           K DISK
     FWMEAL     UF   E           K DISK
     F***  Begin Add  ***  11/19/09  **************************************
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F***  End   Add  ***  11/19/09  **************************************
     FWABLL     UF   E           K DISK
     FWDEHL     UF   E           K DISK
     FWDDSL     UF   E           K DISK
     F                                     RENAME(PDS01:PDS03)
     FWMMUL     UF   E           K DISK
     FWMRPL     UF   E           K DISK
     FWMCEL     UF   E           K DISK
     F***  BEGIN CHG  ***  02/15/00  *********************************
     F**********WMCEL1S UF  E           K        DISK
     F**********  PCE01                             KRENAMEPCE01S
     FWMWVL1    UF   E           K DISK
     F***  END   CHG  ***  02/15/00  *********************************
     FWMENL4    UF   E           K DISK
     FWASRL2    UF   E           K DISK
     FWDCXL2    UF   E           K DISK
     FWDXHL2    UF   E           K DISK
     FWMSHL13   UF   E           K DISK
     FWXNAL     UF   E           K DISK
     FWXNAL11   UF   E           K DISK
     F                                     RENAME(PNA01:PNA02)
     FWXNEL     UF   E           K DISK
     FWXNHL     UF   E           K DISK
     FWXNXL     UF   E           K DISK
     F**** BEGIN ADD **** 08/02/00 ************************************
     FWMEWL1    UF   E           K DISK
     FWXFML11   UF   E           K DISK
     F**** END   ADD **** 08/02/00 ************************************
     F***  Begin Add  ***  02/14/07  **************************************
     FWMCCL     UF   E           K DISK
     F                                     RENAME(PCC01:PCC00)
     FWDERL     UF   E           K DISK
     F                                     RENAME(PER01:PER00)
     FWMMBL5    UF   E           K DISK
     F                                     RENAME(PMB01:PMB05)
     FWDXSL     UF   E           K DISK
     F                                     RENAME(PXS01:PXS00)
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL08)
     F***  End   Add  ***  02/14/07  **************************************
      ***** BEG ADD ******* 02/21/02 **********************************
      *
      * Additional Entity/Location
     FWDLDL1    UF   E           K DISK
      ***** END ADD ******* 02/21/02 **********************************
      ***** BEG ADD ******* 04/12/05 **********************************
      *
      *  Employee Payroll
     FWMEEL     UF   E           K DISK
      ***** END ADD ******* 04/12/05 **********************************
      ***** BEG ADD ******* 09/19/03 **********************************
      *
      * Audit Tracking
     FWMAUL6    UF   E           K DISK
      ***** END ADD ******* 09/19/03 **********************************
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      * Renewal Tracking
     FWDRTL     UF   E           K DISK
      ***** END ADD ******* 03/25/04 **********************************
     F**** BEGIN ADD **** 01/09/04 ************************************
     FWDVHL     UF   E           K DISK
     F**** END   ADD **** 01/09/04 ************************************
     F**** BEGIN ADD **** 10/17/05 ************************************
     FWMRFL     UF   E           K DISK
     F**** END   ADD **** 10/17/05 ************************************
     F**** BEG ADD ***** 03/22/10 *************************************
     FWMLDL1    UF   E           K DISK
     F                                     RENAME(PLD01:PLD02)
     F**** END ADD ***** 03/22/10 *************************************
     F***  Begin Add  ***  08/25/10  **************************************
     FWDEFL1    UF   E           K DISK
     F***  End   Add  ***  08/25/10  **************************************
      ***** beg add ****** 05/22/15 **************************************
     fwxell     uf   e           k disk    rename(pel01:pxel00)
      ***** end add ****** 05/22/15 **************************************
      ***** beg add ****** 08/18/20 **************************************
     fwdill     uf   e           k disk    rename(pil01:pxil00)
     fwdmpl     uf   e           k disk    rename(pmp01:pxmp00)
     fwdppl     uf   e           k disk    rename(ppp01:pxpp00)
     fwdshl     uf   e           k disk    rename(ps101:pxs100)
      ***** end add ****** 08/18/20 **************************************

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      **** BEGIN ADD **** 12/22/99 *********************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      **** END   ADD **** 12/22/99 *********************************************
     D*
     D***  Begin Add  ***  02/14/07  **************************************
     D CLTSPC          C                   CONST('W0051R01X')
     D***  End   Add  ***  02/14/07  **************************************
     D***  Begin Add  ***  11/19/09  **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C*  DELETION OF POLICY YEAR RELATED FILES
     C*
     C     DLTKY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    @@FYR
     C*
     C     DLTKY2        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C*
     C     DLTKYN        KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
      ***** BEG ADD ******* 03/25/04 **********************************
      *
     C     DLTKY3        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@FYR
      *
      ***** END ADD ******* 03/25/04 **********************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@EMP#
     C                   PARM                    @@DIV
     C                   PARM                    @@FYR
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   PARM                    @@QTE             1
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C     *LIKE         DEFINE    AHCO#         @@CO#
     C     *LIKE         DEFINE    AHFND         @@FND
     C     *LIKE         DEFINE    AHEMP#        @@EMP#
     C     *LIKE         DEFINE    AHDIV         @@DIV
     C     *LIKE         DEFINE    AHFYR         @@FYR
      **** BEGIN ADD **** 12/22/99 *********************************************
     C     *LIKE         DEFINE    @@QTE         XXQTE
     C***  Begin Add  ***  02/14/07  **************************************
     C     *LIKE         DEFINE    @@CO#         ##CO#
     C     *LIKE         DEFINE    @@FND         ##FND
     C     *LIKE         DEFINE    @@EMP#        ##EMP#
     C     *LIKE         DEFINE    @@DIV         ##DIV
     C     *LIKE         DEFINE    @@FYR         ##FYR
     C     *LIKE         DEFINE    @@QTE         ##QTE
     C***  End   Add  ***  02/14/07  **************************************
      *
     C                   CLEAR                   XXQTE
      *
     C     Q1PRMS        IFGE      6
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
     C*
     C***  Begin Add  ***  02/06/07  **************************************
     C* Use this subroutine to delete from files that are only in the
     C*  LIVE environment
     C*
     C     XXQTE         IFNE      'Q'
     C                   EXSR      DLTLIV
     C                   ENDIF
     C***  End   Add  ***  02/06/07  **************************************
      ***** BEG ADD ******* 02/21/02 **********************************
      *
      *  Delete Additional Entity/Location Records
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PLD01                              60
     C                   ENDDO                                                  *IN60=*ON
      ***** END ADD ******* 02/21/02 **********************************
      ***** BEG ADD ******* 04/12/05 **********************************
      *
      *  Delete Employee Payroll for policy
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEE01                              60
     C                   ENDDO                                                  *IN60=*ON
      ***** END ADD ******* 04/12/05 **********************************
      ***** BEG ADD ******* 09/19/03 **********************************
     C***  Begin Del  ***  02/06/07  **************************************
      **********
      ***DELETE*RECORDS FROM THE AUDIT TRACKING FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPAU01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ******END*ADD ******* 09/19/03 **********************************
      ******BEG*ADD ******* 03/25/04 **********************************
      **********
      ***DELETE*RECORDS FROM THE RENEWAL TRACKING FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY3    DELETPRT01                60
     C**********           ENDDO                           *IN60=*ON
     C***  End   Del  ***  02/06/07  **************************************
      *
      ***** END ADD ******* 03/25/04 **********************************
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER PAYROLL FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEP01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER AGENT HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PAH01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
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
     C*
     C*  DELETE RECORDS FROM THE EMPLOYER BILLING WORKFILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PBL01                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  Begin Del  ***  02/06/07  **************************************
     C**********
     C***DELETE*RECORDS FROM THE EMPLOYER DETAIL HISTORY FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEH01                60
     C**********           ENDDO                           *IN60=*ON
     C***  End   Del  ***  02/06/07  **************************************
     C*
     C*  DELETE RECORDS FROM THE S/R PAYROLL FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PSR01                              60
     C                   ENDDO                                                  *IN60=*ON
      **** BEGIN ADD **** 12/22/99 *********************************************
     C     XXQTE         IFNE      'Q'
      **** END   ADD **** 12/22/99 *********************************************
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
     C     DLTKY2        DELETE    PDS03                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  DELETE RECORDS FROM THE CERTIFICATE HOLDERS FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PCE01                              60
     C                   ENDDO                                                  *IN60=*ON
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   ENDIF                                                  XXQTE IFNE 'Q'
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C*  DELETE RECORDS FROM THE SUBROGATION FILE
     C*
     C     *IN60         DOUEQ     *ON
     C***  BEGIN CHG  ***  02/15/00  *********************************
     C********** DLTKY2    DELETPCE01S               60
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
     C***  Begin Del  ***  02/06/07  **************************************
     C**********
     C***DELETE*RECORDS FROM THE ENDORSEMENT FILE
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEN01                60
     C**********           ENDDO                           *IN60=*ON
     C***  End   Del  ***  02/06/07  **************************************
      **** BEGIN ADD **** 12/22/99 *********************************************
     C     XXQTE         IFNE      'Q'
      **** END   ADD **** 12/22/99 *********************************************
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
     C*
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
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PNA02                              60
     C                   ENDDO                                                  *IN60=*ON
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
     C*
     C     DLTKY         READE     PSH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   ENDIF                                                  XXQTE IFNE 'Q'
      **** END   ADD **** 12/22/99 *********************************************
     F**** BEGIN ADD **** 01/09/04 ************************************
     C*
     C*    DELETE RECORDS FROM THE VENDOR HISTORY FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PVH01                              60
     C                   ENDDO                                                  *IN60=*ON
     F**** END   ADD **** 01/09/04 ************************************
     F**** BEGIN ADD **** 10/17/05 ************************************
     C*
     C*    DELETE RECORDS FROM THE SCHEDULED RATING FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    RFP01                              60
     C                   ENDDO                                                  *IN60=*ON
     F**** END   ADD **** 10/17/05 ************************************
     F*** BEG ADD ***** 03/22/10 **************************************
     C*
     C*    DELETE RECORDS FROM THE LARGE DEDUCTIBLE FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PLD02                              60
     C                   ENDDO                                                  *IN60=*ON
     F*** END ADD ***** 03/22/10 **************************************
     C***  Begin Add  ***  08/25/10  **************************************
     C*
     C* Delete records from the Fee Information File
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEF01                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  End   Add  ***  08/25/10  **************************************

      ******* beg add ******* 05/22/15 ************************************
      * Delete records from the Employer Detail File (WXELP)
     c     *in60         DouEq     *on
     c     DltKy         Delete    pxel00                             60
     c                   EndDo
      ******* end add ******* 05/22/15 ************************************
     C*
      ******* beg add ******* 08/18/20 ************************************
      * Delete records from the Employer Inc Limit Detail File (WDILP)
     c     *in60         DouEq     *on
     c     DltKy2        Delete    pxil00                             60
     c                   EndDo
      * Delete records from the Employer Premium Detail File (WDMPP)
     c     *in60         DouEq     *on
     c     DltKy2        Delete    pxmp00                             60
     c                   EndDo
      * Delete records from the Employer Premium Periods File (WDPPP)
     c     *in60         DouEq     *on
     c     DltKy2        Delete    pxpp00                             60
     c                   EndDo
      * Delete records from the NCCI Submission History Detail File (WDSHP)
     c     *in60         DouEq     *on
     c     DltKy         Delete    pxs100                             60
     c                   EndDo
      ******* end add ******* 08/18/20 ************************************
     C*
     C***  Begin Add  ***  02/14/07  **************************************
     C*** begin add ***  02/14/22  ****************************************
     C     DLTKY3        Setll     PEL08
     C*** end add *****  02/14/22  ****************************************
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        READE     PEL08                                  60
     C     @@DIV         IFNE      ELDIV
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN60         IFEQ      *ON
     C                   EXSR      LSTDIV
     C                   ENDIF
     C*
     C                   CALL      CLTSPC                               99
     C                   PARM      @@CO#         ##CO#
     C                   PARM      @@FND         ##FND
     C                   PARM      @@EMP#        ##EMP#
     C                   PARM      @@DIV         ##DIV
     C                   PARM      @@FYR         ##FYR
     C                   PARM      XXQTE         ##QTE
     C***  End   Add  ***  02/14/07  **************************************
     C*
     C                   MOVE      *ON           *INLR
     C*
     C***  Begin Add  ***  02/06/07  **************************************
     C*====================================================================
     C* LIVE Only deletions
     C*====================================================================
     C* Use this subroutine to delete from files that are only in the
     C*  LIVE environment
     C     DLTLIV        BEGSR
     C*
     C*  Delete records from the Emploeyr Detail History file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEH01                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  Begin Del  ***  02/14/07  **************************************
     C**********
     C***Delete*records from the Renewal Tracking file
     C**********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY3    DELETPRT01                60
     C**********           ENDDO                           *IN60=*ON
     C***  End   Del  ***  02/14/07  **************************************
     C*
     C*  Delete records from the Audit Tracking file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PAU01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  Delete records from the Endorsement file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    PEN01                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  Begin Add  ***  02/14/07  **************************************
     C*
     C*  Delete records from the Contractor Class file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PCC00                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  Delete records from the Employer EARS Factor file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PER00                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  Delete records from the Member Packets file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        DELETE    PMB05                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  End   Add  ***  02/14/07  **************************************
     C*
     C                   ENDSR
     C***  End   Add  ***  02/06/07  **************************************
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
     C*
     C***  Begin Add  ***  02/14/07  **************************************
     C*====================================================================
     C* Only process when last division being deleted
     C*====================================================================
     C* Use this subroutine to delete from files that are shared across
     C*  divisions
     C     LSTDIV        BEGSR
     C*
     C*  Delete records from the Renewal Tracking file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        DELETE    PRT01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C*  Delete records from the Excluded States Detail file
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        DELETE    PXS00                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C                   ENDSR
     C***  End   Add  ***  02/14/07  **************************************
