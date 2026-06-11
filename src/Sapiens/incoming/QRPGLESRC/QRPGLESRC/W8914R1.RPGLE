     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8914R - WORK FILE BUILD - A/R FILE - WHARP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - A/R FILE                                   *
     H*                                                               *
     H* CREATED: 02/06/01  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 01/05/05  C:132 PROJECT: CI025                                *
     H*              - Change to include all billing and payment      *
     H*                transactions in the future, instead of using   *
     H*                input parm date as cutoff date. (Like 91DKC)   *
     H*                DOES NOT REQUIRE ANY CODING CHANGES            *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHARP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 04/03/03  C:132 PROJECT: WC924  SEQ#: 36720                   *
     H*              - Change to PUT EAARTY (Trans Type) of "N" into  *
     H*                Billing coding instead of Payment coding       *
     H*                                                               *
     H* 05/28/02  C:132 PROJECT: WC924                                *
     H*               - Change to use only EATRDT (Trans date) on     *
     H*                 Compare to bypass processing                  *
     H*                                                               *
     H* 05/08/02  C:132 PROJECT: CI091 SEQ: 32340                     *
     H*               - Change position for default of Is A Direct    *
     H*                 Bill Policy.                                  *
     H*                                                               *
     H* 12/04/01  C:132 PROJECT WC924                                 *
     H*               - CORRECT AMT FOR REFUNDS SINCE USING WMEAP     *
     H*                 RATHER THAN WFARP
     H*                                                               *
     H* 11/13/01  C:132 PROJECT WC924                                 *
     H*               - Change: If WMEAP/EAPODT is Zero, use EATRDT   *
     H*                                                               *
     H* 10/09/01  C:132 PROJECT WC924                                 *
     H*               - Change: If WMEAP/EAARTY is a Payment type     *
     H*                 make it negative by MUlT by a -1              *
     H*                 FIX WHARP ARTRDT & ARPODT                     *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H*  09/17/01 C:132  Project:  WC924                              *
     H*           Add WMEAL for Input instead of WFARP file. Input    *
     H*           Parm will now be the Last Extract Date              *
     H*           Check EATRDT for certain EAARTY codes and EAPODT
     H*           for Different EAARTY codes.
     H*
     H*     08/02/01  C:003  CHG MULT ON "REFUND"
     H*                      CLEAR UNIQUE KEY FIELD (A2KEY)
     H*
     H*     07/11/01  C:132  Add ARARCD & ARARTY to Unique Key
     H*               C:003  LOADED DEFAULT VALUE FOR DIRECT BILL
     H*
     H*     04/05/01  C:58   Add input ocde D = bill.           %
     H*
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * A/R Detail Report Workfile
     F****WFARP   IF  E           K        DISK
      *
      * Employer A/R File by CO#, FND, EMP#
     FWMEAL     IF   E           K DISK
      *
      * Agent History
     FWMAHL     IF   E           K DISK
      *
      * Agent Rate
     FWMARL     IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)
      *
      * FUND
     FWMFNL     IF   E           K DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
      * Exclude Employers file
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
     F**** End Add ******  07/22/05  **************************************
      *
      * Output - Data Warehouse - WHARP
     FWHARP     IF A E             DISK
     F                                     RENAME(PAR01:PAR02)
      *
      *

     D                 DS
     D  XXKEY                  1     36
     D  EACO#                  1      3  0
     D  EAFND                  4      6  0
     D  EAEMP#                 7     15  0
     D  EADIV                 16     20  0
     D  EAFYR                 21     23  0
     D  EATRDT                24     30  0
     D  EATIME                31     36  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1YY                   5      8
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     IPAR02
     I              ARPOL#                      A2POL#
     I              ARDIV                       A2DIV
     I              ARTRTP                      A2TRTP
     I              ARTRDS                      A2TRDS
     I              ARAMT                       A2AMT
     I              ARTRDT                      A2TRDT
     I              ARCHK#                      A2CHK#
     I              ARNINV                      A2NINV
     I              ARFND                       A2FND
     I              ARDBIL                      A2DBIL
     I              ARPODT                      A2PODT
     I              ARKEY                       A2KEY
     I              ARAODT                      A2AODT
      *
     IPAR03
     I              ARCO#                       A3CO#
     I              ARFND                       A3FND
     I              ARRT#                       A3RT#
     I              AREFDT                      A3EFDT
     I              ARNWRT                      A3NWRT
     I              ARRNRT                      A3RNRT
     I              ARSTAT                      A3STAT
     I              ARUSER                      A3USER
     I              ARPGM                       A3PGM
     I              ARTIME                      A3TIME
     I              ARDT                        A3DT
     I              ARDESC                      A3DESC
     I              ARSBRT                      A3SBRT
     I              ARDDP                       A3DDP
     I              ARPLAN                      A3PLAN
     I              ARDBPP                      A3DBPP

     C*
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  READ WORK FILE
      *
     C     EAKY          SETLL     PEA01
     C                   READ      PEA01                                  60
     C     *IN60         DOWEQ     '0'
      *
      *   Selection: bypass all test groups.
      *
     C     EACO#         IFEQ      *ZERO
     C***** BEG ADD ******* 03/10/04 **********************************
     C     EAFYR         ORLT      @@STYR
     C***** END ADD ******* 03/10/04 **********************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     FNTEST        IFEQ      'Y'
     C     *IN60         OREQ      *ON
     C**** BEG ADD **** 10/02/01 *****************************************
     C     FNCICF        ORNE      'Y'
     C**** END ADD **** 10/02/01 *****************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *OFF
     C                   GOTO      NEXT
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
      * WRITE TO OUTPUT-If Either date is past last extract Date
      *
     C     EATRDT        IFGT      XXRNDT
     C**** BEG DEL **** 05/28/02 *****************************************
     C********   EAPODT    ORGT XXRNDT
     C**** END DEL **** 05/28/02 *****************************************
      *
     C                   EXSR      SROUT
      *
     C                   ENDIF
      *
     C     NEXT          TAG
     C                   READ      PEA01                                  60
     C                   ENDDO
      *
      *================================================================
      * TERM - Terminate and Exit Program
      *================================================================
      *
     C                   MOVE      '1'           *INLR
      *
      *================================================================
      * SROUT - Write record to output file
      *================================================================
     C     SROUT         BEGSR
      *
     C  N60
     CAN 60              READ      PAR02                                  60
     C                   CLEAR                   PAR02
      *
      *  Policy Number
     C**** Begin Add ****  07/22/05  **************************************
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        A2POL#
     C                   ELSE
     C**** End Add ******  07/22/05  **************************************
      *
     C                   MOVEL(P)  EAFND         A2POL#
     C                   MOVEL(P)  EAEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C                   MOVEL(P)  EAFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C**** Begin Add ****  07/22/05  **************************************
     C                   ENDIF                                                  e 62 OFF...&
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
      *
     C                   Z-ADD     EADIV         A2DIV
      *
     C                   Z-ADD     EAAMT         A2AMT
      *
     C     EATRDT        IFGT      XXRNDT
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     C     EAARTY        OREQ      'I'
     C**** BEGIN DEL **** 04/03/03 *************************************
     C*********  EAARTY    OREQ 'N'
     C**** END DEL ****** 04/03/03 *************************************
     C***  BEGIN ADD  ***  02/19/04  *********************************
     C     EAARTY        OREQ      'T'
     C***  END   ADD  ***  02/19/04  *********************************
     C     EAARTY        OREQ      'R'
     C     EATRDT        ORGE      EAPODT
     C                   MOVE      'C'           A2TRTP
     C**** BEGIN CHG *** 10/09/01 ************************************
      * IF IS A PAYMENT TYPE, MAKE IT NEGATIVE
     C                   MULT      -1            A2AMT
     C**** END CHG ***** 10/09/01 ************************************
     C*** BGN ADD 12/04/01 *******************************************
      * IF IS A REFUND TYPE, CHANGE IT BACK TO POSITIVE
     C     EAARTY        IFEQ      'R'
     C                   MULT      -1            A2AMT
     C                   ENDIF
     C*** END ADD 12/04/01 *******************************************
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     EAPODT        IFGT      XXRNDT
      *
     C     EATRDT        IFLE      EAPODT
     C     EAARTY        ANDNE     'P'
     C     EAARTY        ANDNE     'I'
     C     EAARTY        ANDNE     'Q'
     C**** BEGIN DEL **** 04/03/03 *************************************
     C********   EAARTY    ANDNE'N'
     C**** END DEL ****** 04/03/03 *************************************
     C     EAARTY        ANDNE     'R'
     C***  BEGIN ADD  ***  02/19/04  *********************************
     C     EAARTY        ANDNE     'T'
     C***  END   ADD  ***  02/19/04  *********************************
     C                   MOVE      'I'           A2TRTP
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     EADESC        A2TRDS
      *
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2TRDT
      *
     C                   MOVEL     EACHK#        A2CHK#
     C                   MOVEL     EANINV        A2NINV
     C                   Z-ADD     EAFND         A2FND
      *
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2PODT
      *
     C**** BEGIN CHG *** 11/13/01 ************************************
     C     A2PODT        IFLE      *BLANKS
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2PODT
     C                   ENDIF
     C**** END CHG ***** 11/13/01 ************************************
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C***********          MOVELDSDATE    XXDATE
     C                   MOVEL     @@JBDT        ZZJBDT            8 0
     C                   MOVEL     ZZJBDT        XXDATE
      ***** END CHG ***** 03/25/04 ************************************
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2AODT
      *
      *  This A2KEY build was taken out on 7/11/01 by moving Blanks to it
      *  Code left in this revision in case needed to be used again.
     C                   MOVEL(P)  XXKEY         A2KEY
     C                   CAT(P)    EAARCD:0      A2KEY
     C                   CAT(P)    EAARTY:0      A2KEY
      *
     C                   MOVE      *BLANKS       A2KEY
      *
      * IS DIRECT BILL POLICY ?
      *
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  60
     C                   ENDDO
      *
     C**** BEGIN ADD **** 05/08/02  **************************
      * Change to always load a "Y" even if no hit on Rate file
     C                   MOVE      'Y'           A2DBIL
     C**** END ADD ****** 05/08/02  **************************
      *
     C     *IN60         IFEQ      *OFF
      *
     C     A3KY          SETGT     PAR03
     C     A3KY1         READPE    PAR03                                  60
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
      ****** END ADD ***** 08/11/04 ************************************
      *
     C     *IN60         DOWEQ     *OFF
     C     A3STAT        ANDEQ     'I'
     C     A3KY1         READPE    PAR03                                  60
     C                   ENDDO
      *
     C**** BEGIN DEL **** 05/08/02  **************************
     C*********            MOVE 'Y'       A2DBIL
     C**** END DEL ****** 05/08/02  **************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      A3PLAN        A2DBIL
     C     A3PLAN        IFEQ      *BLANKS
     C                   MOVE      'Y'           A2DBIL
     C                   END
     C                   ENDIF                                                  AGKY
      *
     C                   ENDIF                                                  AHKY
      *
      *  Write to output file
      *
     C                   WRITE     PAR02
     C                   ENDSR                                                  SROUT
      *
      *================================================================
      * GETDAT- Return date formatted mm/dd/yy
      *================================================================
     C     GETDAT        BEGSR
      *
     C                   MOVEL(P)  XXDATE        X1DATE
      *
     C     SRERR         IFEQ      *OFF
      *
     C     X1MM          CAT(P)    '/':0         X2DATE           10
     C                   CAT(P)    X1DD:0        X2DATE
     C                   CAT(P)    '/':0         X2DATE
     C                   CAT(P)    X1YY:0        X2DATE
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       X2DATE
     C                   ENDIF                                                  SRERR
      *
     C                   ENDSR                                                  GETDAT
     C*
     C*================================================================
     C* *INZSR - Initialize the subset fields
     C*================================================================
     C     *INZSR        BEGSR
      *
      ********************
      *  PLIST Definitions
      ********************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@RNDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
      *
      ****************
      *  *LIKE Defines
      ****************
     C*
     C     *LIKE         DEFINE    EATRDT        XXRNDT
     C     *LIKE         DEFINE    EACO#         XXCO#
      *
      ************************************************************
      *  INITIALIZE
      ************************************************************
      *
     C                   MOVE      @@CO#         XXCO#
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C                   ENDIF                                                  SRERR
     C*
      *********
      *  KLISTS
      *********
      *
      *  Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
      *
      *  Employer A/R File - By Transaction Date
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  Agent History
      *
     C     AHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C                   KFLD                    XXRNDT
      *
     C     AHKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*
     C**** Begin Add ****  07/22/05  **************************************
     C     RPKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
      *
      *  Agent Rate
      *
     C     A3KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    AHRT#
     C                   KFLD                    XXRNDT
      *
     C     A3KY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    AHRT#
     C*
      *
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
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT1           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT2           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*
     C                   ENDSR
     C*================================================================
