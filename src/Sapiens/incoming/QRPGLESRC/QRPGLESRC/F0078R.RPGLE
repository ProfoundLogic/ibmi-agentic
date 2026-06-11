     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0078R - PROVIDER BILL PRE-ADJUDICATION DELETION PROGRAM
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program removes adjudication messages from FDMGP   **
     H**     (Bill Entry Message File) and EOB Codes from FDDEP.     **
     H**                                                             **
     H*      This program supports the following adjudications       **
     H**        . State Adjudication                                 **
     H**        . Pre/Re-Certification                               **
     H**                                                             **
     H**   PROGRAMMER: C:56                                          **
     H**   CREATED: 11/19/98         PROJECT: MC152                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     FA713R - Adjudication Call API                          **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  07/18/23 C:342 Project: NY060                               **
     H*                 NY Compliance: CARC/RARC Update              **
     H*                                                              **
     H*  11/26/02 C:62  Project: 114MDC                              **
     H*                 Don't delete CodeReview EOB codes.           **
     H*                                                              **
     H*  08/21/02 C:62  Project: 114MDC                              **
     H*                 Make sure and delete CodeReview and          **
     H*                 Correct Codeing EOB codes                    **
     H*                                                              **
     H*  01/20/00 C:62  Project: NW012                               **
     H*                 DON'T DELETE NW00001.                        **
     H*                                                              **
     H*  03/19/99 C:56  Project: MC152                               **
     H*                 1. Allow deletion of duplicate charges error **
     H*****************************************************************
     H*
     FFDMGL4    UF   E           K DISK
     FFDDEL3    UF   E           K DISK

     D*
     D* ---------------------------------------------------------------
     D* Tables and arrays
     D* ---------------------------------------------------------------
     D*
     D PC              S              1    DIM(5)                               PURGE CODES
     D*
     D* ---------------------------------------------------------------
     D* External System Information Data Structure
     D* ---------------------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------
     D* System date and time
     D*----------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      /EJECT

     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@RCN             3
     C                   PARM                    @@DEL1            1
     C                   PARM                    @@DEL2            1
     C                   PARM                    @@DEL3            1
     C                   PARM                    @@DEL4            1
     C                   PARM                    @@DEL5            1
     C*
     C* ---------------------------------------------------------------
     C*        Key Lists
     C* ---------------------------------------------------------------
     C*          PMG01 - Bill Entry Message File by DCS#
     C     MGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     DEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     *LIKE         DEFINE    MGCO#         KKCO#
     C     *LIKE         DEFINE    MGSUB#        KKSUB#
     C     *LIKE         DEFINE    MGFND         KKFND
     C     *LIKE         DEFINE    MGFYR         KKFYR
     C     *LIKE         DEFINE    MGCASE        KKCASE
     C     *LIKE         DEFINE    MGEOB#        KKEOB#
     C     *LIKE         DEFINE    MGRCN         KKRCN
     C*
     C*================================================================
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*================================================================
     C*
     C*    Execute SR00 for initialization
     C*    -------------------------------
     C                   EXSR      SR00
     C*
     C*  Read all records for bill
     C*
     C     MGKY          CHAIN     PMG01                              40
     C     *IN40         DOWEQ     *OFF
     C*
     C*    Delete record if match on a parm value:
     C*    Values are: C - Correct Coding
     C*                P - Pre-Certification
     C*                S - State Adjudication
     C*
     C                   MOVE      MGMSG#        XXMSG#            5
     C     MGSRC         LOOKUP    PC                                     41
     C***41*****           DELETPMG01
     C     *IN41         IFEQ      *ON
     C*** BEGIN ADD *** 01/20/00 **************************************
     C     MGMSG#        ANDNE     'NW00001'
     C*** END   ADD *** 01/20/00 **************************************
     C*** BEG DEL *** 03/19/99 ****************************************
     C********** XXMSG#    ANDNE'00003'
     C*** END DEL *** 03/19/99 ****************************************
     C                   DELETE    PMG01
     C                   ELSE
     C                   EXCEPT    RPMG01
     C                   ENDIF                                                  *IN41 = *ON
     C*
     C     MGKY          READE     PMG01                                  40
     C                   ENDDO                                                  40 = Off
     C*
     C* DELETE EOB CODES; ONLY IF SOURCE IS A=ADJUDICATION OR N=NETWORK
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C     DEESRC        OREQ      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*** BEGIN ADD *** 08/21/02 **************************************
     C     DEESRC        OREQ      'C'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*** BEGIN DEL *** 11/26/02 **************************************
     C********** DEESRC    OREQ 'R'
     C********** DEEOB     ANDNE*BLANKS
     C*** END   DEL *** 11/26/02 **************************************
     C*** END   ADD *** 08/21/02 **************************************
     C                   DELETE    PDE01
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF                                                  DEESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO
     C*
     C*  End of program
     C                   SETON                                        LR
     C*
     C*================================================================
     C*================================================================
     C*          S U B R O U T I N E S
     C*================================================================
     C*================================================================
     C*  SR00    Initialization Subroutine
     C*================================================================
     C*
     C*          Get time
     C*          ------------------------------------------------------
     C     SR00          BEGSR
     C                   TIME                    DSDTTM
     C*
     C*          Move parameters to key fields
     C*          ------------------------------------------------------
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@EOB#        KKEOB#
     C                   MOVE      @@RCN         KKRCN
     C*
     C*          Move delete parameters to array
     C*          ------------------------------------------------------
     C                   MOVE      @@DEL1        PC(1)
     C                   MOVE      @@DEL2        PC(2)
     C                   MOVE      @@DEL3        PC(3)
     C                   MOVE      @@DEL4        PC(4)
     C                   MOVE      @@DEL5        PC(5)
     C*
     C*          End of subroutine SR00
     C*          ------------------------------------------------------
     C     SR00X         ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
     H*****************************************************************
