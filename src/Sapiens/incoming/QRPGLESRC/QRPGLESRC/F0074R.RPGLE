     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0074R - ERROR MESSAGE PURGE
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program removes adjudication messages from FDMGP   **
     H**     (Bill Entry Message File). The following adjudications  **
     H**     are supported by this program:                          **
     H**        . State Adjudication                                 **
     H**        . Pre/Re-Certification                               **
     H**        . HPR CodeReview                                     **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 12/10/96         PROJECT: MC119                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F0070R - Bill Entry                                     **
     H**     F0086R - Request Payment EOMB Print                     **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      40       No FDMGL record found                         **
     H**      41       Purge current record                          **
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**   FDMGL    (PMG01): Bill Entry Message File (CO#/SUB#/...   **
     H**   FDMGL1   (PMG02): Bill Entry Message File (CO#/SUB#/...   **
     H*****************************************************************
     H*
     FFDMGL     UF   E           K DISK
     FFDMGL1    UF   E           K DISK
     F                                     RENAME(PMG01:PMG02)

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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      /EJECT

     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQDCS#            5
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQDEL1            1
     C                   PARM                    QQDEL2            1
     C                   PARM                    QQDEL3            1
     C                   PARM                    QQDEL4            1
     C                   PARM                    QQDEL5            1
     C*
     C* ---------------------------------------------------------------
     C*        Key Lists
     C* ---------------------------------------------------------------
     C*          PMG01 - Bill Entry Message File by DCS#
     C     MGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDCS#
     C     *LIKE         DEFINE    MGCO#         XXCO#
     C     *LIKE         DEFINE    MGSUB#        XXSUB
     C     *LIKE         DEFINE    MGFND         XXFND
     C     *LIKE         DEFINE    MGFYR         XXFYR
     C     *LIKE         DEFINE    MGCASE        XXCASE
     C     *LIKE         DEFINE    MGDCS#        XXDCS#
     C*
     C*          PMG02 - Bill Entry Message File by EOB#
     C     MGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C     *LIKE         DEFINE    MGEOB#        XXEOB#
     C*
     C*================================================================
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*================================================================
     C*
     C*          Execute SR00 for initialization
     C*          ------------------------------------------------------
     C                   EXSR      SR00
     C*
     C*          Determine if delete is to be by DCS# or EOB#
     C*          ------------------------------------------------------
     C     QQDCS#        IFNE      '00000'
     C*
     C*             Use DCS# for deletion - Read all records for bill
     C*             ---------------------------------------------------
     C     MGKY          CHAIN     PMG01                              40
     C     *IN40         DOWEQ     *OFF
     C*
     C*                Delete record if match on a parm value:
     C*                   Values are: C - Codereview
     C*                               P - Pre-Certification
     C*                               S - State Adjudication
     C*                ------------------------------------------------
     C     MGSRC         LOOKUP    PC                                     41
     C   41              DELETE    PMG01
     C*
     C*                Read next record
     C*                ------------------------------------------------
     C     MGKY          READE     PMG01                                  40
     C                   ENDDO                                                  40 = Off
     C*
     C*             Use EOB# for deletion - Read all records for bill
     C*             ---------------------------------------------------
     C                   ELSE                                                   QQDSC# <> 0
     C     MGKY1         CHAIN     PMG02                              40
     C     *IN40         DOWEQ     *OFF
     C*
     C*                Delete record if match on a parm value:
     C*                   Values are: C - Codereview
     C*                               P - Pre-Certification
     C*                               S - State Adjudication
     C*                ------------------------------------------------
     C     MGSRC         LOOKUP    PC                                     41
     C   41              DELETE    PMG02
     C*
     C*                Read next record
     C*                ------------------------------------------------
     C     MGKY1         READE     PMG02                                  40
     C                   ENDDO                                                  40 = Off
     C                   ENDIF                                                  QQDSC# <> 0
     C*
     C*          End of program
     C*          ------------------------------------------------------
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB         XXSUB
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQDCS#        XXDCS#
     C*
     C*          Move delete parameters to array
     C*          ------------------------------------------------------
     C                   MOVE      QQDEL1        PC(1)
     C                   MOVE      QQDEL2        PC(2)
     C                   MOVE      QQDEL3        PC(3)
     C                   MOVE      QQDEL4        PC(4)
     C                   MOVE      QQDEL5        PC(5)
     C*
     C*          End of subroutine SR00
     C*          ------------------------------------------------------
     C     SR00X         ENDSR
