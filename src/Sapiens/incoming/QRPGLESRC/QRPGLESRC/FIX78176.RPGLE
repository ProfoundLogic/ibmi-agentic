     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h debug(*yes)
     H*
     H/TITLE FIX78176 - Reverse Claim Staff Code Changes
     H*****************************************************************
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Created to address needs of support seq78176.  Reverse     *
     H*    Claims staff code changes.                                 *
     H*                                                               *
     H*    FIX78176                                                   *
     H*      - Reads temporary WADTL records matching criteria of     *
     H*        process run in error.                                  *
     H*      - If run in 'U'pdate mode,                               *
     H*        Updates Claims files (WMCMP, FMCMP) with correct data. *
     H*      - Produces report of affected claims.                    *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*      FA030R                                                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/04/11 C:061 SUPPORT WA045 78176                            *
     H*                Created.                                       *
     H*                                                               *
     H*****************************************************************
      *
      * Darts Transfer
     Fwadtl     IF   E           K DISK
      *
      * Claims Master
     Fwmcml1    UF   E           K DISK
      *
      *** Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)


      * SYSTEM DATE AND TIME

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0


     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      ****************************************************************
      *  KLIST
      ****************************************************************

      * Claims Master

     c     cmky          klist
     c                   kfld                    dtco#
     c                   kfld                    dtfnd
     c                   kfld                    dtfyr
     c                   kfld                    dtcase


      ****************************************************************
      *  PLIST
      ****************************************************************

     c     *entry        PLIST
     c                   parm                    @@upd             1


     c     dtcymd        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      '*CYMD'       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1

      ****************************************************************
      *  MAIN LOOP
      ****************************************************************

      * Initialize
     C                   MOVE      *ALL'_'       LINE            132
     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCYMD            7 0

     C                   Z-ADD     *ZERO         COUNT            11 0
     C                   EXCEPT    HEAD

      * Read saved DARTS Transfer file records

     c                   read      pdt01

     c                   dow       not %eof

      * Update Claime Records with previous adjuster staff code

     C     cmky          chain     pcm01

     c                   if        %found
     c                             and (dtpgm = 'W4301R')
     c                             and (dtdt = 1111006)
     c                             and (dtuser = 'EJOHNSON')
      *
      *                    *** WRITE DETAIL TO SPOOL FILE
     C   OA              EXCEPT    HEAD
     c   oa              setoff                                       oa
     C                   EXCEPT    DET
     C                   ADD       1             COUNT

     c                   if        @@UPD = 'U'
     c                   eval      cmclex = dtostf

     c                   eval      cmuser = q1user
     c                   eval      cmpgm  = q1pgm
     c                   eval      cmtime = dstime
     c                   eval      cmdt   = xxcymd
     c                   update    pcm01
     c                   exsr      MCUPD
     C                   endif

     C                   endif

     c                   read      pdt01
     C                   enddo                                                  E2 RECORD
      *
     c                   if        count = 0
     C                   EXCEPT    NOREC
     c                   else
     C                   EXCEPT    TOT
     c                   endif
      *
     C                   SETON                                        LR
      *
      *
     C*===============================================================*
     C*  MCUPD - This subroutine determines if WCMC is installed and
     C*    linked, then calls FA030R to update the staff codes in the
     C*    WCMC claim file.
     C*===============================================================*
     C     MCUPD         BEGSR

     C*     WCMC Installed determination

     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1
     C                   PARM                    ##FBIF            1
     C                   PARM                    ##FL01            1

     C     ##FBIF        IFEQ      'Y'
     C     ##FL01        ANDEQ     'Y'
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE

     C                   CALL      'FA030R'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      CMNRSE        ##CMNU            3            Nurse
     C                   PARM      *BLANKS       ##CMSU            3            Unavailable
     C                   PARM      CMCLEX        ##CLAD            3            Claim examiner
     C                   PARM      CMSUPR        ##CLSU            3            Supervisor
     C                   PARM      CMFEAD        ##CLFA            3            FEAD
     C                   PARM      *BLANKS       ##RTCD            1

     C                   ENDIF
     C*
     C                   ENDSR
      *
      *
      *
      *
      *
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       DSDATE        Y    132
      *
     O          E            HEAD           3
     O                       Q1PGM               10
     O                                         +020 'REVERSE STAFF CODE'
     O                                         +001 'CHANGES'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132

     O          E            HEAD           0
     O                       LINE              +000

     O          E            HEAD           3
     O                                            6 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           22 'CASE'
     O                                         +003 'OLD'
     O                                         +003 'NEW'
      *
     O          E            DET            1
     O                       DTCO#         Z      6
     O                       DTFND         Z     10
     O                       DTFYR         Z     15
     O                       DTCASE        Z     22
     O                       DTNSTF            +003
     O                       DTOSTF            +003
      *
     O          E            TOT         2  1
     O                                           20 'COUNT'
     O                       COUNT         Z   +005
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
