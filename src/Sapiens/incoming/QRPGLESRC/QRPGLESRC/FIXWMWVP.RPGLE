     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXWMWVP - WAIVER DATA FIX PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program reads records from the certificate holder     *
     H*    file (WMCEP) that are a waiver type (CETYPE = 'S') and     *
     H*    write a record to the waiver file (WMWVP).  Once the       *
     H*    waiver record is created, it will delete the record from   *
     H*    WMCEP.
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Command Line                                               *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:113                                            *
     H*  CREATED:    05/12/99  PROJECT: CM045                         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR      FUNCTION DESCRIPTION                        *
     H*      50           General usage                               *
     H*      51           General usage                               *
     H*      60           General chaining                            *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*   FILES:                                                      *
     H*   WMWVP - Waiver Detail file                                  *
     H*   WMCEP - Certificate Holder file                             *
     H*                                                               *
     H*****************************************************************
     F*
     F*  Certificate Holders file
     FWMCEP     UF   E             DISK
     F*
     F*  Waiver Detail file
     FWMWVP     O    E             DISK
     F*
     F*  Output file
     FQSYSPRT   O    F   92        PRINTER OFLIND(*INOA)

     D*================================================================
     D*  Data Structure Definitions
     D*================================================================
     D*
     D*  External system information
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* System date and time
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*================================================================
     C*  Main
     C*================================================================
     C*
     C*  Get the time and print header
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C*  Read a record from the certificate holders file
     C*
     C                   READ      WMCEP                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*  If the type is 'S' copy the fields to WMWVP.
     C*
     C     CETYPE        IFEQ      'S'
     C*
     C                   Z-ADD     CECO#         WVCO#
     C                   Z-ADD     CEFND         WVFND
     C                   Z-ADD     CEFYR         WVFYR
     C                   Z-ADD     CEEMP#        WVEMP#
     C                   Z-ADD     CEDIV         WVDIV
     C                   Z-ADD     CECER#        WVWAV#
     C                   MOVEL     CENAME        WVNAME
     C                   MOVEL     CEADR1        WVADR1
     C                   MOVEL     CEADR2        WVADR2
     C                   MOVEL     CECTY         WVCTY
     C                   MOVEL     CEST          WVST
     C                   Z-ADD     CEZIP         WVZIP
     C                   MOVEL     CEATTN        WVPROJ
     C                   MOVEL     CESTAT        WVSTAT
     C                   MOVEL     CEUSER        WVUSER
     C                   MOVEL     CEPGM         WVPGM
     C                   MOVEL     CETIME        WVTIME
     C                   MOVEL     CEDT          WVDT
     C                   MOVEL     CEIUSR        WVIUSR
     C                   MOVEL     CEIPGM        WVIPGM
     C                   MOVEL     CEIDT         WVIDT
     C                   MOVEL     CEITM         WVITM
     C*
     C*  Write the new record to WMWVP
     C*
     C                   WRITE     PWV01
     C*
     C*  Delete the old record from WMCEP
     C*
     C                   DELETE    WMCEP
     C*
     C*  Print the detail line
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  CETYPE = 'S'
     C*
     C*  Read the next record from the certificate holders file
     C*
     C                   READ      WMCEP                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C*  End the program
     C*
     C                   SETON                                        LR
     O/EJECT
     O*** HEADER LINES ************************************************
     OQSYSPRT   T    LR                     1
     O          E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           47 'WAIVER RECORDS MOVED FRO'
     O                                           63 'M WMCEP TO WMWVP'
     O                       UDATE         Y     92
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       PAGE          Z     92
     O                                           88 'PAGE'
     O          E            HEAD           2
     O                                            8 'WAIVER #'
     O                                           16 'COMPANY'
     O                                           22 'GROUP'
     O                                           29 'POL YR'
     O                                           40 'EMPLOYER #'
     O                                           49 'DIVISION'
     O                                           54 'NAME'
     O*** DETAIL LINE  ************************************************
     O          E            DET            2
     O                       WVWAV#        4      8
     O                       WVCO#         4     16
     O                       WVFND         4     22
     O                       WVFYR         4     29
     O                       WVEMP#        4     40
     O                       WVDIV         4     49
     O                       WVNAME              91
