     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA042R - API to Access WMOFP and Return Office Info
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*    This program will Access the WCMS Office Information by    *
     H*    first accessing the WCMS claim record to get the office    *
     H*    number.  Pass back Carrier name, Carrier Addr1, Carrier    *
     H*    Addr2, Carrier City/St/Zip, Carrier Area code and Carrier  *
     H*    phone#.  If any of these chains fail, then "F" will be     *
     H*    passed back to indicate failure.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F4112R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 02/28/00  PROGRAMMER: C:62  PROJECT: 48MEM/48MEN    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  *IN99 - General Chaining usage                               *
     H*                                                               *
     H*****************************************************************
     FWMOFL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9

     C*
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C     OFKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMOFF#
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@CRNM           40
     C                   PARM                    @@CRA1           40
     C                   PARM                    @@CRA2           40
     C                   PARM                    @@CRCZ           40
     C                   PARM                    @@CRAR            3 0
     C                   PARM                    @@CRPH            7 0
     C                   PARM                    @@RTCD            1
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     OFKY          CHAIN     POF01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  OFNAME        @@CRNM
     C                   MOVEL(P)  OFADR1        @@CRA1
     C                   MOVEL(P)  OFADR2        @@CRA2
     C*
     C                   Z-ADD     OFZIP         XXZIP
     C*
     C     OFCTY         CAT(P)    ',':0         @@CRCZ
     C                   CAT       OFST:1        @@CRCZ
     C                   CAT       XXZIP1:1      @@CRCZ
     C*
     C     XXZIP2        IFNE      *BLANKS
     C     XXZIP2        ANDNE     '0000'
     C*
     C                   CAT       '-':0         @@CRCZ
     C                   CAT       XXZIP2        @@CRCZ
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     OFAREA        @@CRAR            3 0
     C                   Z-ADD     OFPHN#        @@CRPH            7 0
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
