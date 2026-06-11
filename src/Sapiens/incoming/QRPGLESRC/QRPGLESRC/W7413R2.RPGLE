     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7413R2 - OVERRIDE POLICY PAGE ENDORSEMENT EFF DATE
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL OVERRIDE THE ENDORSEMENT EFFECTIVE    **
     H**     DATE THAT IS TO PRINT ON THE POLICY PAGE ENDORSEMENT    **
     H**     AT THE FOOTER WITH THE ENDORSEMENT EFFECTIVE DATE AS    **
     H**     KEYED BY THE USER UNDER OPT 54 IN EMPLOYER POLICY YRS.  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C2 - NCCI POLICY INFO PAGE - RESOLVE OUTPUT        **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:09                                          **
     H**   CREATED:    7/07/99      PROJECT: CM027 (WA096/SEQ# 18648)**
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H**                                                             **
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*****************************************************************
     FWXFMP     UF   E             DISK
     FWMMUL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     MUKY          KLIST
     C                   KFLD                    FMCO#
     C                   KFLD                    FMFND
     C                   KFLD                    FMEMP#
     C                   KFLD                    FMDIV
     C                   KFLD                    FMFYR
     C                   KFLD                    FMFORM
     C                   KFLD                    XXEND#
     C*
     C                   Z-ADD     *ZEROS        XXEND#
     C                   READ      WXFMP                                  60
     C     *IN60         IFEQ      '0'
     C     MUKY          CHAIN     PMU01                              65
     C     *IN65         IFEQ      '0'
     C     MUEEDT        ANDNE     FMEEDT
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   UPDATE    PFM01
     C                   END                                                    *IN65
     C                   END                                                    *IN60
     C*
     C                   MOVE      '1'           *INLR
