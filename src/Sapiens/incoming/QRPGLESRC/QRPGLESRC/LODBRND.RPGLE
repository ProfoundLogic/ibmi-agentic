     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*
     H**************************************************************************************
     H*  Description: This program load the Brand Name 'Sapiens, Inc.'
     H*               in company master file (SMCOP) file.                  ne FDCAP and   *
     H*                                                                                    *
     H*  Created: 06/18/10   Programmer C:220   Project:SY215                              *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 06/10/20  C:33  Project: SY419   *** No comments                  *
     H*                 Sapiens                                           *
     H**************************************************************************************
     FSMCOP     uf   e             DISK
     F*                                                                                    *
     F**************************************************************************************
     D*  Brand name constant
     D brnd            c                   CONST('Sapiens, Inc.')
     D
     D*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D
     D
     DPSDS           ESDS                  Extname(SZQ1P)
     D
     C*  Parm lists
     c     DTCYMD        PLIST
     c                   PARM                    SRDTI             8 0
     c                   PARM      *blanks       SRFFMT            8
     c                   PARM      '*CYMD'       SRTFMT            8
     c                   PARM                    SRDTO            10
     c                   PARM                    SRERR             1
     C*  Key lists
     C
     C*
     C*====================================================================
     C*  mainloop - process all policies
     C*====================================================================
     C*
     C                   TIME                    dsdttm
     C*
     C*  Process all company records
     C*
     C     1             setll     pco01                                  70
     C                   read      pco01                                  70
     C*
     C                   dow       *in70 = *off
     C*
     C*  Load Brand Name 'Sapiens, Inc.'
     C*
     C                   movel     brnd          cobrnd
     C*
     C                   eval      copgm= 'LODBRND'
     C                   eval      couser= Q1USER
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         codt
     C*
     C                   eval      cotime = dstime
     C*
     C                   update    pco01
     C*
     C                   read      pco01                                  70
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
