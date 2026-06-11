     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/TITLE M0021R - Work with Environment Library List
    H*****************************************************************
    H*  Created:                                                     *
    H*  04/05/01  C:69  Project: SY033                               *
    H*                                                               *
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*****************************************************************
     Fs0035f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fsitpf     uf a e             disk
    D*
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D optnm           c                   'ITP Control Information'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##err           s              1
     D ##leng          s              3  0
     D ##objn          s             10
     D ##objt          s              7
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     D/copy sprstring
     D/copy sprdate
     D
    C*
     C                   movel(p)  optnm         scconm
     C                   eval      scconm = centerFld(scconm)
    C*
     C     1             chain(n)  sitpf                              99
     C                   if        *in99 = *off
     C                   exsr      mvdbsc
     C                   endif
    C*
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
     C                   exfmt     scr01
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      errck1
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif
E01  C                   enddo
    C*
     C                   eval      *inlr = *on
    C*================================================================
    C*  ERRCK1 - Error checking for display
    C*================================================================
     C     errck1        begsr
    C*
     C                   if        *in90 = *off
     C                             and scuse <> 'Y'
     C                             and scuse <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   endif
    C*
     C                   if        scuse = 'Y'
     C                   if        *in90 = *off
     C                             and scprcsvr = ' '
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scdocstg = ' '
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and sctmpdir = ' '
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C     1             chain     sitpf                              99
     C                   exsr      mvscdb
     C                   time                    dsdttm
     C                   movel     q1pgm         itpgm
     C                   movel     q1user        ituser
     C                   eval      itdt = dtCYMD(dsdate)
     C                   movel     dstime        ittime
     C                   if        *in99 = *off
     C                   update    pit01
     C                   else
     C                   write     pit01
     C                   endif
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     ituse         scuse
     C                   movel     itprcsvr      scprcsvr
     C                   movel     itdocstg      scdocstg
     C                   movel     itdocdir      scdocdir
     C                   movel     ittmpdir      sctmpdir
     C                   movel     itpgm         scpgm
     C                   movel     ituser        scuser
     C                   eval      scdt = dtMDYY(itdt)
     C                   movel     ittime        sctime
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     scuse         ituse
     C                   movel     scprcsvr      itprcsvr
     C                   movel     scdocstg      itdocstg
     C                   movel     scdocdir      itdocdir
     C                   movel     sctmpdir      ittmpdir
     C                   endsr
