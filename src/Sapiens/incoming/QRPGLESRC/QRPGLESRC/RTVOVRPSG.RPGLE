     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE RTVOVRPSG - API - Retrieve  Overlay/Page Segment
    H*********************************************************************
     H* inType: @@CLT# Client ID                                          *
     H*         @@CO#  Company number                                     *
     H*         @@OPFL Flag to indicate if O overlay or P page segment    *
     H* Must be given one of:                                             *
     H*         @@FORM Endorsement Form Name                              *
     H*         @@PRNM Program name                                       *
     H*         @@PRTF Output printer file name                           *
     H* May optionally also be provided with:                             *
     H*         @@PAGE Page number                                        *
     H*         @@SEQ# Sequence number                                    *
     H*         @@FND  Group number                                       *
     H* Return:                                                           *
     H*         @@OVPS Overlay / Page Segment Name                        *
     H*         @@LIB  Library where Overlay or Page Segment stored       *
     H*         @@VPOS Vertical position                                  *
     H*         @@HPOS Horizontal Position                                *
     H*         @@WIDE Widthr                                             *
     H*         @@HIGH Height                                             *
     H*         @@ROTA Rotate                                             *
     H*         @@RTCD Success Indicator returns 'X' in error in call.    *
    H*********************************************************************
/*  CH* Created: 08/05/10  C:220 Project: WC1920                          *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
      * 05/22/11 C:190 Project 114WZD
      *                Add processing for the carrier #
      *
    H*********************************************************************
      ******* beg chg ****** 05/22/11 *********************************
     f*******stopl     if   e           k disk
     fstopl4    if   e           k disk
      ******* end chg ****** 05/22/11 *********************************
     fstopl1    if   e           k disk    rename(pop01:pop02)
     fstopl2    if   e           k disk    rename(pop01:pop03)
     f*
     D*
     DxxCo#            s                   like(opco#)
      ******* beg add ****** 05/22/11 *********************************
     Dxxfnd            s                   like(opfnd)
     Dsvfnd            s                   like(opfnd)
     Dxxcar#           s                   like(opcar#)
     Dsvcar#           s                   like(opcar#)
      ******* end add ****** 05/22/11 *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@clt#            3
     C                   PARM                    @@co#             3 0
     C                   PARM                    @@opfl            1
     C                   PARM                    @@form           15
     C                   PARM                    @@prnm           10
     C                   PARM                    @@prtf           10
     C                   PARM                    @@page            7 0
     C                   PARM                    @@seq#            7 0
     C                   PARM                    @@fnd             3 0
     C                   PARM                    @@ovps            8
     C                   PARM                    @@lib            10
     C                   PARM                    @@vpos            5 3
     C                   PARM                    @@hpos            5 3
     C                   PARM                    @@wide            5 3
     C                   PARM                    @@high            5 3
     C                   PARM                    @@rota            3 0
     C                   PARM                    @@rtcd            1
      ******* beg add ****** 05/22/11 *********************************
     C                   PARM                    @@car#            5 0
      ******* end add ****** 05/22/11 *********************************
     C
     C**
     C                   select
     C                   when      @@clt# <> *zeros
     C                             and @@co# <> *zeros
     C                             and @@form <> *blanks
     C                   exsr      srform
     C                   goto      return
     C*
     C                   when      @@prnm <> *blanks
     C                   exsr      srprnm
     C                   goto      return
     C*
     C                   when      @@prtf <> *blanks
     C                   exsr      srprtf
     C                   goto      return
     C*
     C                   other
     C                   eval      @@rtcd = 'X'
     C                   endsl
     C*
     C     return        tag
     C*
     C                   eval      *inlr = *on

     C*====================================================================
     C* Initialization
     C*====================================================================
     C     *inzsr        begsr
     C*
     C     OpKey         Klist
     C                   Kfld                    @@clt#
     C                   Kfld                    @@co#
     C                   Kfld                    @@form
     C                   Kfld                    @@page
     C                   Kfld                    @@seq#
      ******* beg chg ****** 05/22/11 *********************************
     C*******            Kfld                    @@fnd
     C                   Kfld                    xxfnd
     C                   Kfld                    xxcar#
      ******* end chg ****** 05/22/11 *********************************
     C                   Kfld                    @@opfl
     C*
     C     OpKey1        Klist
     C                   Kfld                    @@clt#
     C                   Kfld                    @@co#
     C                   Kfld                    @@prnm
     C                   Kfld                    @@seq#
     C                   Kfld                    @@opfl
     C*
     C     OpKey2        Klist
     C                   Kfld                    @@clt#
     C                   Kfld                    @@co#
     C                   Kfld                    @@prtf
     C                   Kfld                    @@seq#
     C                   Kfld                    @@opfl
     C*
      ******* beg add ****** 05/22/11 *********************************
     c                   z-add     0             svfnd
     c                   z-add     0             svcar#
     c                   eval      svfnd = @@fnd
     c                   if        %parms >= 18
     c                   eval      svcar# = @@car#
     c                   endif
      ******* end add ****** 05/22/11 *********************************
     C                   endsr

     C*====================================================================
     C* Load Endorsement Form Information
     C*====================================================================
     C     SrForm        begsr
     C*
     C*
      ******* beg add ****** 05/22/11 *********************************
     c                   z-add     0             xxfnd
     c                   z-add     0             xxcar#
     c                   if        svfnd <> 0
     c                   eval      xxfnd = svfnd
     c                   eval      svfnd = 0
     c                   else
     c                   if        svcar# <> 0
     c                   eval      xxcar# = svcar#
     c                   eval      svcar# = 0
     c                   endif
     c                   endif
      ******* end add ****** 05/22/11 *********************************
     C     OpKey         chain     pop01                              60
     C                   dow       *in60 = *off
     C*
     C                   if        @@page <> *zeros and
     C                             @@page <> oppage or
     C                             @@seq# <> *zeros and
     C                             @@seq# <> opseq#  or
     C                             @@fnd  <> *zeros and
     C                             @@fnd  <> opfnd
     C                   iter
     C                   else
     C*
     C                   eval      @@ovps = opovps
     C                   eval      @@lib  = oplib
     C                   eval      @@vpos = opvpos
     C                   eval      @@hpos = ophpos
     C                   eval      @@wide = opwide
     C                   eval      @@high = ophigh
     C                   eval      @@rota = oprota
     C                   eval      @@rtcd = *blank
     C*
     C                   goto      return
     C                   endif
      ******* beg add ****** 05/22/11 *********************************
     c                   eval      xxfnd  = 0
     c                   eval      xxcar# = 0
      ******* end add ****** 05/22/11 *********************************
     C*
     C     OpKey         chain     pop01                              60
     C                   enddo
     C*
     C                   endsr

     C*====================================================================
     C* Load Program Information
     C*====================================================================
     C     SrPrnm        begsr
     C*
     C                   if        @@seq# <> *zeros
     C*
     C     OpKey1        chain     pop02                              60
     C                   if        *in60 = *off
     C                   eval      @@ovps = opovps
     C                   eval      @@lib  = oplib
     C                   eval      @@vpos = opvpos
     C                   eval      @@hpos = ophpos
     C                   eval      @@wide = opwide
     C                   eval      @@high = ophigh
     C                   eval      @@rota = oprota
     C                   eval      @@rtcd = *blank
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr

     C*====================================================================
     C* Load Output printer file Information
     C*====================================================================
     C     SrPrtf        begsr
     C*
     C                   if        @@seq# <> *zeros
     C*
     C     OpKey2        chain     pop03                              60
     C                   if        *in60 = *off
     C                   eval      @@ovps = opovps
     C                   eval      @@lib  = oplib
     C                   eval      @@vpos = opvpos
     C                   eval      @@hpos = ophpos
     C                   eval      @@wide = opwide
     C                   eval      @@high = ophigh
     C                   eval      @@rota = oprota
     C                   eval      @@rtcd = *blank
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
