     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*********************************************************************
    H/TITLE  Prompt - Roll Exp Mod to WCMS
    H*********************************************************************
    H*  DESCRIPTION:  Prompts user for paramaters that will be used to   *
    H*                load Exp Mod to WCMS                               *
    H*                                                                   *
    H*  CALLED FROM:  Menu                                               *
    H*                                                                   *
    H*  CALLS TO:     E8051C - Submitter                                 *
    H*                                                                   *
    H*  CREATED: 11/05/03      C:136     PROJECT: WC1156                 *
    H*********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fe8051f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fexxtl2    if   e           k disk    rename(pxx01:pxx02)
     F
    F*
     D/copy scopypr01d
     D/copy wprgroup
     D/copy sprdate
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Roll Exp Mod to WCMS')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D group           s              3  0
     D fnrrn           s              9  0
     D grpYear         s              3  0
     D ##efdt          s              7  0
     D inkc            s               n
     D inkl            s               n
      *

     D srchGrpYear     pr                  extpgm('W0011R')
     D  ##kc                          1
     D  ##kl                          1
     D  ##call                        1    const
     D  ##rrn                         9  0 const
     D  ##fyr                         3  0 const
      *
     D SubmitPgm       pr                  extpgm('E8051C')
     D  ##co#                         3    const
     D  ##Fnd                         3    const
     D  ##yrB                         3    const
     D  ##efdt                        7    const
     D  ##que                        10    const
     D  ##mode                        1    const
      *
     D ChkGroup        pr                  like(*in90)
     D ChkGrpYear      pr                  like(*in90)
     D ChkDate         pr                  like(*in90)
     D ChkForRec       pr                  like(*in90)
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq

     C/copy scopypr01c
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     dscapi        begsr
     C                   call      comapi        compl                  91
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     errck1        begsr

     C  n90              eval      *in90 = ChkGroup
     C  n90              eval      *in90 = ChkGrpYear
     C  n90              eval      *in90 = ChkDate
     C  n90              eval      *in90 = ChkForRec

     C                   endsr                                                  errck1
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     kjdo          begsr
      *
     C                   select
      *
     C                   when      hfld = 'SCGRP'
     C                   callp     grpSrch(inkc:inkl:group)
      *
     C                   if        group <> *zero
     C                   eval      scgrp = group
     C                   eval      *in81 = *on
     C                   endif
      *
     C                   when      hfld = 'SCFYR' and scgrp <> 0
     C                   eval      fnrrn = 0
     C                   callp     validGrp(scgrp:fnrrn)
     C                   callp     srchGrpYear(inkc:inkl:'2':fnrrn:grpYear)

     C                   if        grpYear <> *zero
     C                   eval      scfyr   = grpYear
     C                   eval      *in82 = *on
     C                   else
     C                   eval      *in81 = *on
     C                   endif
      *
     C                   endsl
      *
     C                   endsr
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     othfks        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     pgmstr        begsr
    C*
     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    C*
     C                   endsr                                                  pgmstr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     predsp        begsr
     C                   endsr
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     prsctl        begsr

     C                   eval      ##efdt = dtCymd(scexdt)
     C                   callp     SubmitPgm(%editc(##co#:'Z'):
     C                                       %editc(scGrp:'Z'):
     C                                       %editc(scFyr:'Z'):
     C                                       %editc(##efdt:'Z'):
     C                                       (##outq):
     C                                       ('D')
     C                                      )

     C                   endsr
    C*====================================================================
    C* ChkGroup - Check the scgrp field
    C*====================================================================
     PChkGroup         b

     D ChkGroup        pi                  like(*in90)

     C                   if        validGrp(scgrp) = *off
     C                   seton                                        71
     C                   return    *on
     C                   endif

     C                   return    *off

     PChkGroup         e
    C*====================================================================
    C* ChkGrpYear - Check the scfyr field
    C*====================================================================
     PChkGrpYear       b

     D ChkGrpYear      pi                  like(*in90)

     C                   if        validGrpYear(scgrp:scfyr) = *off
     C                   seton                                        72
     C                   return    *on
     C                   endif

     C                   return    *off

     PChkGrpYear       e
    C*====================================================================
    C* ChkDate - Check the scexdt field
    C*====================================================================
     PChkDate          b

     D ChkDate         pi                  like(*in90)

     C                   if        dtCheck(scexdt) = *zero
     C                   seton                                        73
     C                   return    *on
     C                   endif

     C                   eval      scexdt = dtCheck(scexdt)

     C                   return    *off

     PChkDate          e
    C*====================================================================
    C* ChkForRec - Check that a type 'C' record exists in EXXTP for requested date
    C*====================================================================
     PChkForRec        b

     D ChkForRec       pi                  like(*in90)

     D date            s                   like(xefdt)

     C     xxky          klist
     C                   kfld                    ##co#
     C                   kfld                    scgrp
     C                   kfld                    date

     C                   eval      date   = dtCymd(scexdt)
     C     xxky          chain     pxx02

     C                   if        not %found
     C                   seton                                        74
     C                   return    *on
     C                   endif

     C                   return    *off

     PChkForRec        e
