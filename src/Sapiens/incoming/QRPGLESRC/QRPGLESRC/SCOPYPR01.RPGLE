    D**********************************************************************
    D*  Revisions:
     D*  01/28/10  C:XXX Project: SY215 - Change Brand Name
     D*            C:189
    D**********************************************************************
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D modea           c                   const('-- Add --')
     D modec           c                   const('- Change -')
     D moded           c                   const('-Display-')
      ****** BEG CHG ****** 02/08/06 **************************************
     D***** pdsnam          c                   const('Progressive Data So-
     D*****                                     lutions, Inc.')
     D pdsnam          c                   const('Fiserv Insurance So-
     D                                     lutions, Inc.')
      ****** END CHG ****** 02/08/06 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##conm          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D f3reqd          s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    C****************************************************************
    C*
    C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
    C*
    C*  71 - These indicators are to used as error indicators for
    C*  ||   the various screens. They will set off after
    C*  89 - displaying each input capible screen.
    C*  90 - Used to general purpose error indicator
    C*  91 - Error occurred when calling API to get descriptions
    C*
    C****************************************************************
    C*
    C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  FINISH - Final processing for prompt
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*
    C****************************************************************
    C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  DSCAPI - Call API for getting descriptions (use 91 on error)
    C*  ERRCK1 - Process F keys & determine errors on the first screen
    C*  PGMSTR - Used for any start of program processing (one-time)
    C*  PREDSP - Pre-processing before displaying (every time)
    C*  PRSCTL - Process control after no errors found
    C*
    C****************************************************************
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C                   movel     q1user        scuser
     C                   move      *off          f3reqd
     C                   exsr      getcof
     C                   exsr      pgmstr
    C*
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
     C                   exsr      predsp
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
     C                   exsr      pgmend
    C*
     C                   eval      *inlr = *on
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C                   exsr      prsctl
B01  C                   if        f3reqd = *on
     C                   move      *on           *in90
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   move      *on           *in91
     C                   exsr      dscapi
B01  C                   if        *in91 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       ##data = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
