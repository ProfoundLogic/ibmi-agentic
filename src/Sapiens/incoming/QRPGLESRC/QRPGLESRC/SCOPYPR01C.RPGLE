    ***********************************************************************
    **
    **  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
    **
    **  71 - These indicators are to used as error indicators for
    **  ||   the various screens. They will set off after
    **  89 - displaying each input capible screen.
    **  90 - Used to general purpose error indicator
    **  91 - Error occurred when calling API to get descriptions
    **
    ***********************************************************************
    **
    **  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
    **
    **  FINISH - Final processing for prompt
    **  GETCOF - Gets company and fund names if possible; otherwise
    **           get PDS name for company name
    **
    ***********************************************************************
    **  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
    **
    **  DSCAPI - Call API for getting descriptions (use 91 on error)
    **  ERRCK1 - Process F keys & determine errors on the first screen
    **  PGMSTR - Used for any start of program processing (one-time)
    **  PREDSP - Pre-processing before displaying (every time)
    **  PRSCTL - Process control after no errors found
    **
    ***********************************************************************
    C*
      /if Defined(CONVERT)
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      /Endif

     C                   move      *off          f3reqd
    C*
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
     C                   exsr      othfks
     C  n90              exsr      errck1
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif
E01  C                   enddo
    C*
     C                   exsr      pgmend
    C*
     C                   eval      *inlr = *on
    C*
    C*====================================================================
    C*  FINISH - Final processing for file update/display
    C*====================================================================
     C     FINISH        begsr
     C                   exsr      prsctl
B01  C                   if        f3reqd = *on
     C                   move      *on           *in90
E01  C                   endif
     C                   endsr
    C*====================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*====================================================================
     C     GETCOF        begsr
     C                   move      *on           *in91
     C                   exsr      dscapi
B01  C                   if        *in91 = *on
     C                   movel(p)  pdsnam        ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
