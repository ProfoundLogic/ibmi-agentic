    D**********************************************************************
    D*  Revisions:
     D*
     D*  08/09/10  C:191 Remove /Copy to avoid nested /copy error in SQLRPGLE
     D*
     D*  01/28/10  C:XXX Project: SY215 - Change Brand Name
     D*            C:189
     D*  09/06/06  C:61  Retrieve Client ID. (CLTID)
     D*
    D*  01/13/00  C:69  Created RPG/IV version of work-with /COPY
    D*
    D**********************************************************************
      /if not Defined(CONVERT)
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
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** pdsnam       c                   const('Progressive Data So-
     D***                                  lutions, Inc.')
     D pdsnam          c                   const('Stoneriver, Inc.')
     D*** END CHG   ********* 01/28/10 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D cpyflg          s              1
     D dsptyp          s              1
     D outflg          s              1
     D savedt          s              7  0
     D savetm          s              6  0
     D scuser          s             10
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D*** BEGIN CHG ****** 08/09/10 ************************************
     D*******/copy SPrRtvBrnd
     DRtvBrnd          pr                  extPgm('RTVBRND')
     D outType                        1    const
     D inLine1                       80
     D inLine2                       80    options(*nopass)
     D outCo#                         3    options(*nopass)
     D*** END   CHG ****** 08/09/10 ************************************
     D***  End   Add  ***  01/28/10  **************************************
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      /Endif
    C*
    C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
    C*
    C*  09 - Reserved for maintenance/inquiry conditions
    C*  71 - These indicators are to used as error indicators for
    C* thru  the various screens. They will set off after
    C*  89 - displaying each input capible screen.
    C*  90 - Used to general purpose error indicator
    C*  99 - Used when chaining to file being updated
    C*
    C****************************************************************
    C*
    C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  FINISH - Final processing for file update/display
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*  PREADD - Pre-processing before add process
    C*  PRECHG - Pre-processing before change process
    C*  PREDLT - Pre-processing before delete process
    C*  PREDSP - Pre-processing before display process
    C*  XSCR02 - Display second screen and edit check
    C*  XSCR03 - Display third screen and edit check
    C*
    C****************************************************************
    C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  DLTPRE - Used for any processing before delete occurs
    C*  DSPLYR - After displaying information
    C*  ERRCK1 - Used to determine errors on the first screen
    C*  ERRCK2 - Used to determine errors on the second screen
    C*  ERRCK3 - Used to determine errors on the third screen
    C*  MVDBSC - Used to move data base fields to screen fields
    C*  MVSCDB - Used to move screen fields to data base fields
    C*  OTHERR - Used for processing other modes
    C*  PGMEND - Used for any end of program processing
    C*  PGMSTR - Used for any start of program processing
    C*  PREOTH - Pre-processing for other modes
    C*  UPDATR - Used to update data base record
    C*  WRITER - Used to write data base record
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
     ****** beg add ****** 09/06/06 *******************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     ****** end add ****** 09/06/06 *******************************
     C                   movel     q1user        scuser
     C                   move      *off          *inkl
     C                   move      *off          outflg
     C                   move      *off          dsptyp
B01  C                   if        @@mode = 'D'
     C                   move      *on           *in09
X01  C                   else
     C                   move      *off          *in09
E01  C                   endif
     C                   move      *off          cpyflg
     C                   if        @@mode = 'P'
     C                   move      *on           cpyflg
     C                   endif
     C                   exsr      getcof
     C                   exsr      getpgt
     C                   exsr      pgmstr
    C*
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or outflg = *on
     C                   exsr      refrsh
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B02  C                   dou       *in90 = *off
     C                   exfmt     scr01
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B04  C                   if        *inkc = *off
     C                             and *inkl = *off
B04  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck1
E04  C                   endif
B04  C                   if        *in90 = *off
B05  C                   if        #ofscr = 1
     C                   exsr      finish
X05  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr02
E05  C                   endif
E04  C                   endif
E03  C                   endif
E03  C                   endif
E02  C                   enddo
E01  C                   enddo
     C                   exsr      pgmend
    C*
     C                   return
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C     @@mode        caseq     'A'           writer
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C     @@mode        caseq     'P'           writer
     C                   cas                     otherr
     C                   endcs
B01  C                   if        @@mode <> 'A'
     C                   move      *on           outflg
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   call      comapi        compl                  99
B01  C                   if        *in99 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      ##data = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
    C*================================================================
    C*  GETDTM - Gets date/time before display screen and saves
    C*================================================================
     C     getdtm        begsr
     C                   time                    dsdttm
     C                   eval      savetm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         savedt
     C                   endsr
    C*================================================================
    C*  GETPGT - Get program title for display heading
    C*================================================================
     C     getpgt        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     C                   endsr
    C*================================================================
    C*  KEDO   - Process F5; refresh
    C*================================================================
     C     kedo          begsr
     C                   exsr      refrsh
     C                   move      *on           *in90
     C                   endsr
    C*================================================================
    C*  PREADD - Pre-processing before add process
    C*================================================================
     C     preadd        begsr
     C                   if        cpyflg = *off
     C                   clear                   dbrcdf
     C                   else
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   endif
     C                   movel(p)  modea         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  PRECHG - Pre-processing before change process
    C*================================================================
     C     prechg        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   movel(p)  modec         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  PREDLT - Pre-processing before delete process
    C*================================================================
     C     predlt        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   exsr      dltpre
     C     @@rrn         delete    dbrcdf                             99
     C                   exsr      pgmend
     C                   return
     C                   endsr
    C*================================================================
    C*  PREDSP - Pre-processing before display process
    C*================================================================
     C     predsp        begsr
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   movel(p)  moded         scmode
     C                   exsr      mvdbsc
     C                   endsr
    C*================================================================
    C*  REFRSH - Refresh screen values and date/time (1st time in/F5)
    C*================================================================
     C     refrsh        begsr
     C     @@mode        caseq     'A'           preadd
     C     @@mode        caseq     'C'           prechg
     C     @@mode        caseq     'D'           predsp
     C     @@mode        caseq     'E'           predlt
     C     @@mode        caseq     'P'           preadd
     C                   cas                     preoth
     C                   endcs
     C                   exsr      getdtm
     C                   endsr
    C*================================================================
    C*  REFRS2 - Refresh screen values and date/time (on screen chg)
    C*================================================================
     C     refrs2        begsr
     C     @@mode        caseq     'C'           prechg
     C     @@mode        caseq     'D'           predsp
     C                   cas                     preoth
     C                   endcs
     C                   exsr      getdtm
     C                   endsr
    C*================================================================
    C*  XSCR02 - Display second screen and edit check
    C*================================================================
     C     xscr02        begsr
     C                   exsr      refrs2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C                   dou       *in90 = *off
     C                   exfmt     scr02
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck2
E03  C                   endif
B03  C                   if        *in90 = *off
B04  C                   if        #ofscr = 2
     C                   exsr      finish
X04  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr03
E04  C                   endif
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  XSCR03 - Display third screen and edit check
    C*================================================================
     C     xscr03        begsr
     C                   exsr      refrs2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl3
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C                   dou       *in90 = *off
     C                   exfmt     scr03
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck3
E03  C                   endif
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
