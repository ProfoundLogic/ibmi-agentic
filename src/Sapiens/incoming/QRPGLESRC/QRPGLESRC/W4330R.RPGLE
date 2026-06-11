     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H* ***************************************************************
     H actgrp(*caller)
     H/TITLE W4330R  - TREATING PHYSICIAN HISTORY MAINTENANCE
     H Bnddir('WTBNDDIR')
     H* ***************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     Work with PHYSICIAN HISTORY FILE                        **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W0070R10 - F19                                          **
     H**   CALLS TO:                                                 **
     H**     W4330R02                                                **
    H*
     H* CREATED BY:   220                                             *
     H* PROJECT:      25WPV             03/26/10                      *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* ***************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw4330f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmtpp     if   e             disk
     F                                     rename(ptp01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmtpl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptp01:dbrcdf)
     Fsmcol     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmpdl     if   e           k disk
     Fwmcml1    if   e           k disk
     F                                     infds(dbfds2)
     Fwmupl     if   e           k disk
     Fwmeml     if   e           k disk
      *  System Control
     Fssylf     if   e           k disk
     F                                     ignore(spc01)
     F                                     ignore(sfb01)
     F                                     ignore(src01)
     F                                     ignore(sdr01)
     F                                     ignore(sbc01)
     F                                     ignore(scs01)
     F                                     ignore(sgl01)
     F                                     ignore(sp201)
     F*  GENERIC TABLE FILE
     Fwtgtl     if   e           k disk
      *
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Treating Physician'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W4330R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
     D xcbc01          c                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D xcbc02          c                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D xcexre          c                   CONST('EXCS REIN ')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(tpco#)
     D kkfnd           s                   like(tpfnd)
     D kkfyr           s                   like(tpfyr)
     D kkcase          s                   like(tpcase)
     D kkseq#          s                   like(tpseq#)
    D*---------------------------------------------------------------------
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D xxdat           s             10
     D scsds           ds
     D  xfdat                  1     10
     D  xfhyp                 12     12    inz('-')
     D  xtdat                 14     23
     D  xphyn                 26     47
     D  xprol                 51     52
     D  xclin                 57     76
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
     C                   parm                    qqseq#            5
     C                   parm                    qqrtcd            1
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqseq#
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*                  kfld                    kkseq#
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     syky          klist
     C                   kfld                    xxsyk             2
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*
     C*  CLAIM MASTER DEMOGRAPHIC FILE
     C*
     C     cdky          klist
     C                   kfld                    xxss#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     coky          klist
     C                   kfld                    xxco#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
     C     upky          klist
     C                   kfld                    q1user
     C*
     C     gtky          klist
     C                   kfld                    xxco#             3 0
     C                   kfld                    xxtbid
     C                   kfld                    xxkey            10
     C*
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C     *inkj         ifeq      *on
     C                   exsr      kjdo
     C                   endif
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   movel     qqco#         kkco#
     C                   movel     qqfnd         kkfnd
     C                   movel     qqfyr         kkfyr
     C                   movel     qqcase        kkcase
     C                   movel     *loval        kkseq#
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C*                  clear                   kkco#
     C*                  clear                   kkfnd
     C*                  clear                   kkfyr
     C*                  clear                   kkcase
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   clear                   scdata
     C*
     C*    Find out the role of Phy
     C*
     C                   z-add     tpfrdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdat
     C                   if        xxdat <> *blanks
     C                   eval      xxdat = %subst(xxdat:1:2) + '/' +
     C                             %subst(xxdat:3:2) + '/' + %subst(xxdat:5:4)
     C                   movel     xxdat         xfdat
     C                   else
     C                   movel     *blanks       xfdat
     C                   endif
     C*
     C                   z-add     tptodt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdat
     C                   if        xxdat <> *blanks
     C                   eval      xxdat = %subst(xxdat:1:2) + '/' +
     C                             %subst(xxdat:3:2) + '/' + %subst(xxdat:5:4)
     C                   movel     xxdat         xtdat
     C                   else
     C                   movel     *blanks       xtdat
     C                   endif
     C*
     C                   movel     tpphyn        xphyn
     C                   movel     tpprol        xprol
     C                   movel     tpclin        xclin
     C*
     C                   if        tpedat = 'N'
     C                   movel     *blanks       xtdat
     C                   endif
     C*
     C                   movel     scsds         scdata
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     *like         define    cdss#         xxss#
     C*
     C                   clear                   scedn
     C                   clear                   scfnd
     C                   clear                   xcfyr
     C                   clear                   sccase
     C                   clear                   scprol
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     upky          chain     pup01                              60
     C     *in60         ifeq      '1'
     C                   move      *zero         upco#
     C                   endif
     C                   move      upco#         xxco#
     C*
     C                   move      @@call        qqcall            1
     C*
     C                   movel     qqfyr         xxfyr             3 0
     C                   z-add     xxfyr         xcfyr             3 0
     C                   movel     qqfnd         scfnd
     C                   movel     qqcase        sccase            7 0
     C                   movel     qqco#         xxco#             3 0
     C                   movel     qqfnd         xxfnd             3 0
     C                   movel     qqfyr         xxfyr             3 0
     C                   movel     qqcase        xxcase            7 0
     C*
     C*  GET THE CLAIMS MASTER (SOC SEC NUMBER)
     C*
     C     cmky          chain     pcm01                              60
     C                   move      cmss#         xxss#
     C                   z-add     dbrrn2        @1rrn             9 0
     C*
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt
     C*
     C*  GET THE CLAIMANT NAME
     C*
     C     cdky          chain     pcd01                              60
     C     *in60         ifeq      '0'
     C                   move      *blanks       qqfmn            15
     C                   move      *blanks       qqfsn            15
     C                   move      *blanks       qqmid            56
     C                   move      '1'           qqterm            1
     C                   movel     cdfsn         qqfsn
     C                   movel     cdmid         qqmid
     C                   call      'SRFRLS'
     C                   parm                    qqmid
     C                   parm                    qqfsn
     C                   parm                    qqterm
     C                   movel     qqmid         qqfmn
     C*
     C                   move      '1'           qqterm
     C                   move      *blanks       qqlsn            56
     C                   movel     cdlsn         qqlsn
     C                   call      'SRNAME'
     C                   parm                    qqlsn
     C                   parm                    qqfmn
     C                   parm                    qqterm
     C                   movel     qqlsn         scclnm
     C                   else
     C                   move      *blanks       scclnm
     C                   move      *zeros        scss#
     C                   endif
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     coky          chain     pco01                              60
     C                   movel(P)  coname        srdfld
     C                   call      'SRCENTER'
     C                   parm                    srdfld           80
     C                   parm      40            srdlen            3 0
     C                   movel     srdfld        scconm
     C     fnky          chain     pfn01                              60
     C                   movel     xxfnd         scfnd
     C                   movel     fnname        scname
     C     xcfyr         ifeq      *zero
     C                   z-add     upfyr         xcfyr
     C                   endif
     C*
     C                   movel(P)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data           80
     C                   parm      40            ##leng            3 0
     C                   movel     ##data        scprds
     C*
     C                   exsr      first
     C*
     C*  Reset the view indicator, if necessary
     C*
     C                   move      *off          *in09
     C     qqcall        ifeq      '1'
     C                   move      *on           *in09
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C**                 read      dbrcdf                                 95
     C     key1          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C**                 readp     dbrcdf                                 95
     C     key1          readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        scprol <> ' ' and scprol  <> tpprol
     C*
     C                   eval      meetss = *off
     C*
     C                   endif                                                 Status is A or I
     C                   endsr
     C*===============================================================
     C* FIRST  - SUBROUTINE FOR LOADING SCREEN HEADER FIELDS
     C*===============================================================
     C*
     C     first         begsr
     C*
     C                   move      qqfnd         xxfnd
     C                   move      cmemp#        svemp#            9 0
     C                   move      cmdiv         svdiv             5 0
     C                   move      cmemp#        xxemp#            9 0
     C                   move      cmdiv         xxdiv             5 0
     C                   move      qqcase        svcase            7 0
     C                   move      qqcase        xxcase
     C*
     C     emky          chain     pem01                              60
     C*
     C* CONCATENATE EMP #, DIV, & NAME
     C*
     C                   call      'WA005R'
     C                   parm                    cmemp#
     C                   parm                    cmdiv
     C                   parm                    emname
     C     scedn         parm                    xxedn            80
     C*
     C*  GET EXCESS REINSURANCE INFO
     C*
     C     cmexcs        ifne      'Y'
     C     @1rrn         ifne      *zero
     C                   call      'WA010R'
     C                   parm                    @1rrn             9 0          FOR WMCMP
     C                   parm                    xxfndr           13 2
     C                   parm                    xxexre            1
     C                   endif
     C                   endif
     C*
     C     xxexre        ifeq      'Y'
     C     cmexcs        oreq      'Y'
     C                   movel(P)  xcexre        scmsg1
     C                   move      *on           *in69
     C                   endif
     C*
     C* GET SYSTEM CONTROL FILE
     C*
     C                   movel(P)  'SY'          xxsyk             2
     C     syky          chain     ssy01                              60
     C     *in60         ifeq      *off
     C*
     C* BAR CODE INSTALLED
     C*
     C     sybcif        ifeq      'Y'
     C                   call      'BA001R'
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqbcfl            1
     C                   parm                    qqtrtp            1
     C                   parm                    qqtusr           10
     C                   parm                    qqrtcd            1
     C*
     C     qqbcfl        ifeq      'Y'
     C                   move      *on           *in68
     C     qqtrtp        ifeq      'I'
     C                   movel(P)  xcbc02        scmsg2
     C                   else
     C                   movel(P)  xcbc01        scmsg2
     C                   cat       qqtusr:1      scmsg2
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   endif                                                  *IN60
     C*
     C*  CLAIMANT SS#
     C*
     C     cmss#         ifne      *blank
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****ss1           cat(p)    '-':0         scss#
     C*****scss#         cat       ss2:0         scss#
     C*****scss#         cat       '-':0         scss#
     C*****scss#         cat       ss3:0         scss#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   endif
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     cmcvt#        ifne      *blank
     C                   movel     cmcvt#        sccvt
     C                   move      *on           *in64
     C                   endif
     C*
     C*  FYR-CASE
     C*
     C                   move(p)   cmfyr         xafyr             3
     C                   move(p)   cmcase        xacase            7
     C     xafyr         cat(p)    '-':0         scyrcs
     C     scyrcs        cat       xacase:0      scyrcs
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   move      *blanks       scscl#
     C                   movel     cmacl#        scscl#
     C*
      *
     C                   endsr
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     kjdo          begsr
     C*
     C                   select
     C*
     C     hfld          wheneq    'SCPROL'
     C                   MOVEL     'PHCD'        xxtbid
     C                   move      *blank        xxkey
     C                   call      'W2493R'
     C                   parm                    qqprds           40
     C                   parm                    qqcall            1
     C                   parm                    xxtbid            4
     C                   parm                    xxkey            10
     C                   parm                    qqrtcd            1
      *
     C     xxkey         ifeq      *blank
     C                   seton                                          4190     POS CUSR
     C                   endif
      *
     C     qqrtcd        ifeq      '*'
     C                   movel     xxkey         scprol
     C     gtky          chain     pgt01                              60
     C     *in60         ifeq      *off
     C     gtstat        andeq     'A'
     C                   movel     gtkey         scprol
     C                   seton                                            90     POS CUSR
     C                   endif
     C                   endif
     C*
     C                   other
     C*
     C                   endsl
     C*
     C                   endsr                                                    KJDO
     C*
      *
** Column headings
Opt  Dates of services        Treating physician name  Role  Clinic
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
