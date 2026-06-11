     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('W0572R')
     H*********************************************************************
     H*  Work with Additinal Claimaints
     H*********************************************************************
     H* Created: 10/05/09   C:33   Project: WC1770                        *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     Fw0572f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdcap     if   e             disk    rename(pca01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdcal1    if   e           k disk    rename(pca01:dbrcdf)
     F                                     infds(dbfds)
     Fwdcal     if   e           k disk    rename(pca01:pca00)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D#ofOptFn         c                   20
     D of              s              1    dim(#ofOptFn) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOptFn) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Additional Claimants'
     D slmsgF6         c                   'WC39997'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0572R02'
     D comapi          c                   'WA001R'
     D ClmAPI          c                   'WA006R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D ##rrn2          s              9  0
     D ##Case          s                   like(CMCASE)
     D ##Yr            s                   like(CMFYR)
     D @@rrn           s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D kkCo#           s                   like(CACO#)
     D kkFnd           s                   like(CAFND)
     D kkFyr           s                   like(CAFYR)
     D kkCase          s                   like(CACASE)
     D kkLsN           s                   like(CALSN)
     D kkFsN           s                   like(CAFSN)
     D kkseq#          s                   like(CASEQ#)
     D emRRN           s                   like(@@rrn)
     D fnRRN           s                   like(@@rrn)
     D sv@@rrn         s                   like(@@rrn)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DclmLine1         ds            79
     D clmLine1a               1      6
     D clmLine1b               8     10
     D clmLine1c              12     71
     D*
     DclmLine2         ds            79
     D SCYRCS                  8     18
     D SCSS#                  26     36
     D SCCLNM                 39     58
     D SCACDT                 69     78

     DLoadAddress      pr                  like(scData2)
     DLoadPhone        pr            13
     DMoreThan4Act     pr             1n

     D/copy sPrMessage
     D/copy wPrGroup
     D/copy wPrEmplr

     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@kc
     C                   parm                    @@kl
    C*
    C*  Add/change/display program parameter list
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      sv@@rrn       ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C*  Claim line API
     C     ClmPl         plist
     C                   parm      CMCO#         ##Co#
     C                   parm      CMFND         ##Fnd
     C                   parm      CMFYR         ##Yr
     C                   parm      CMCASE        ##Case
     C                   parm                    clmLine1
     C                   parm                    clmLine2
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkFyr
     C                   kfld                    kkCase
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkFyr
     C                   kfld                    kkCase
     C                   kfld                    kkLsN
     C                   kfld                    kkFsN
     C                   kfld                    kkSeq#
     C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   select
     C                   when      ##Mode = '4'
     C                   callp     sndPgmMsg(slmsgF6 : '*INFO' : slmsgf :
     C                             '*LIBL' )
     C                   clear                   ##Mode
     C                   other
     C                   eval      *in11 = ssStat <> *blanks
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C                   select
     C*_________
     C*
     C                   when      scSel = '20'
     C                   select
     C                   when      scStat = 'I' and MoreThan4Act
     C                   seton                                        71  90
     C                   leavesr
     C                   endsl
     C*_________
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
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
     C     @@rrn         chain     wmcmp
     C*
     C                   eval      kkCo#  = CMCO#
     C                   eval      kkFnd  = CMFND
     C                   eval      kkFyr  = CMFYR
     C                   eval      kkCase = CMCASE
     C                   eval      kkLsN  = *loval
     C                   eval      kkFsN  = *loval
     C                   eval      kkSeq# = *loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   select
     C                   when      *in21
     C                   eval      kkLsn  = scPsT1
     C                   eval      kkFsN  = scPsT1a
     C                   eval      kkSeq# = *loval
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkLsn  = scLsN
     C                   eval      kkFsN  = scFsN
     C                   eval      kkSeq# = scSeq#
     C*
     C                   endsr
    C*================================================================
    C* Load screen headings
    C*================================================================
     C     LoadHdgs      begsr
     C*
     C* If claim was converted from another system, display old claim #
     C                   if        CMCVT# <> *blank
     C                   eval      scCvt = CMCVT#
     C                   eval      *in64 = *on
     C                   endif
     C*
     C* Agency Claim # (or, State Claim #)
     C                   eval      scSCl# = CMACL#
     C*
     C* Claims API
     C*
     C                   call      ClmAPI        ClmPl
     C*
     C* Get employer name
     C                   eval      scEDN = EmpDivName(CMEMP#
     C                                               :CMDIV
     C                                               :emRRN
     C                                               :CMFND
     C                                               :CMCO#
     C                                               )
     C*
     C* Get group name
     C                   eval      scFnd  = CMFND
     C                   eval      scFNam = GetGrpName(CMFND
     C                                                :fnRRN
     C                                                :CMCO#
     C                                                )
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scLsN  = CALSN
     C                   eval      scFsN  = CAFSN
     C                   eval      scAdr1 = CAADR1
     C                   eval      scAdr2 = CAADR2
     C                   eval      scCty  = CACTY
     C                   eval      scSt   = CAST
     C                   eval      scZip  = CAZIP
     C                   eval      scStat = CASTAT
     C                   eval      scSeq# = CASEQ#
     C*
     C                   select
     C                   when      *in21
     C                   eval      %subst(scData: 1:30) = CALSN
     C                   eval      %subst(scData:33:15) = CAFSN
     C                   eval      %subst(scData:50:15) = CAMID
     C                   eval      %subst(scData:54:13) = LoadPhone
     C                   eval      %subst(scData:72: 1) = CASTAT
     C                   endsl
     C*
     C                   eval      scData2 = LoadAddress
     C*
     C                   endsr
     C*================================================================
     C* Option 20 = Change Status
     C*================================================================
     C     Opt20         begsr
     C*
     C                   eval      ##Mode = 'S'
     C                   call      ACDPgm        ACDPL                  99
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scSel         caseq     '20'          Opt20                    90
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   if        %parms >= 3
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >= 4
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     @@rrn         chain     wmcmp
     C*
     C                   exsr      LoadHdgs
     C*
     C                   eval      sv@@rrn = @@rrn
     C                   clear                   @@rrn
     C*
     C                   eval      *in12 = *on
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
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
     C                   select
     C                   when      ssStat <> *blanks and CASTAT <> ssStat
     C                   eval      meetSS = *off
     C                   endsl
     C*
     C                   endsr





     P*====================================================================
     P* Load the address into one field
     P*====================================================================
     PLoadAddress      b
     DLoadAddress      pi                  like(scData2)

     DinZip            s              9
     DoutAddress       s                   like(scData2) inz

     C                   if        CAADR1 = *blanks
     C                   return    *blanks
     C                   endif
     C*
     C                   eval      inZip = %editc(CAZIP:'Z')
     C*
     C                   eval      outAddress = %trim(outAddress)
     C                                        + %trim(CAADR1)
     C*
     C                   if        CAADR2 <> *blanks
     C                   eval      outAddress = %trim(outAddress)
     C                                        + ', '
     C                                        + %trim(CAADR2)
     C                   endif
     C*
     C                   select
     C*__________
     C                   when      CACTY <> *blank
     C                   eval      outAddress = %trim(outAddress)
     C                                        + ', '
     C                                        + %trim(CACTY)
     C*
     C                   eval      outAddress = %trim(outAddress)
     C                                        + ', '
     C                                        + %trim(CAST)
     C*
     C                   eval      outAddress = %trim(outAddress)
     C                                        + '  '
     C                                        + %subst(inZip:1:5)
     C                   if        %subst(inZip:6:4) <> '0000'
     C                   eval      outAddress = %trim(outAddress)
     C                                        + '-'
     C                                        + %subst(inZip:6:4)
     C                   endif
     C*__________
     C                   when      CAADR3 <> *blank
     C                   eval      outAddress = %trim(outAddress)
     C                                        + ', '
     C                                        + %trim(CAADR3)
     C*
     C                   eval      outAddress = %trim(outAddress)
     C                                        + ', '
     C                                        + %trim(CACTRY)
     C*__________
     C                   endsl
     C*
     C                   return    outAddress

     PLoadAddress      e





     P*====================================================================
     P* Load the phone # into one field
     P*====================================================================
     PLoadPhone        b
     DLoadPhone        pi            13

     DinPhone          s              7
     DoutPhone         s             13    inz

     C                   eval      inPhone = %editc(CAPHN#:'Z')
     C*
     C                   if        inPhone = *blanks
     C                   return    *blanks
     C                   endif
     C*
     C                   eval      outPhone = '('
     C                                      + %editc(CAAREA:'Z')
     C                                      + ')'
     C                                      + %subst(inPhone:1:3)
     C                                      + '-'
     C                                      + %subst(inPhone:4:4)
     C*
     C                   return    outPhone

     PLoadPhone        e





     P*====================================================================
     P* Determine if there are more than 4 active claimant records
     P*====================================================================
     PMoreThan4Act     b
     DMoreThan4Act     pi             1n

     Dcount            s              1  0 inz

     C     key           setll     pca00
     C                   dou       %eof or count > 4
     C     key           reade     pca00
     C                   if        CASTAT = 'A'
     C                   eval      count = count + 1
     C                   endif
     C                   enddo
     C*
     C                   return    count > 4

     PMoreThan4Act     e





** Column headings
Opt  Last                            First            MI  Phone            Sts
** DStsription--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
215F15=Fold/Truncate        YYY
217F17=Subset               NNN
3
