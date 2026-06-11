     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
    H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
    H*====================================================================
     H*  Revisions:
     H*
    H*====================================================================
     Fw0716f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdmsp     if   e             disk    rename(pms01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdmsl     if   e           k disk    rename(pms01:dbrcdf)
     F                                     infds(dbfds)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     F                                     infds(cmfds)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Multiple Settlements'
     D slmsgF6         c                   'WC39997'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0716R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##Case          s                   like(CMCASE)
     D ##Co#           s                   like(CMCO#)
     D ##CoNm          s             40
     D ##FNam          s             40
     D ##Fnd           s                   like(CMFND)
     D ##Fyr           s                   like(CMFYR)
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(msco#)
     D kkfnd           s                   like(msfnd)
     D kkfyr           s                   like(msfyr)
     D kkcase          s                   like(mscase)
     D fnRRN           s              9  0
     D emRRN           s              9  0
     D ClaimAPI        c                   const('WA006R')
     D*
     D cmfds           ds
     D  cmRRN                397    400b 0
     D*
     D                 ds
     D line1                   1     79
     D  line1a                 1      6
     D  line1b                 8     10
     D  line1c                12     71
     D*
     D                 ds
     D line2                   1     79
     D  scYrCs                 8     18
     D  scSS#                 26     36
     D  scClNm                39     58
     D  scAcDt                69     78
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     D/copy wPrGroup
     D/copy wPrEmplr
    D*
     D/copy sCopyWW42c
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@kc
     C                   parm                    @@kl
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C*  Claim line API
     C     ClaimPl       plist
     C                   parm      CMCO#         ##Co#
     C                   parm      CMFND         ##Fnd
     C                   parm      CMFYR         ##Fyr
     C                   parm      CMCASE        ##Case
     C                   parm                    line1
     C                   parm                    line2
     C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkFyr
     C                   kfld                    kkCase
     C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
     C     chkssm        begsr
     C*
     C                   select
     C                   when      ##Mode = '4'
     C                   callp     sndPgmMsg(slmsgF6 : '*INFO' : slmsgf :
     C                             '*LIBL' )
     C                   clear                   ##Mode
     C                   other
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
     C     errck1        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
     C     errck2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
     C     inzsst        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
     C     keyinz        begsr
     C*
     C* Chain out to claim file
     C     @@rrn         chain     wmcmp
     C*
     C                   eval      kkCo#  = CMCO#
     C                   eval      kkFnd  = CMFND
     C                   eval      kkFyr  = CMFYR
     C                   eval      kkCase = CMCASE
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
     C     keyset        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
     C     keysfl        begsr
     C*
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scCaMs = MSCAMS
     C                   eval      scTpOA = MSTPOA
     C                   eval      scTpOD = MSTPOD
     C                   eval      scDlyD = MSDLYD
     C*
     C                   select
     C                   when      info# = 1 and view# = 1
     C                   eval      %subst(scData: 6) = MSCAMS
     C                   eval      %subst(scData:15) = %editc(MSTPOA:'J')
     C                   eval      %subst(scData:33) =
     C                                          AlphaUSA(CvtToDate(MSTPOD))
     C                   eval      %subst(scData:55) =
     C                                          AlphaUSA(CvtToDate(MSDLYD))
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
     C     othfks        begsr
     C                   endsr
     C*
    C*====================================================================
    C* OTHOPT - Process other options
    C*====================================================================
     C     othopt        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
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
     C                   endsr
     C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C     pgmstr        begsr
     C*
     C*  If claim was converted from another system, display old claim #
     C                   if        CMCVT# <> *blank
     C                   eval      scCvt = CMCVT#
     C                   eval      *in64 = *on
     C                   endif
     C*
     C*  Agency claim # (or, state claim #)
     C                   eval      scSCl# = CMACL#
     C*
     C*  Call claims API
     C                   call      ClaimAPI      ClaimPl
     C*
     C*  Get employer name
     C                   eval      scEDN = EmpDivName(CMEMP#:
     C                                                CMDIV:
     C                                                emRRN:
     C                                                CMFND:
     C                                                CMCO#)
     C*
     C*  Get group name
     C                   eval      scFnd = CMFND
     C                   eval      scFNam = GetGrpName(CMFND:
     C                                                 fnRRN:
     C                                                 CMCO#)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
     C     sf1upd        begsr
     C*
     C                   eval      scData = '* * * Deleted'
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
     C     slctrd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt   Settlement      TPOC Amount    TPOC Date   Funding Delayed Start Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
