     Hcopyright('Fiserv, Inc., 2003-2008')                                                         *
     H/title - Work with Return Fund Batches
      *==================================================================================*
      *  DESCRIPTION:  Work with return fund batches.                                    *
      *                                                                                  *
      *  CALLED FROM: W0500R                                                             *
      *  CREATED: 03/12/2003  John Stone/Matt Dawson  Project: WC1033                    *
      *==================================================================================*
      *  REVISIONS:                                                                      *
      *                                                                                  *
      *  09/14/15 C:180 Support 34992
      *                 Display file change only - Increase size of
      *                 hidden employer name key field to allow
      *                 proper retrieval of records from database when
      *                 pagedown is pressed.
      *
      *  12/26/07 C:183 Project: 81WZB                                                   *
      *                -Added subset for Surplus Dividend, Employer # range, Agent#      *
      *                 range, customer ID range.                                        *
      *                                                                                  *
      *  07/18/07 C:136 Project: WC1491 (Return Dep & Ind Cash Distribution)             *
      *                -Add batch type to header.                                        *
      *                                                                                  *
      *  11/05/04  C:33  Project: WC1033                                                 *
      *                  Include calculations for Agency Billing.                        *
      *==================================================================================*
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW0501F    CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     Sfile(scr01s:sfrrn1)
     F                                     Sfile(scr02s:sfrrn2)
     F                                     Infds(fids01)
     *  Physical file definition (for refreshing subfile record)
     Fwxrtp     IF   E             Disk
     F                                     Rename(Prt01:Pfrcdf)
     *  Primary logical file definition (for displaying items by name)
     Fwxrtl2    IF   E           K Disk
     F                                     Infds(Dbfds)
     F                                     Rename(Prt01:Dbrcdf)
     *  Secondary logical files definition (for displaying items)
     Fwxrtl1    IF   E           K Disk
     F                                     Infds(Dbfds2)
     F                                     Rename(Prt01:Dbrcdf2)
     F***  Begin Add  ***  11/05/04  **************************************
     Fwxrtl8    if   e           k disk    infds(dbfdsa)
     F                                     rename(prt01:dbrcdfa)
     Fwxrtl7    if   e           k disk    infds(dbfds2a)
     F                                     rename(prt01:dbrcdf2a)
     F***  Begin Add  ***  11/05/04  **************************************
      * Return Premium Batch Control
     Fwxrnp     IF   E             Disk
     F                                     Infds(rninfds)
      * Employer Master
     Fwmeml     IF   E           K Disk
     F                                     Infds(eminfds)
      * Premium Finance
     Fwmpfl     IF   E           K Disk
     F                                     Infds(pfinfds)
      * Fund Master
     Fwmfnl     IF   E           K Disk
     F                                     Infds(fninfds)
      * Employer Detail
     Fwdell     IF   E           K Disk
     F                                     Infds(elinfds)
      * A/R File
     Fwmeal     IF   E           K Disk
     F***  Begin Add  ***  11/05/04  **************************************
     Fwmaal     if   e           k disk    rename(paa01:paa00)
     F***  End   Add  ***  11/05/04  **************************************
     D eminfds         ds
     D  emrrn                397    400b 0
     D pfinfds         ds
     D  pfrrn                397    400b 0
     D fninfds         ds
     D  fnrrn                397    400b 0
     D elinfds         ds
     D  elrrn                397    400b 0
     D rninfds         ds
     D  rnrrn                397    400b 0
     D***  Begin Chg  ***  11/05/04  **************************************
     D*hd*******       S             78    dim(2)  ctdata perrcd(1)
     D #head           c                   2
     D hd              S             78    dim(#head) ctdata perrcd(1)
     D hda             S             78    dim(#head) ctdata perrcd(1)
     D***  End   Chg  ***  11/05/04  **************************************
     D of              S              1    dim(15) ctdata perrcd(1)
     D ofd             S             35    dim(15) alt(of)
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           C                   'Return Fund Batch Detail'
     D slmsgi          C                   'WC39998'
     D slmsgf          C                   'WMESGG'
     D acdpgm          C                   'W0501R02'
     D comapi          C                   'WA001R'
     D uspapi          C                   'W0006R'
     D infomx          C                   1
     D viewmx          C                   2
     D sf1pag          C                   9
     D datusa          S               d   datfmt(*Usa)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D***  Begin Add  ***  11/05/04  **************************************
     D agency6         c                   '066=Agency billing stmts   YYN'
     D***  End   Add  ***  11/05/04  **************************************
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0

     D @@call          s              1
     D @@fnd           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fyr           s              3
     D @@date          s              8
     D @@amount        s              9
     D @@chk#          s              7
     D @@rtcd          s              1
     D @@rrn           s              9  0
     D***  Begin Add  ***  11/05/04  **************************************
     D @@mode          s              1
     D ##mode2         s                   like(@@mode)
     D agencyBill      s              1n
     D***  End   Add  ***  11/05/04  **************************************
     D kkco#           s                   like(Rtco#)
     D kkbat#          s                   like(Rtbat#)
     D kkfnd           s                   like(Rtfnd)
     D kkempn          S                   like(Rtempn)
     D kkemp#          S                   like(Rtemp#)
     D kkdiv           S                   like(Rtdiv)
     D kkfyr           S                   like(Rtfyr)
     D kkagtn          S                   like(Rtagtn)
     D kkagt#          S                   like(Rtagt#)
     D kkfein          S                   like(Rtfein)
     D kkmod           S                   like(Rtmod)
     D string1         s                   like(RTEMPN)
     D string2         s                   like(ssname)
     d strlen          s              3  0
     d start           s              3  0
     d patlen          s              3  0
     d trans           s              1
     d trim            s              1
     d wild            s              1
     d scnres          s              3  0
     d xxadj           s              1
     D*** BEG ADD **** 12/26/07 ********************************************************
     D @RTCUST         S                   LIKE(RTCUST)
     D @SSCUS1         S                   LIKE(RTCUST)
     D @SSCUS2         S                   LIKE(RTCUST)
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D*** END ADD **** 12/26/07 ********************************************************

     D
     D  view1          ds
     D  v1rp                   1      1
     D  v1empn                 4     24
     D  v1grp                 26     28
     D  v1emp#                30     38
     D  v1div                 40     44
     D  v1fyr                 46     48
     D  v1pamt                50     63
     D  v1ptfl                66     66
     D  v1adj                 70     70
     D  v1aud                 74     74
     D***  Begin Add  ***  11/05/04  **************************************
     D  v1agtn                 4     34
     D  v1fein                36     44
     D  v1mod                 46     48
     D***  End   Add  ***  11/05/04  **************************************
     D
     D  view2          ds
     D  v2rp                   1      1
     D  v2grp                  4      6
     D  v2emp#                 8     16
     D  v2div                 18     22
     D  v2fyr                 24     26
     D  v2empn                28     48
     D  v2pamt                50     63
     D  v2ptfl                66     66
     D  v2adj                 70     70
     D  v2aud                 74     74
     D***  Begin Add  ***  11/05/04  **************************************
     D  v2fein                 4     12
     D  v2mod                 14     16
     D  v2agtn                18     48
     D*
     D dbfdsa          ds
     D  dbrrna               397    400b 0
     D dbfds2a         ds
     D  dbrrn2a              397    400b 0
     D***  End   Add  ***  11/05/04  **************************************
      *
     C/copy sprstring
     C/copy scopyww01c
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@call
     C                   parm                    @@rrn             9 0
     C***  Begin Add  ***  11/05/04  **************************************
     C*** beg del ***** 07/18/07 ******************************************
     C*****************  parm                    @@mode            1
     C*** end del ***** 07/18/07 ******************************************
     C***  End   Add  ***  11/05/04  **************************************
     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      rnrrn         ##rrn2
     C*** Begin Add  ***  11/05/04  ***************************************
     C                   parm      @@mode        ##mode2
     C*** Begin Add  ***  11/05/04  ***************************************
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *Blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      *Blanks       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
      *
     C     v1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
     C                   kfld                    kkempn
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     v1kya         klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
     C                   kfld                    kkagtn
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     v2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     v2kya         klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     eaky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTEMP#
     C                   kfld                    RTDIV
     C                   kfld                    RTFYR
      *
     C     eaky1         klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTEMP#
     C                   kfld                    RTDIV
     C                   kfld                    RTFYR
     C                   kfld                    RTIDT
     C                   kfld                    RTITIM
      *
     C     emky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTEMP#
     C                   kfld                    RTDIV
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     keyWMAAL      klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFEIN
     C                   kfld                    RTMOD
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     elky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTEMP#
     C                   kfld                    RTDIV
     C                   kfld                    RTFYR
      *
     C     fnky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
      *
     C     pfky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTPFIN
      *
     C     pfky1         klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTPFIN
     C                   kfld                    kkefdt            7 0
     *================================================================
     * CHKSSM - Set indicator 11 for subset message
     *================================================================
     C     chkssm        begsr
     C                   eval      *in11 =  (ssselc <> *blank or
     C                                       ssname <> *blank or
     C                                       ssfnd  <> *zero  or
     C***  Begin Add  ***  11/05/04  **************************************
     C                                       ssfein <> *zero  or
     C***  End   Add  ***  11/05/04  **************************************
     C                                       ssemp# <> *zero)
     C                   endsr
     *================================================================
     * ERRCK1 - Additional errors on subfile selection
     *================================================================
     C     errck1        begsr
     C                   if        scsel =  '7' and
     C                             RTPTFL <> 'P'
     C                   seton                                        71  90
     C                   endif
     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        begsr
     C***  BEG ADD ******  12/26/07  **************************************
      *
      * Convert SSCUS1 and SSCUS2 to Upper case
      *
     C                   EVAL      @SSCUS1 = %XLATE(LO:UP:SSCUS1)
     C                   EVAL      @SSCUS2 = %XLATE(LO:UP:SSCUS2)
      *
     C                   SELECT
      *
     C                   WHEN      SSSELC <> ' ' AND SSSELC <> 'Y'
     C                             AND SSSELC <> 'N'
     C                   SETON                                        38  90
      *
     C                   WHEN      SSCUS1 <> *BLANKS AND SSCUS2 = *BLANKS
     C                   SETON                                        33  90
      *
     C                   WHEN      SSCUS2 <> *BLANKS AND SSCUS1 = *BLANKS
     C                   SETON                                        32  90
      *
     C                   WHEN      @SSCUS2 < @SSCUS1
     C                   SETON                                        39  90
      *
     C                   WHEN      SSEMP1 <> *ZERO AND SSEMP2 = *ZERO
     C                   SETON                                        35  90
      *
     C                   WHEN      SSEMP2 <> *ZERO AND SSEMP1 = *ZERO
     C                   SETON                                        34  90
      *
     C                   WHEN      SSEMP2 < SSEMP1
     C                   SETON                                        40  90
      *
     C                   WHEN      SSAGT1 <> *ZERO AND SSAGT2 = *ZERO
     C                   SETON                                        37  90
      *
     C                   WHEN      SSAGT2 <> *ZERO AND SSAGT1 = *ZERO
     C                   SETON                                        36  90
      *
     C                   WHEN      SSAGT2 < SSAGT1
     C                   SETON                                        41  90
      *
     C                   WHEN      SSSDFL <> 'Y' AND SSSDFL <> 'N' AND
     C                             SSSDFL <> ' ' AND CLTID = '081'
     C                   SETON                                        42  90
      *
     C                   ENDSL
     C***  END ADD ******  12/26/07  **************************************
     C                   endsr
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   clear                   ssfnd
     C                   clear                   ssemp#
     C                   clear                   ssfein
     C                   clear                   ssname
     C                   clear                   ssselc
     C***  End   Add  ***  11/05/04  **************************************
     C***  BEG ADD ******  12/26/07  **************************************
     C                   clear                   sscus1
     C                   clear                   sscus2
     C                   clear                   ssemp1
     C                   clear                   ssemp2
     C                   clear                   ssagt1
     C                   clear                   ssagt2
     C                   clear                   sssdfl
     C***  END ADD ******  12/26/07  **************************************
     C*
     C                   endsr
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     keyinz        begsr
     C                   move      *loval        kkempn
     C                   move      *loval        kkfnd
     C                   move      *loval        kkemp#
     C                   move      *loval        kkdiv
     C                   move      *loval        kkfyr
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      kkagtn = *loval
     C                   eval      kkfein = *loval
     C                   eval      kkmod  = *loval
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   endsr
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     Keyset        begsr
     C                   select
     C                   when      *in21
     C                   movel(p)  scpst1        kkempn
     C***  Begin Add  ***  11/05/04  **************************************
     C                   movel(p)  scpst1        kkagtn
     C***  End   Add  ***  11/05/04  **************************************
     C                   when      *in22
     C                   move(p)   scpst2        kkfnd
     C                   eval      kkemp# = scpsta
     C                   eval      kkdiv  = scpstb
     C***  Begin Add  ***  11/05/04  **************************************
     C                   movel(p)  scpst9        kkfein
     C                   movel(p)  scpstm        kkmod
     C***  End   Add  ***  11/05/04  **************************************
     C                   other
     C                   endsl
     C                   endsr
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     keysfl        begsr
     C                   eval      kkempn = sfempn
     C                   eval      kkfnd  = sfgrp
     C                   eval      kkemp# = sfemp#
     C                   eval      kkdiv  = sfdiv
     C                   eval      kkfyr  = sffyr
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      kkagtn = sfagtn
     C                   eval      kkfein = sffein
     C                   eval      kkmod  = sfmod
     C***  End   Add  ***  11/05/04  **************************************
     C                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     Mvdbsc        begsr

     C                   eval      sfempn  = RTEMPN
     C                   eval      sfemp#  = RTEMP#
     C                   eval      sfgrp   = RTFND
     C                   eval      sfemp#  = RTEMP#
     C                   eval      sfdiv   = RTDIV
     C                   eval      sffyr   = RTFYR
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      sfagtn = RTAGTN
     C                   eval      sffein = RTFEIN
     C                   eval      sfmod  = RTMOD
     C***  End   Add  ***  11/05/04  **************************************

     C     eaky1         setgt     pea01
     C     eaky          reade     pea01
     C                   if        %eof
     C                   eval      xxadj = *blank
     C                   else
     C                   eval      xxadj = 'Y'
     C                   endif

     C                   select
     C*__________
     C*
     C                   when      *in21
     C***  Begin Add  ***  11/05/04  **************************************
     C                   clear                   view1
     C*
     C                   select
     C*_______________
     C*
     C                   when      agencyBill
     C                   eval      scdbrr  = dbrrna
     C                   eval      v1rp    = RTSELC
     C                   eval      v1agtn  = RTAGTN
     C                   eval      v1fein  = %editc(RTFEIN:'3')
     C                   eval      v1mod   = %editc(RTMOD :'3')
     C                   eval      v1pamt  = %editc(RTAMT:'2')
     C*_______________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      v1rp    = RTSELC
     C                   eval      v1empn  = RTEMPN
     C                   eval      v1grp   = %editc(RTFND:'Z')
     C                   eval      v1emp#  = %editc(RTEMP#:'Z')
     C                   eval      v1div   = %editc(RTDIV:'Z')
     C                   eval      v1fyr   = %editc(RTFYR:'Z')
     C                   eval      v1pamt  = %editc(RTAMT:'2')
     C                   eval      v1ptfl  = RTPTFL
     C                   eval      v1adj   = xxadj
     C                   eval      v1aud   = RTCAFL
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   movel(p)  view1         scdata
     C*__________
     C*
     C                   when      *in22
     C***  Begin Add  ***  11/05/04  **************************************
     C                   clear                   view2
     C*
     C                   select
     C*_______________
     C*
     C                   when      agencyBill
     C                   eval      scdbrr  = dbrrn2a
     C                   eval      v2rp    = RTSELC
     C                   eval      v2agtn  = RTAGTN
     C                   eval      v2fein  = %editc(RTFEIN:'3')
     C                   eval      v2mod   = %editc(RTMOD :'3')
     C                   eval      v2pamt  = %editc(RTAMT:'2')
     C*_______________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      v2rp    = RTSELC
     C                   eval      v2empn  = RTEMPN
     C                   eval      v2grp   = %editc(RTFND:'Z')
     C                   eval      v2emp#  = %editc(RTEMP#:'Z')
     C                   eval      v2div   = %editc(RTDIV:'Z')
     C                   eval      v2fyr   = %editc(RTFYR:'Z')
     C                   eval      v2pamt  = %editc(RTAMT:'2')
     C                   eval      v2ptfl  = RTPTFL
     C                   eval      v2adj   = xxadj
     C                   eval      v2aud   = RTCAFL
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   movel(p)  view2         scdata
     C*
     C                   endsl

     C                   endsr
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     othfks        begsr
     C                   endsr
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        begsr

     C     scsel         caseq     '6'           opt6
     C     scsel         caseq     '7'           opt7
     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '60'          opt60
     C                   endcs

     C                   endsr
     *================================================================
     * Option 6 - Call W/W Employers
     *================================================================
     C     opt6          begsr

     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C     scdbrr        chain     pfrcdf
     C     keyWMAAL      chain     wmaal
     C*
     C                   if        %found
     C*
     C                   move      AAAGT#        ##agt#
     C*
     C                   call      'W3102R'
     C                   parm      '1'           ##call            1
     C                   parm                    ##agt#            5
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C*
     C                   endif
     C*__________
     C*
     C                   when      @@mode = 'E'
     C***  End   Add  ***  11/05/04  **************************************
     C     scdbrr        chain     pfrcdf
     C     emky          chain     pem01
     C                   if        %found
     C                   call      'W0050R'
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    ##kc              1
     C**********         parm                    ##kl              1
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C***  Begin Chg  ***  11/05/04  **************************************
     C                   parm      '1'           ##call            1
     C                   parm      *zero         ##rrn             9 0
     C                   parm      *zero         ##rrn2            9 0
     C                   parm                    ##qte             1
     C                   parm                    ##view            1 0
     C                   parm      emrrn         ##rrn3            9 0
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * Option 7 - Call Premium Finance Information
     *================================================================
     C     Opt7          begsr

     C     scdbrr        chain     pfrcdf
     C     fnky          chain     pfn01
     C                   eval      kkefdt = *hival
     C     pfky1         setgt     ppf01
     C     pfky          readpe    ppf01                                  60
     C                   if        not *in60
     C                   call      'W0045R02'
     C                   parm                    ##kc
     C                   parm                    ##kl
     C                   parm      'D'           ##mode
     C                   parm      pfrrn         ##rrn
     C                   parm      fnrrn         ##rrn2
     C                   endif

     C                   endsr
     *================================================================
     * Option 20 processing
     *================================================================
     C     Opt20         begsr

     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         call      'W0501R02'
     C**********         parm                    ##kc
     C**********         parm                    ##kl
     C**********         parm      'S'           ##mode
     C**********         parm      scdbrr        ##rrn
     C                   eval      ##mode = 'S'
     C                   call      ACDpgm        ACDpl
     C***  End   Chg  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * Option 60 processing
     *================================================================
     C     Opt60         begsr

     C     scdbrr        chain     pfrcdf
     C     elky          chain     pel01
     C                   if        %found
     C                   move(p)   RTCO#         #1co#
     C                   move(p)   RTFND         #1fnd
     C                   Call      'W0612R'
     C                   parm                    ##kc
     C                   parm                    ##kl
     C                   parm      '1'           ##call
     C                   parm                    #1co#             3
     C                   parm                    #1fnd             3
     C                   parm                    ##emp#            9
     C                   parm                    ##div             5
     C                   parm                    ##fyr             3
     C                   parm                    ##date            8
     C                   parm                    ##amount          9
     C                   parm                    ##chk#            7
     C                   parm                    ##rtcd            1
     C                   parm      elrrn         ##rrn
     C                   endif

     C                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     Pgmstr        begsr

     C***  Begin Add  ***  11/05/04  **************************************
     C*** beg chg ***** 07/18/07 ******************************************
     C**************     eval      agencyBill = @@mode = 'A'
     C**************     eval      *in98 = agencyBill

     C     @@rrn         chain     prn01
     C                   if        RNBTYP = 'A' or
     C                             RNBTYP = 'I' and RNPTYP = 'A'
     C                   eval      agencyBill = *on
     C                   eval      *in98 = *on
     C                   eval      @@mode = 'A'
     C                   else
     C                   eval      @@mode = 'E'
     C                   endif

     c                   eval      scbtyp = RNBTYP
     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Account Current'
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl
     C*** end chg ***** 07/18/07 ******************************************

     C                   select
      *** beg add ***** 07/18/07 ******************************************
     C                   when      scbtyp = 'I'

     c                   if        agencyBill
     c                   eval      hd = hda
     c                   endif
     C* Inactivate Opt 6
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '06'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
     C* Inactivate Opt 7
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '07'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
     C* Inactivate Opt 60
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '60'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
      *** end add ***** 07/18/07 ******************************************
     C*
     C                   when      agencyBill
     C* Update headings
     C                   eval      hd = hda
     C* Update Opt 6
     C                   move      *off          found
     C                   eval      i = 1
B01  C                   dow       found = *off and of(i) <> '3'
B02  C                   if        of(i) = '1'
     C                   movel     ofd(i)        dumnum
B03  C                   if        '06' = dumnum
     C                   movel(p)  agency6       dumofd
     C                   movel     dumofd        ofd(i)
     C                   move      *on           found
E03  C                   endif
E02  C                   endif
     C                   eval      i = i + 1
E01  C                   enddo
     C* Inactivate Opt 7
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '07'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
     C* Inactivate Opt 60
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '60'
     C                   eval      xxmis  = 'NNN'
     C                   exsr      updofa
     C                   when      @@mode = 'E'
     C                   other
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************

     C*** beg del ***** 07/18/07 ******************************************
     C*****@@rrn*******  chain     prn01
     C*** end del ***** 07/18/07 ******************************************
     C                   if        %found
     C*** beg add ***** 07/18/07 ******************************************
     c                   if        RNMIN  <> 0
     c                   eval      *in41 = *on
     c                   endif
     c                   if        RNTGRP <> 0
     c                   eval      *in42 = *on
     c                   endif
     c                   if        RNTAUD <> 0
     c                   eval      *in43 = *on
     c                   endif
     C*** end add ***** 07/18/07 ******************************************
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   IF        CLTID = '081'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 ******************************************
     C                   eval      scbat# = RNBAT#
     C                   eval      scfgrp = RNFGRP
     C                   eval      sctgrp = RNTGRP
     C                   move      RNMIN         h11              11 0
     C                   move      h11           scminp
     C                   eval      srdti = RNFAUD
     C                   call      'SRCVTDAT'    dtmdy
     c                   movel     srdto         scfaud
     C                   eval      srdti = RNTAUD
     C                   call      'SRCVTDAT'    dtmdy
     c                   movel     srdto         sctaud
     C                   Eval      kkco#  = RNCO#
     C                   Eval      kkbat# = RNBAT#
     C                   else
     C                   seton                                        lr
     C                   endif

     C                   endsr
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     Read          begsr

     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         If        *In21
     C*****Key1*         Reade(N)  Dbrcdf                                 95
     C**********         Endif
     c**********
     C**********         If        *In22
     C*****Key1*         Reade(N)  Dbrcdf2                                95
     C**********         Endif
     C                   select
     C                   when      *in21 and agencyBill
     C     key1          reade(n)  dbrcdfa                                95
     C                   eval      dbrrn = dbrrna
     C                   when      *in21
     C     key1          reade(n)  dbrcdf                                 95
     C                   when      *in22 and agencyBill
     C     key1          reade(n)  dbrcdf2a                               95
     C                   eval      dbrrn2 = dbrrn2a
     C                   when      *in22
     C     key1          reade(n)  dbrcdf2                                95
     C                   other
     C                   eval      *in95 = *on
     C                   endsl
     C***  End   Chg  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     Readp         begsr

     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         If        *In21
     C*****Key1*         Readpe(N) Dbrcdf                                 95
     C**********         Endif
     C**********
     C**********         If        *In22
     C*****Key1*         Readpe(N) Dbrcdf2                                95
     C**********         Endif
     C                   select
     C                   when      *in21 and agencyBill
     C     key1          readpe(n) dbrcdfa                                95
     C                   eval      dbrrn = dbrrna
     C                   when      *in21
     C     key1          readpe(n) dbrcdf                                 95
     C                   when      *in22 and agencyBill
     C     key1          readpe(n) dbrcdf2a                               95
     C                   eval      dbrrn2 = dbrrn2a
     C                   when      *in22
     C     key1          readpe(n) dbrcdf2                                95
     C                   other
     C                   eval      *in95 = *on
     C                   endsl
     C***  End   Chg  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     setgt         begsr

     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         If        *In21
     C*****v1ky*         Setgt     Dbrcdf                             96
     C**********         Endif
     C**********
     C**********         If        *In22
     C*****v2ky*         Setgt     Dbrcdf2                            96
     C**********         Endif
     C                   select
     C                   when      *in21 and agencyBill
     C     v1kya         setgt     dbrcdfa                            96
     C                   when      *in21
     C     v1ky          setgt     dbrcdf                             96
     C                   when      *in22 and agencyBill
     C     v2kya         setgt     dbrcdf2a                           96
     C                   when      *in22
     C     v2ky          setgt     dbrcdf2                            96
     C                   other
     C                   eval      *in96 = *on
     C                   endsl
     C***  End   Chg  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     Setll         begsr
      *
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         If        *In21
     C*****v1ky*         Setll     Dbrcdf                             96
     C**********         Endif
     C**********
     C**********         If        *In22
     C*****v2ky*         Setll     Dbrcdf2                            96
     C**********         Endif
     C                   select
     C                   when      *in21 and agencyBill
     C     v1kya         setll     dbrcdfa                            96
     C                   when      *in21
     C     v1ky          setll     dbrcdf                             96
     C                   when      *in22 and agencyBill
     C     v2kya         setll     dbrcdf2a                           96
     C                   when      *in22
     C     v2ky          setll     dbrcdf2                            96
     C                   other
     C                   eval      *in96 = *on
     C                   endsl
     C***  End   Chg  ***  11/05/04  **************************************
      *
     C                   endsr
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '      ***Deleted***'
     C                   endsr
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        begsr
     C                   endsr
     *================================================================
     * SUBSET - Determine if record should be put in subfile
     *================================================================
     C     subset        begsr
      *
     C                   if        ssselc <> *blank and
     C                             ssselc <> RTSELC
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssfnd <> *zero and
     C                             ssfnd <> RTFND
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssemp# <> *zero and
     C                             ssemp# <> RTEMP#
     C                   eval      meetss = *off
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        ssfein <> *zero and
     C                             ssfein <> RTFEIN
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   select
     C                   when      agencyBill and RTACFL <> 'Y'
     C                   eval      meetss = *off
     C                   when      not agencyBill and RTACFL = 'Y'
     C                   eval      meetss = *off
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   if        ssname <> *blank
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   eval      string1 = upper(RTAGTN)
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      string1 = upper(RTEMPN)
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      string2 = upper(ssname)
      *
     c                   call      'QCLSCAN'
     c                   parm                    string1
     c                   parm      40            strlen
     c                   parm      1             start
     c                   parm                    string2
     c                   parm      20            patlen
     c                   parm      '0'           trans
     c                   parm      '1'           trim
     c                   parm      ' '           wild
     c                   parm                    scnres

     c                   if        scnres = 0
     c                   eval      meetss = *off
     c                   endif
     C                   endif
     C***  BEG ADD  *****  12/26/07  **************************************
      *
     C                   IF        SSAGT1 <> *ZERO AND SSAGT2 <> *ZERO
     C                                           AND
     C                             (RTAGT# < SSAGT1 OR RTAGT# > SSAGT2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSAGT1
      *
     C                   IF        SSEMP1 <> *ZERO AND SSEMP2 <> *ZERO
     C                                           AND
     C                             (RTEMP# < SSEMP1 OR RTEMP# > SSEMP2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSEMP1
      *
     C                   IF        SSCUS1 <> *BLANKS AND SSCUS2 <> *BLANKS
     C
     C                   EVAL      @RTCUST = %XLATE(LO:UP:RTCUST)
     C                   IF        @RTCUST < @SSCUS1 OR @RTCUST > @SSCUS2
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  @RTCUST < SSCUS1
     C                   ENDIF                                                  SSCUS1
      *
     C                   IF        CLTID = '081'
      *
     C                   IF        RTSDFL = *BLANK
     C                   MOVE      'N'           @RTSDFL           1
     C                   ELSE
     C                   MOVE      RTSDFL        @RTSDFL           1
     C                   ENDIF                                                  RTSDFL=*BLANK
      *
     C                   IF        (SSSDFL <> *BLANK AND
     C                             SSSDFL <> @RTSDFL)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSSDFL <> *BLANK
      *
     C                   ENDIF                                                  CLTID = '081'
     C***  END ADD  *****  12/26/07  **************************************
     C                   endsr
** Column headings
Opt Ck Employer Name         Grp Employer#   Div  Yr    Amount      To Adj Aud
Opt Ck Grp Employer#   Div  Yr Employer Name            Amount      To Adj Aud
** Column headings (Agency Billing)
Opt Ck Agent Name                       Agt Fein/Mod    Amount
Opt Ck  Agt Fein/Mod Agent Name                         Amount
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Employer Info          YYN
1077=Finance Info           YYN
1                           NNN
12020=Change wrt ck status  YNN
16060=A\R Inquiry           YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYN
3
