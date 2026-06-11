     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     Hoption(*nodebugio:*srcstmt)
      *
     H/title W0070RNC - NC NOI Information Maintenance
     H*********************************************************************
     H*  Description:                                                     *
     H*                                                                   *
     H*  This program is used to enter fields which are used on the       *
     H*  NC Notice of Injury FORM 19.                                     *
     H*                                                                   *
     H*********************************************************************
     H* Revisions:
     H*                                                                   *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 07/11/11 C:062 SEQ#77360                                          *
     H*                Assign the correct parm in call of W0190R          *
     H*                                                                   *
     H* 10/22/09 C:189 SEQ#70040                                          *
     H*                Additional screens being displayed which           *
     H*                has not been set up in Group set up.               *
     H*                                                                   *
     H* 08/28/09 C:189 Project: 133WHE                                    *
     H*                Add the call to Accident Info program              *
     H*                W0070R08 and not call W0190R and SC010R.           *
     H*                                                                   *
     H* 12/19/08  C:61  PROJECT 81WCN
     H*                 Created.
     H*
     H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0070fnc  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwmnip     uf a e             disk    rename(pni01:dbrcdf)
     Fwmnil     if   e           k disk    rename(pni01:pni00) infds(niFDS)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwmcml1    if   e           k disk                        infds(cmFDS)
     Fwmcdl     if   e           k disk    rename(pcd01:pcd00)
     Fwmcxl     if   e           k disk    rename(pcx01:pcx00)
      *
      *
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Claim NC NOI Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   3
      *
      *
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##Co#           s              3  0
     D ##CoNm          s             40
     D ##FNam          s             40
     D ##Fnd           s              3  0
     D ##Pgm           s             10
     D ##wwmo          s              1
     D ##Prds          s             40
     D ##Call          s              1
     D ##Fyr           s              3
     D ##Case          s              7
     D ##AcDt          s              8
     D ##RtCd          s              1
     D ##Key           s             40
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@Co#           s              3
     D @@Fnd           s              3
     D @@Fyr           s              3
     D @@Case          s              7
     C*** BEG ADD ***** 08/28/09 **************************************
     D @@rtcd          s              1
     C*** END ADD ***** 08/28/09 **************************************
     D xxCo#           s                   like(CMCO#)
     D xxFnd           s                   like(CMFND)
     D xxFyr           s                   like(CMFYR)
     D xxCase          s                   like(CMCASE)
     D xxEmp#          s                   like(CMEMP#)
     D xxDiv           s                   like(CMDIV)
     D xxDt            s                   like(CMDT)
     D xxSS#           s                   like(CDSS#)
     D xxSt            s                   like(NIST)
     D xxmis           s              3
     D xxnmbr          s              2
     D xxopfn          s              1
     D xxPgm           s                   like(q1pgm)
     D xxErr           s              1
     D years           s              2  0
     D months          s              2  0
     D days            s              2  0
     D rrn             s              9  0
     D dumnum          s              2
     D dumofd          s             30
     D found           s              1n
     D i               s              2  0
     D of#             s              3  0
     D ol              s             80    dim(14)
     D fl              s             80    dim(14)
     D vo              s              1    dim(99)
     D vf              s              1    dim(24)
     D#ofOfd           c                   3
     D of              s              1    dim(#ofOfd) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOfd) alt(of)
     D ##of            s            100
     D ##ofd           ds          3500
     D cmFDS           ds
     D  cmRRN                397    400B 0
     D niFDS           ds
     D  niRRN                397    400B 0
     D ##dtMDY         ds             8
     D  ##year                 1      2  0
     D  ##mnth                 4      5  0
     D  ##days                 7      8  0
     D usaHiDt         s               d   datfmt(*usa) inz
     D usaCmDt         s               d   datfmt(*usa) inz
     D usaSvDt         s               d   datfmt(*usa) inz
     D usaAcDt         s               d   datfmt(*usa) inz
     D x1hidt          ds             7
     D  xxyy                   1      3  0
     D  xxmm                   4      5  0
     D  xxdd                   6      7  0
     D fids01        e ds                  extname(szq2p)
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrEmplr
     *
     C/COPY SCOPYMT41C
     *
     *
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Fyr
     C                   parm                    @@Case
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   parm                    @@rtcd
     C*** END ADD ***** 08/28/09 **************************************
      ***** BEG ADD ******* 10/22/09 **********************************
ks01 C                   parm                    @@cont
      ***** END ADD ******* 10/22/09 **********************************
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     ACDPL3        plist
     C                   parm      scPrDs        #1PrDs           40
     C                   parm      @@call        #1Call            1
     C                   parm                    #1Co#             3
     C                   parm                    #1Fnd             3
     C                   parm                    #1Fyr             3
     C                   parm                    #1Case            7
     C                   parm                    #1AcDt            8
     C                   parm                    #1RtCd            1

     C*** BEG ADD ***** 08/28/09 **************************************
     C     acdpl4        plist
     C     *inkc         parm                    @@KC              1
     C     *inkl         parm                    @@KL              1
     C                   parm                    @@MODE            1
     C                   parm                    @@RRN             9 0
      ***** BEG CHG ******* 10/22/09 **********************************
     C*************      parm      'Y'           @@CONT            1
     C                   parm                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************

      *
      *
     *================================================================
     *  KLIST definitions
     *================================================================
     C     keyWMCDL      klist
     C                   kfld                    xxSS#
      *
     C     keyWDCXL      klist
     C                   kfld                    xxCo#
     C                   KFLD                    xxFnd
      *
     C     keyWMFNL      klist
     C                   kfld                    xxCo#
     C                   KFLD                    xxFnd
      *
     C     keyWMCML1     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxCase
      *
     C     keyWMNIL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxCase
     C                   kfld                    xxSt                           ADD 05/18/00
      *
      *
     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr
      *
      *
     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr
      *
      *
     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr
      *
     C                   endsr
      *
      *
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr
      *
      *
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   If        CLTID = '133'
     C                             and @@call <> *blanks
     C                             and not *inkc and not *inkl
      *
     C     keyWMCML1     CHAIN     PCM01                              99
     C                   Z-ADD     cmRRN         @@RRN
      *
     C                   Select
     C                   when      @@call = 'A'
     C                   movel     'A'           @@MODE
     C                   when      @@call = 'Y'
     C                   movel     'C'           @@MODE
     C                   when      @@call = 'Z'
     C                   movel     'D'           @@MODE
     C                   endsl
      *
     C                   eval      @@rtcd = *blanks
     C                   CALL      'W0070R08'    ACDPL4                   99
      *
     C   kl              eval      @@rtcd = 'B'
     C   kc              eval      @@rtcd = 'A'
      *
     C                   If        *inkl
     C                   eval      *in90 = *on
     C                   eval      *inkl = *off
     C                   endif
      *
     C                   endif                                                  CLTID = 133
      *
     C                   If        cltid = '133'
     C   kl              eval      @@rtcd = 'B'
     C   kc              eval      @@rtcd = 'A'
     C                   endif
      *
     C*** END ADD ***** 08/28/09 **************************************
     C                   endsr
      *
      *
     *================================================================
     * GetClmInfo - Get the claim information
     *================================================================
     C     GetClmInfo    begsr
      *
     C                   clear                   CMEMP#
     C                   clear                   CMDIV
     C                   clear                   CMSS#
     C                   clear                   CMJRST
     C                   clear                   CMACDT
     C                   clear                   CMHIDT
     C                   clear                   CMACL#
     C                   clear                   CMCVT#
      *
     C     keyWMCML1     chain     pcm01
      *
     C                   if        CMSS# <> *blanks
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C                   eval      SCSS# = %subst(CMSS#:1:3)
     C                                   + '-'
     C                                   + %subst(CMSS#:4:2)
     C                                   + '-'
     C                                   + %subst(CMSS#:6:4)
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C                   endif                                                  CMSS# <> BLANK
      *
      *  Claimant name
      *
     C                   clear                   CDLSN
     C                   clear                   CDFSN
     C                   clear                   CDMID
     C                   eval      xxSS# = CMSS#
     C     keyWMCDL      chain     pcd00
     C                   eval      scCNam = %trim(CDLSN)
     C                                    + ', '
     C                                    + %trim(CDFSN)
     C                                    + ' '
     C                                    + %trim(CDMID)
      *
     C                   clear                   scAcDt
     C                   if        CMACDT <> *zero
     C                   eval      scAcDt = DtMDYY(CMACDT)
     C                   endif
      *
     C                   eval      scSCL# = CMACL#
      *
      * If claim was converted from another system, display old claim #
      *
     C                   if        CMCVT# <> *blank
     C                   eval      scCvt = CMCVT#
     C                   eval      *in64 = *on
     C                   endif
      *
     C                   eval      scYrCs = @@Fyr
     C                                    + '-'
     C                                    + @@Case
      *
     C                   endsr
      *
      *
     *================================================================
     * GetEmpInfo - Get the employer information
     *================================================================
     C     GetEmpInfo    begsr
      *
      *  Get the Emp#, Div, & name
     C                   eval      scEDN = EmpDivName(CMEMP#
     C                                               :CMDIV
     C                                               :rrn
     C                                               :CMFND
     C                                               :CMCO#
     C                                               )
      *
      * Get employer cancel date
     C                   eval      scCxDt = GetCxDate(CMCO#
     C                                               :CMFND
     C                                               :CMEMP#
     C                                               :CMDIV
     C                                               )
     C                   eval      *in41 = scCxDt <> *zero
     C
     C                   endsr
      *
      *
     *================================================================
     * MaintFlds - Update maintance fields
     *================================================================
     C     MaintFlds     begsr
      *
     C                   eval      NIUSER = q1User
     C                   eval      NIPGM  = q1Pgm
     C                   eval      NITIME = TimeTo6(SystemTime)
     C                   eval      NIDT   = DateTo7(SystemDate)
      *
     C                   endsr
      *
      *
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
      *
      * scr01
     C                   eval      sc60A1 = ni60a1
     C                   eval      sc60A2 = ni60a2
     C                   eval      sc40a1 = ni40a1
     C                   movel(p)  ni30a1        sc30a1
     C                   eval      sc30a2 = ni30a2
     C                   eval      sc20a1 = ni20a1
     C                   eval      scdbdt = dtmdyy(cmdbdt)

     c                   if        ni1a3 = 'A'
     C                   eval      scdam  = 'X'
     c                   endif

     c                   if        ni1a3 = 'P'
     C                   eval      scdpm  = 'X'
     c                   endif

      * scr02

     c                   if        @@mode = 'A'
     c                   z-add     0             xx20a3y           2 0
     c                   z-add     0             xx20a3m           2 0
     c                   z-add     0             xx20a3d           2 0

     c                   if        (cmhidt <> 0) and (cmacdt <> 0)
     C                   eval      usaHiDt = CvtToDate(CMHIDT)
     C                   eval      usaAcDt = CvtToDate(CMACDT)

      * calc years employed

     c                   eval      usaCmDt = usaHidt

     c                   dow       usaCmdt <= usaAcDt
     c                   eval      usaSvDt = usaCmDt
     c                   adddur    1:*Y          usaCmDt

     c                   if        usaCmDt <= usaAcDt
     c                   eval      xx20a3y = xx20a3y + 1
     c                   endif

     c                   enddo

     c                   eval      usaCmDt = usaSvDt

      * calc months employed

     c                   dow       usaCmDt <= usaAcDt
     c                   eval      usaSvDt = usaCmDt
     c                   adddur    1:*M          usaCmDt

     c                   if        usaCmdt <= usaAcDt
     c                   eval      xx20a3m = xx20a3m + 1
     c                   endif

     c                   enddo

     c                   eval      usaCmDt = usaSvDt

      * calc days employed

     c                   dow       usaCmDt <= usaAcDt
     c                   eval      usaSvDt = usaCmDt
     c                   adddur    1:*d          usaCmDt

     c                   if        usaCmdt <= usaAcDt
     c                   eval      xx20a3d = xx20a3d + 1
     c                   endif

     c                   enddo
      * zzz
     c                   move      xx20a3y       sc20a3y
     c                   move      xx20a3m       sc20a3m
     c                   move      xx20a3d       sc20a3d
     c                   endif

     c                   else

     c                   if        ni20a3 <> *blanks
     c                   eval      sc20a3y = %subst(ni20a3 : 1 : 2)
     c                   eval      sc20a3m = %subst(ni20a3 : 3 : 2)
     c                   eval      sc20a3d = %subst(ni20a3 : 5 : 2)
     c                   endif
     c                   endif

     C                   movel(p)  ni10a1        sc10A1
     C                   movel(p)  ni10a2        sc10A2
     C                   movel(p)  ni10a3        sc10A3
     C                   movel(p)  ni10a4        sc10A4

      * scr03
     C                   movel(p)  ni60a3        sc60A3
     C                   movel(p)  ni60a4        sc60A4
     C                   move      ni10a5        sc10A5
     C                   move      ni1A6         sc1A6
     C                   move      ni1A7         sc1A7
     C                   movel(p)  ni20a2        sc20A2
     C                   movel(p)  ni20a4        sc20A4
     C                   movel(p)  ni60a5        sc60A5
     C                   move      ni1A8         sc1A8
      *
     C                   endsr
      *
      *
     *================================================================
     * MVSCDB - Move screen fields
     *================================================================
     C     mvscdb        begsr
      *
     C                   z-add     xxco#         nico#
     C                   z-add     xxfnd         nifnd
     C                   z-add     xxfyr         nifyr
     C                   z-add     xxcase        nicase
     C                   MOVE      'NC'          nist
      *
      * scr01
     C                   eval      NI60A1 = sc60a1
     C                   eval      NI60A2 = sc60a2
     C                   eval      NI40a1 = sc40a1
     C                   movel(p)  sc30a1        ni30a1
     C                   eval      NI30a2 = sc30a2
     C                   eval      NI20a1 = sc20a1

     c                   if        scdam <> *blank
     C                   eval      NI1A3  = 'A'
     c                   endif

     c                   if        scdpm <> *blank
     C                   eval      NI1A3  = 'P'
     c                   endif

      * scr02
     c                   if        (sc20a3y <> *blanks)
     c                             or (sc20a3m <> *blanks)
     c                             or (sc20a3d <> *blanks)
     c                   eval      ni20a3 = sc20a3y + sc20a3m + sc20a3d
     c                   endif

     C                   movel(p)  sc10a1        NI10A1
     C                   movel(p)  sc10a2        NI10A2
     C                   movel(p)  sc10a3        NI10A3
     C                   movel(p)  sc10a4        NI10A4

      * scr03
     C                   movel(p)  sc60a3        NI60A3
     C                   movel(p)  sc60a4        NI60A4
     C                   move      sc10a5        NI10A5
     C                   move      sc1A6         NI1A6
     C                   move      sc1A7         NI1A7
     C                   movel(p)  sc20a2        NI20A2
     C                   movel(p)  sc20a4        NI20A4
     C                   movel(p)  sc60a5        NI60A5
     C                   move      sc1A8         NI1A8
      *
     C                   exsr      MaintFlds
      *
     C                   endsr
      *
      *
     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr
      *
      *
     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr
      *
      *
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
      *
      *  If @@Call = A, Go on to the next program in -ADD- mode.
      *  If @@Call = Y, Go on to the next program in -UPDATE- mode.
      *  If @@Call = Z, Go on to the next program in -DISPLAY- mode.
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   If        CLTID <> '133'
     C*** END ADD ***** 08/28/09 **************************************
     C                   select
      *__________
      *
     C                   when      @@Call = 'Y'
     C                             or @@Call = 'Z'
     C                   eval      #1RtCd = '1'
     C                   move      xxco#         #1Co#
     C                   move      xxfnd         #1Fnd
     C                   move      xxfyr         #1Fyr
     C                   move      xxcase        #1Case
     C                   move      scacdt        #1AcDt
     C                   call      'W0190R'      ACDPL3                 99
      *__________
      *
     C                   when      @@Call = 'A'
     C                   clear                   CXDBEN
     C                   clear                   CXDART
     C     keyWDCXL      chain     pcx00
     C                   select
     C                   when      CXDBEN = 'Y'
     C                   move      xxco#         #1Co#
     C                   move      xxfnd         #1Fnd
     C                   move      xxfyr         #1Fyr
     C                   move      xxcase        #1Case
     C                   move      scacdt        #1AcDt
     C*** Begin Chg *** 07/11/11 ******************************************
     C**********         eval      ##RtCd = '1'
     C                   eval      #1RtCd = '1'
     C*** End   Chg *** 07/11/11 ******************************************
     C                   call      'W0190R'      ACDPL3                 99
     C                   when      CXDART = 'Y'
     C                   eval      ##Key = @@Co# + @@Fnd + @@Fyr + @@Case
     C                   call      'SC010R'                             99
     C                   parm                    ##Key
     C                   parm      'W0070R'      ##Pgm
     C                   endsl
      *
     C                   endsl                                                   *IN60
      *
     C                   eval      *inkl = ##RtCd = 'B'
     C                   eval      *inkc = ##RtCd = 'A'
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   endif                                                  CLTID <> 133
     C                   If        cltid = '133'
     C   kl              eval      @@rtcd = 'B'
     C   kc              eval      @@rtcd = 'A'
     C                   endif
     C*** END ADD ***** 08/28/09 **************************************
      *
     c                   seton                                        LR
      *
     C                   endsr
      *
      *
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
      *
     C                   move      @@Co#         xxCo#
     C                   move      @@Fnd         xxFnd
     C                   move      @@Fyr         xxFyr
     C                   move      @@Case        xxCase
      *
     C                   eval      xxDt = DateTo7(SystemDate)
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
      *
     C                   select
      *
     C                   when      @@call = 'A'
     C                   eval      @@mode = 'A'
      *
     C                   when      @@call = ' '
     C                             or @@call = 'C'
     C                             or @@call = 'Y'
     C                   eval      @@mode = 'C'
      *
     C                   when      @@call = '1'
     C                             or @@call = '2'
     C                             or @@call = 'Z'
     C                   eval      @@mode = 'D'
      *
     C                   endsl
      *
      * Check for record in WMNIP, if not found, change to -ADD- mode
      *  load the RRN of WMNIP to @@rrn
      *
     C                   clear                   niRRN
     C                   eval      xxSt = 'NC'
     C     keyWMNIL      chain     pni00
     C                   eval      @@rrn = niRRN
     C                   if        not %found and @@mode <> 'D'
     C                   eval      @@mode = 'A'
     C                   endif
      *
     C                   eval      ##Co# = xxCo#
     C                   eval      ##Fnd = xxFnd
     C                   exsr      GetCoF
      *
      * If group is expired, then display in Inquiry Mode
      *
     C                   clear                   FNNAME
     C                   eval      scFnd = xxFnd
     C     keyWMFNL      chain     pfn00
     C                   if        %found
     C                   if        FNEXDT > *zero and FNEXDT < xxDt
     C                   eval      @@mode = 'D'
     C                   endif
     C                   endif
     C                   eval      scFNam = FNNAME
      *
     C                   exsr      GetClmInfo
     C                   exsr      GetEmpInfo
      *
     C                   endsr
      *
      *
     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr
      *
      *
     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
      *
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
      *
     C                   endsr
      *
     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
      *
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      NIIUSR = NIUSER
     C                   eval      NIITM  = NITIME
     C                   eval      NIIDT  = NIDT
     C                   write     dbrcdf
     C                   feod      wmnip
      *
     c                   eval      outflg = *on
      *
     C                   endsr
      *
** Description--------------MISN
203F3=Exit                  YYY
212F12=Cancel               YYY
3
