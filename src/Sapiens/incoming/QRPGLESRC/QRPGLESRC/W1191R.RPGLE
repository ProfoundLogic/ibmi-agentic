     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    h/TITLE W1191R - W/W HCN Program History Information
    H*********************************************************************
     H* Created: 11/10/10   C:33   Project: 139WPL
    H*********************************************************************
     H* Revisions:
     H*
     H* 02/19/13 C:189 Support: WA099 79100
     H*                Exexute the processing only if the chain is successful
     H*
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw1191f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdhcp     if a e             disk    rename(phc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdhcl6    if   e           k disk    rename(phc01:dbrcdf)
     F                                     infds(dbfds)
     Fwdhcl1    if   e           k disk    rename(phc01:dbrcdf2)
     F                                     infds(dbfds2)
     Fwdhclr    if   e           k disk    rename(phc01:phc0r)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmhcl     if   e           k disk    rename(phc01:mhc00)
     Fwmshl     if   e           k disk    rename(psh01:psh00)
    D*
     D#ofHDs           c                   2
     D hd              s             78    dim(#ofHDs) ctdata perrcd(1)
     D#ofOFDs          c                   20
     D of              s              1    dim(#ofOFDs) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFDs) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'HCN Program Information'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W1191R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   10
     d editLowLine     c                   const(x'24')
     d displayLow      c                   const(x'A0')
     d displayHigh     c                   const(x'A2')
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
     D @@kc            s              1n
     D @@kl            s              1n
     D @@Call          s              1
     D @@rrn           s              9  0
     D @@Mode          s              1
     D kkCo#           s                   like(HCCO#)
     D kkFnd           s                   like(HCFND)
     D kkEmp#          s                   like(HCEMP#)
     D kkDiv           s                   like(HCDIV)
     D kkFyr           s                   like(HCFYR)
     D kkIn            s                   like(HCIN)
     D midTerm         s              1n
     D revised         s              1n
     D empMstrCall     s              1n   inz
     D x8Date          s              8  0
     D emRRN           s              9  0
     D fnRRN           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DPgmExistsInStk   pr                  extPgm('SRCHKPGM')
     D program                       10                 const
     D exists                         1n
    D*
     DPolicyIncept     pr             1n
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)
    D*
     D/copy wPrGroup
     D/copy wPrEmplr
    C*
     C/COPY SCOPYWW42C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@Call
     C                   parm                    @@rrn
     C                   parm                    @@Mode
    C* @@Mode is optional.  'E' when @@rrn = RRN of WMEMP.
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scDbRR        ##rrn
     C                   parm      @@rrn         ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     key           klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C                   kfld                    kkIn
     C*
     C     key2          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkIn
     C                   kfld                    kkFyr
     C*
     C     keyWDHCLR     klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C*
     C     key1WDHCLR    klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C                   kfld                    kkIn
     C*
     C     keyWMHCL      klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCST
     C*
     C     key1WMHCL     klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCST
     C                   kfld                    HCIN
    C*====================================================================
    C* ChkMidTerm - Check to see if running a mid-term endorsment
    C*====================================================================
     C     ChkMidTerm    begsr
     C*
     C                   callp     PgmExistsInStk('W0051R1':midTerm)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ChkRevised - Check to see if running a revised bill
    C*====================================================================
     C     ChkRevised    begsr
     C*
     C                   callp     PgmExistsInStk('W0090R':revised)
     C*
     C* If revised billing, copy the data to overriding files
     C                   if        revised
     C                   exsr      LoadOvrFile
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ChkSsm - Set indicator 11 for subset message
    C*====================================================================
     C     ChkSsm        begsr
     C*
     C                   eval      *in11 =
     C                              (ssFyr <> *zero and empMstrCall)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Errck1 - Additional errors on subfile selection
    C*====================================================================
     C     Errck1        begsr
     C                   endsr
     C*
    C*====================================================================
    C* Errck2 - Error checking for the subset screen
    C*====================================================================
     C     Errck2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* InzSst - Initialize the subset fields
    C*====================================================================
     C     InzSst        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KeyInz - Initialize key values for program start
    C*====================================================================
     C     KeyInz        begsr
     C*
     C                   eval      kkCo#  = EMCO#
     C                   eval      kkFnd  = EMFND
     C                   eval      kkEmp# = EMEMP#
     C                   eval      kkDiv  = EMDIV
     C*
     C                   eval      kkIn   = *hival
     C                   eval      kkFyr  = *hival
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySet - Set key values when doing "position to"
    C*====================================================================
     C     KeySet        begsr
     C*
     C                   select
     C                   when      *in21
     C                   move      scPst1        kkFyr
     C                   clear                   kkIn
     C                   when      *in22
     C                   move      scPst2        x8Date
     C                   eval      kkIn = DtCYMD(x8Date)
     C                   clear                   kkFyr
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySfl - Set key values from subfile records for paging
    C*====================================================================
     C     KeySfl        begsr
     C*
     C                   eval      kkFyr = scFyr
     C                   eval      kkIn  = scIn
     C*
     C                   endsr
    C*
    C*====================================================================
    C* LoadOvrFile - Load the overridden file
    C*====================================================================
     C     LoadOvrFile   begsr
     C*
     C                   eval      kkCo#  = ELCO#
     C                   eval      kkFnd  = ELFND
     C                   eval      kkEmp# = ELEMP#
     C                   eval      kkDiv  = ELDIV
     C                   eval      kkFyr  = ELFYR
     C                   eval      kkIn   = *loval
     C*
     C* If records exist, do not write more
     C     keyWDHCLR     chain     dbrcdf
     C                   if        %found
     C                   leavesr
     C                   endif
     C*
     C     key1WDHCLR    setll     phc0r
     C     keyWDHCLR     reade     phc0r
     C                   dow       not %eof
     C                   eval      HCUSER = q1User
     C                   eval      HCPGM  = q1Pgm
     C                   eval      HCDT   = DateTo7(SystemDate)
     C                   eval      HCTIME = TimeTo6(SystemTime)
     C                   write     pfrcdf
     C                   feod      WDHCP
     C     keyWDHCLR     reade     phc0r
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* LoadScrHead - Load screen header information
    C*====================================================================
     C     LoadScrHead   begsr
    C*
     C                   eval      scFnd = EMFND
     C                   eval      scFndNm = GetGrpName(EMFND:fnRRN:EMCO#)
     C                   eval      scEDN = EmpDivName(EMEMP#:EMDIV:emRRN
     C                                               :EMFND:EMCO#)
     C                   eval      *in63 = GetPrvEmp#(EMCO#:EMFND:EMEMP#
     C                                               :EMDIV
     C                                               :scOFnd:scOEmp:scODiv)
     C                                     = *blanks
     C                   eval      sc#Div = DivCount(EMCO#:EMFND:EMEMP#)
     C                   eval      scCxDt = DtMDYY(GetCxDate(EMCO#:EMFND
     C                                                      :EMEMP#:EMDIV)
     C                                            )
     C                   eval      *in41 = scCxDt <> *zero
    C*
     C                   endsr
     C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C* Get group level information
     C                   clear                   HCFACT
     C     key1WMHCL     setgt     mhc00
     C     keyWMHCL      readpe    mhc00
     C*
     C* Hidden screen fields
     C                   eval      scFyr = HCFYR
     C                   eval      scSt  = HCST
     C                   eval      scIn  = HCIN
     C                   eval      scOut = HCOUT
     C*
     C* Data field
     C                   select
     C                   when      *in21
     C                   eval      scData      =
     C                               ' '       + %editc(HCFYR:'X')
     C                             + '  '      + AlphaUSA(CvtToDate(HCIN))
     C                             + '  '      + AlphaUSA(CvtToDate(HCOUT))
     C                             + '   '     + HCST
     C                   when      *in22
     C                   eval      scData      = AlphaUSA(CvtToDate(HCIN))
     C                             + '  '      + AlphaUSA(CvtToDate(HCOUT))
     C                             + '   '     + %editc(HCFYR:'X')
     C                             + '   '     + HCST
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OthFks - Process other function keys
    C*====================================================================
     C     OthFks        begsr
     C                   endsr
     C*
    C*====================================================================
    C* OthOpt - Process other options
    C*====================================================================
     C     OthOpt        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C                   eval      *inlr = *on
     C                   endsr
     C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   select
     C                   when      %parms >= 5
     C                              and @@Mode = 'E'
     C                   eval      emRRN = @@rrn
     C                   eval      @@Call = '1'
     C                   eval      empMstrCall = *on
     C                   eval      at1 = editLowLine
     C                   clear                   ssFyr
     C                   other
     C     @@rrn         chain     WDELP
     C                   eval      emRRN = GetEmpRRN(ELEMP#:ELDIV:ELFND:ELCO#)
     C                   eval      ssFyr = ELFYR
     C                   eval      at1 = displayHigh
     C                   endsl
     C     emRRN         chain     WMEMP
     C*
     C*** begin chg ***  02/19/13  ***************************************************
     C                   If        %found(WMEMP)
     C*** end   chg ***  02/19/13  ***************************************************
     C                   exsr      LoadScrHead
     C*
     C                   exsr      ChkRevised
     C                   exsr      ChkMidTerm
     C*
     C                   exsr      SetOptFnc
     C*
     C                   exsr      KeyInz
     C*** begin chg***   02/19/13  **************************************************
     C                   endif
     C*** end  chg ***   02/19/13  **************************************************
     C                   endsr
     C*
    C*====================================================================
    C* Read - Read next record from file
    C*====================================================================
     C     Read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   when      *in22
     C     key           reade     dbrcdf2                                95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ReadP - Read previous record from file
    C*====================================================================
     C     ReadP         begsr
     C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   when      *in22
     C     key           readpe    dbrcdf2                                95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetGT - Set greater than on file
    C*====================================================================
     C     SetGT         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   when      *in22
     C     key2          setgt     dbrcdf2                            96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetLL - Set lower limits on file
    C*====================================================================
     C     SetLL         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   when      *in22
     C     key2          setll     dbrcdf2                            96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetOptFnc - Set option function key changes
    C*====================================================================
     C     SetOptFnc     begsr
     C*
     C                   if        not PolicyIncept(ELCO#:ELFND:ELEMP#:ELDIV
     C                                             :ELFYR)
     C                              and not midTerm
     C*
     C                   eval      xxOpFn = '1'
     C                   eval      xxNmBr = '02'
     C                   eval      xxMIS  = 'NNN'
     C                   exsr      UpdOFA
     C*
     C                   eval      xxOpFn = '2'
     C                   eval      xxNmBr = '06'
     C                   eval      xxMIS  = 'NNN'
     C                   exsr      UpdOFA
     C*
     C                   endif
     C*
     C                   if        PolicyIncept(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR)
     C*
     C                   eval      xxOpFn = '1'
     C                   eval      xxNmBr = '04'
     C                   eval      xxMIS  = 'YNN'
     C                   exsr      UpdOFA
     C*
     C                   endif
     C*
     C                   if        empMstrCall
     C*
     C                   eval      xxOpFn = '2'
     C                   eval      xxNmBr = '17'
     C                   eval      xxMIS  = 'YYY'
     C                   exsr      UpdOFA
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Sf1Upd - Update subfile record when item is deleted
    C*====================================================================
     C     Sf1Upd        begsr
     C                   eval      scData = '* * * Deleted'
     C                   endsr
     C*
    C*====================================================================
    C* SlctRd - Move fields for selected record to parameters
    C*====================================================================
     C     SlctRd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* Subset - Determine if record should be put in subfile
    C*====================================================================
     C     Subset        begsr
     C*
     C                   select
     C                   when      ssFyr <> *zero
     C                              and ssFyr <> HCFYR
     C                   eval      meetSs = *off
     C                   endsl
     C*
     C                   endsr





    P*====================================================================
    P* Determine if the program is added at policy inception
    P*====================================================================
     PPolicyIncept     b
     DPolicyIncept     pi             1n
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)

     C     keyWMSHL      klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inFyr

     C                   clear                   SHSTAT
     C*
     C     keyWMSHL      setgt     psh00
     C     keyWMSHL      readpe    psh00
     C*
     C                   select
     C                   when      SHSTAT = '3' or SHSTAT = '4'
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl

     PPolicyIncept     e





** Column headings
Opt    Yr    Opt In     Opt Out     St
Opt    Opt In      Opt Out     Yr   St
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
