     H*copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H*StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM BUILDS THE WORKFILE USED FOR THE REPORT PROGRAM. *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W8006C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:189                                             *
     H* CREATED: 01/25/10               PROJECT: WC1851               *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/16/11 C:65  WC1851
     H*                Chain to group fee detail setup in order to populate
     H*                values when the override flags are not set.
     H*
     H* 05/09/11 C:65  WC1851
     H*                Ensure chains do not depend on claim # if they
     H*                do not need to since there are fee detail records
     H*                with a zero case #. (changes from CM* to FC* fields
     H*                                     not commented.)
     H*
     H* 02/15/11 C:061 WC1851
     H*          C:33  Re-write (no comments)
     H*****************************************************************
     FWXFDP     o    e             disk    rename(pfd01:xfd0p)
     FWDEFL1    if   e           k disk
     FWMCML     if   e           k disk    rename(pcm01:pcm00)
     FWMTHL     if   e           k disk    rename(pth01:pth00)
     FWMFNL     if   e           k disk    rename(pfn01:pfn00)
     FWMEML     if   e           k disk    rename(pem01:pem00)
     FWDELL     if   e           k disk    rename(pel01:pel00)
     FWMEPL     if   e           k disk    rename(pep01:pep00)
     FWDEHL3    if   e           k disk    rename(peh01:peh03)
     FWMCDL     if   e           k disk    rename(pcd01:pcd00)
     FWXGSL     if   e           k disk    rename(pgs01:pgs00)
     FWDFCL5    if   e           k disk    rename(pfc01:pfc05)
     FWMFHL     if   e           k disk    rename(pfd01:pfh00)
     FWTRTL1    if   e           k disk
     FWMUPL1    if   e           k disk
     FWMAHP     if   e             disk    rename(pah01:pah0p)
     FWTFIL     IF   E           K DISK
     FWMRSL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D@@CO#            s              3
     D@@FND            s              3
     D@@AsOf           s              8
     D@@PoDt           s              8
     D@@Emp#           s              9
     D@@Div            s              5
     D@@SSF            s              1
     DxxCo#            s                   like(CMCO#)
     DxxFnd            s                   like(CMFND)
     DxxEmp#           s                   like(CMEMP#)
     DxxDiv            s                   like(CMDIV)
     DxxFyr            s                   like(CMFYR)
     DxxCase           s                   like(CMCASE)
     DxxAgNm           s                   like(FDAGNM)
     DxxPrNm           s                   like(FDPRNM)
     DxxRtCd           s                   like(ELRTCD)
     DxxPrDt           s               d
     DxxAsOf           s               d
     DxxPoDt           s               d
     Dx7AsOf           s              7  0
     Dx7PoDt           s              7  0
     Dx8AsOf           s              8  0
     Dx8PoDt           s              8  0
     Dx7Date           s              7  0
     Drrn              s              9  0
     DahRRN            s                   like(rrn)
     DsingleEmp        s              1n
     DsysDt            s               d
     DsysDt7           s              7  0
     DsysDt8           s              8  0

     DinWMFHL          ds                  likerec(pfh00:*input)
     DkeyWMFHL         ds                  likerec(pfh00:*key) inz
     Dkey1WMFHL        ds                  qualified           inz
     D FDCO#                          3  0
     D FDFND                          3  0
     D FDEMP#                         9  0
     D FDDIV                          5  0
     D FDFYR                          3  0
     D FDCASE                         7  0

     DkeyWMTHL         ds                  likerec(pth00:*key) inz
     Dkey1WMTHL        ds                  qualified inz
     D THCO#                          3  0
     D THFND                          3  0
     D THFYR                          3  0
     D THCASE                         7  0
     DinWMTHL          ds                  likerec(pth00:*input)

     DkeyWDEHL3        ds                  qualified inz
     D EHCO#                          3  0
     D EHFND                          3  0
     D EHEMP#                         9  0
     D EHDIV                          5  0
     D EHFYR                          3  0
     D EHEFDT                         7  0
     Dkey1WDEHL3       ds                  qualified inz
     D EHCO#                          3  0
     D EHFND                          3  0
     D EHEMP#                         9  0
     D EHDIV                          5  0
     D EHFYR                          3  0
     DinWDEHL3         ds                  likerec(peh03:*input)

     DkeyWMEPL         ds                  qualified inz
     D EPCO#                          3  0
     D EPFND                          3  0
     D EPEMP#                         9  0
     D EPDIV                          5  0
     D EPFYR                          3  0
     DinWMEPL          ds                  likerec(pep00:*input)

     D#ofReserves      c                   10
     DreserveTypes     ds                  inz
     D reserveType                    2    dim(#ofReserves)
     D                                      overlay(reserveTypes:1)
     D  CMRT01                             overlay(reserveTypes: 1)
     D  CMRT02                             overlay(reserveTypes: 3)
     D  CMRT03                             overlay(reserveTypes: 5)
     D  CMRT04                             overlay(reserveTypes: 7)
     D  CMRT05                             overlay(reserveTypes: 9)
     D  CMRT06                             overlay(reserveTypes:11)
     D  CMRT07                             overlay(reserveTypes:13)
     D  CMRT08                             overlay(reserveTypes:15)
     D  CMRT09                             overlay(reserveTypes:17)
     D  CMRT10                             overlay(reserveTypes:19)
     DreserveRmns      ds                  inz
     D reserveRmn                     9  2 dim(#ofReserves)
     D                                      overlay(reserveRmns:1)
     D  CMRR01                             overlay(reserveRmns: 1)
     D  CMRR02                             overlay(reserveRmns:10)
     D  CMRR03                             overlay(reserveRmns:19)
     D  CMRR04                             overlay(reserveRmns:28)
     D  CMRR05                             overlay(reserveRmns:37)
     D  CMRR06                             overlay(reserveRmns:46)
     D  CMRR07                             overlay(reserveRmns:55)
     D  CMRR08                             overlay(reserveRmns:64)
     D  CMRR09                             overlay(reserveRmns:73)
     D  CMRR10                             overlay(reserveRmns:82)
     DreservePds       ds                  inz
     D reservePd                      9  2 dim(#ofReserves)
     D                                      overlay(reservePds:1)
     D  CMAR01                             overlay(reservePds: 1)
     D  CMAR02                             overlay(reservePds:10)
     D  CMAR03                             overlay(reservePds:19)
     D  CMAR04                             overlay(reservePds:28)
     D  CMAR05                             overlay(reservePds:37)
     D  CMAR06                             overlay(reservePds:46)
     D  CMAR07                             overlay(reservePds:55)
     D  CMAR08                             overlay(reservePds:64)
     D  CMAR09                             overlay(reservePds:73)
     D  CMAR10                             overlay(reservePds:82)

     DreserveInfo      ds                  qualified
     D type                                like(RTTYPE)
     D incurred                            like(FDTINC)
     D paid                                like(FDTPD)

     DcompInfo         ds                  likeDS(reserveInfo)
     DmedInfo          ds                  likeDS(reserveInfo)
     DexpInfo          ds                  likeDS(reserveInfo)
     DtotInfo          ds                  likeDS(reserveInfo)

     DGetClmntName     pr                  like(FDCNAM)

     DGetEmpProd       pr                  like(FDPROD)
     D inRRN                               like(rrn)
     D outName                             like(FDPRNM)

     DGetPayroll       pr                  like(FDCUPR)
     D asOfDt                          d

     DGetPremium       pr                  like(FDCUPR)
     D asOfDt                          d

     DGetIMR           pr                  like(FDCUTY)
     D asOfDt                          d

     DGetPriorRcd      pr
     D asOfDt                          d

     DGetStaffName     pr                  like(UPUSNM)
     D inStaffCode                         like(UPSTAF)

     DLoadRsvInfo      pr

     D/copy sPrDate
     D/copy wPrAgent
     D/copy wPrCarrier
     D/copy wPrProduce

      *====================================================================
      * KLIST
      *====================================================================
     C     fndWDEFL1     klist
     C                   kfld                    GSCO#
     C                   kfld                    GSFND
     C*
     C     empWDEFL1     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C*
     C     keyWDFCL5     klist
     C                   kfld                    EFCO#
     C                   kfld                    EFFND
     C                   kfld                    EFEMP#
     C                   kfld                    EFDIV
     C                   kfld                    EFFYR
     C                   kfld                    x7AsOf
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C*
     C     keyWMCML      klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C                   kfld                    FCDIV
     C                   kfld                    FCFYR
     C                   kfld                    FCCASE
     C*
     C     keyWMCDL      klist
     C                   kfld                    CMSS#
     C*
     C     keyWMFNL      klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C*
     C     keyWMEML      klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C                   kfld                    FCDIV
     C*
     C     keyWDELL      klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C                   kfld                    FCDIV
     C                   kfld                    FCFYR
     C*
     C     RSKY1         klist
     C                   kfld                    ELCO#
     C                   kfld                    xxRtCd
     C*
     C     CIKY          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     c                   kfld                    EFTYPE
     c                   kfld                    EFDESC
     C                   kfld                    XXST
     C                   kfld                    ELPFDT
     C*
     C     CIKY1         klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     c                   kfld                    EFTYPE
     c                   kfld                    EFDESC
     C                   kfld                    XXST
     C*
     C*
     C     *ENTRY        plist
     C                   parm                    @@CO#
     C                   parm                    @@FND
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     C                   parm                    @@AsOf                                 loss as of d
     C                   parm                    @@PoDt                                 loss as of d
     C                   parm                    @@SSF

     C*
     C                   eval      sysDt  = SystemDate
     C                   eval      sysDt7 = DateTo7(sysDt)
     C                   eval      sysDt8 = DateTo8(sysDt)

     C                   MOVE      @@Co#         xxCo#
     C                   MOVE      @@Fnd         xxFnd
     C                   MOVE      @@Emp#        xxEmp#
     C                   MOVE      @@Div         xxDiv
     C                   MOVE      @@AsOf        x8AsOf
     C                   MOVE      @@PoDt        x8PoDt
     C*
     C                   eval      x7AsOf = DtCYMD(x8AsOf)
     C                   eval      xxAsOf = CvtToDate(x7AsOf)
     C*
     C                   eval      x7PoDt = DtCYMD(x8PoDt)
     C                   eval      xxPoDt = CvtToDate(x7PoDt)
     C*
     C                   eval      singleEmp = xxEmp# <> *zero

     C                   Z-ADD     *ZEROS        X                 3 0
     C
     C                   select
     C                   when      singleEmp
     C                   exsr      ProcessEmp
     C                   other
     C*  Read through group select file
     C     xxCo#         setll     wxgsl
     C                   dou       %eof(wxgsl)
     C     xxCo#         reade     wxgsl
     C                   if        not %eof and GSIEFL = 'I'
     C                   exsr      ProcessGrp
     C                   endif
     C                   enddo
     C                   endsl                                              @@Emp# = ' '
     C*
     C                   MOVE      '1'           *INLR
     C*
      *===============================================================
      * Process records for a single employer
      *===============================================================
     C     ProcessEmp    begsr
     C*
     C* Use Employer Fee Information a driver file
     C     empWDEFL1     setll     wdefl1
     C                   dou       %eof(wdefl1)
     C     empWDEFL1     reade     wdefl1
     C                   if        %eof
     C                   iter
     C                   endif
     C                   exsr      ProcessClm
     C                   enddo
     C*
     C                   endsr
     C*
      *===============================================================
      * Process records for a single group
      *===============================================================
     C     ProcessGrp    begsr
     C*
     C* Use Employer Fee Information a driver file
     C     fndWDEFL1     setll     wdefl1
     C                   dou       %eof(wdefl1)
     C     fndWDEFL1     reade     wdefl1
     C                   if        %eof
     C                   iter
     C                   endif
     C                   exsr      ProcessClm
     C                   enddo
     C*
     C                   endsr
     C*
      *===============================================================
      * Process claim records
      *===============================================================
     C     ProcessClm    begsr
     C*
     C*  Get claim information
     C     keyWDFCL5     setll     wdfcl5
     C                   dou       %eof(wdfcl5)
     C     keyWDFCL5     reade     wdfcl5
     C                   if        %eof(wdfcl5)
     C                   iter
     C                   endif
     C                   exsr      WriteWorkFile
     C                   enddo
     C*
     C                   endsr
     C*
      *===============================================================
      * Write information to work file
      *===============================================================
     C     WriteWorkFile begsr
     C*
     c*** begin chg ***  05/09/11  ********************************************
     C*****keyWMCML      chain     wmcml
     C*****keyWMCDL      chain     wmcdl
     c                   if        fccase <> 0
     C     keyWMCML      chain     wmcml
     C     keyWMCDL      chain     wmcdl
     c                   else
     c                   clear                   pcm00
     c                   clear                   pcd00
     c                   endif
     c*** end chg *****  05/09/11  ********************************************
     C     keyWMFNL      chain     wmfnl
     C     keyWMEML      chain     wmeml
     C     keyWDELL      chain     wdell
     C*
     c                   eval      xxRtCd = ELRTCD
     C                   clear                   RSJRST
     C     RSKY1         CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        XXST              2
     C*
     C     CIKY          SETGT     PFI01                                           WTFIP
     C     CIKY1         READPE    PFI01                                  60
     C                   DOW       *IN60 =*OFF
     C
     C                   IF        FIEXDT = 0  OR  FIEXDT>ELPFDT
     C                   LEAVE
     C                   ENDIF
     C
     C     CIKY1         READPE    PFI01                                  60
     C                   ENDDO
     C*
     c                   if        FIAOEL <> 'Y'
     C                   eval      EFINBT = FIINBT
     C                   eval      EFINPN = FIINPN
     C                   eval      EFINFF = FIINFF
     C                   eval      EFMOBT = FIMOBT
     C                   eval      EFMOPN = FIMOPN
     C                   eval      EFMOFF = FIMOFF
     C                   eval      EFROBT = FIROBT
     C                   eval      EFROPN = FIROPN
     C                   eval      EFROFF = FIROFF
     C                   eval      EFALBT = FIALBT
     C                   eval      EFALPN = FIALPN
     C                   eval      EFALFF = FIALFF
     c                   endif
     C*
     c                   if        FICAOE <> 'Y'
     C                   eval      EFCOBS = FICOBB
     C                   eval      EFCOMA = FIMNCA
     C                   eval      EFCOPN = FICOPN
     C                   eval      EFCOFF = FICOFF
     c                   endif
     C*
     C                   callp     GetPriorRcd(xxAsOf)
     C                   callp     LoadRsvInfo
     c*** begin add ***  05/09/11  ********************************************
     C                   clear                   xfd0p
     c*** end add *****  05/09/11  ********************************************
     C*
      /free
       eval FDCO#  = FCCO#;
       eval FDFND  = FCFND;
       eval FDFNAM = FNNAME;
       eval FDENAM = EMNAME;
       eval FDENM2 = EMNAM2;
       eval FDEMP# = FCEMP#;
       eval FDDIV  = FCDIV;
       eval FDFYR  = FCFYR;
       eval FDTYPE = EFTYPE;
       eval FDDESC = EFDESC;
       eval FDBT   = FCBT;
       eval FDCAP# = GetCarrPol(FCCO#:FCFND:FCEMP#:FCDIV:FCFYR);
       eval FDCASE = FCCASE;
       //** begin add ***  05/09/11  ********************************************
       if fccase <> 0;
       //** end add *****  05/09/11  ********************************************
          eval FDCNAM = GetClmntName;
          select;
          when @@SSF = 'N';
           eval FDSS# = '*********';
          when @@SSF = '4';
           eval FDSS# = '*****' + %subst(CMSS#:6:4);
          other;
           eval FDSS# = CMSS#;
          endsl;
          eval FDOCCD = CMOCCD;
          eval FDOCDS = CMOCDS;
          eval FDACDT = DtMDYY(CMACDT);
          eval FDDTRP = DtMDYY(CMDTRP);
          eval FDSTAT = CMSTAT;
       //** begin add ***  05/09/11  ********************************************
       endif;
       //** end add *****  05/09/11  ********************************************
       select;
       when EFTYPE = 'GEN' and EFCOFF = *zero;
        eval FDMINF = %editc(EFCOMA:'J');
       other;
        eval FDMINF = 'N/A';
       endsl;
       if FDTYPE = 'CLF';
        select;
        when FDPRBL = *zero;
         eval FDASTS = 'N';
        other;
         eval FDASTS = 'C';
        endsl;
       endif;
       //** begin add ***  05/09/11  ********************************************
       if fccase <> 0;
       //** end add *****  05/09/11  ********************************************
         eval FDCINC = compInfo.incurred;
         eval FDCPD  = compInfo.paid;
         eval FDMINC = medInfo.incurred;
         eval FDMPD  = medInfo.paid;
         eval FDEINC = expInfo.incurred;
         eval FDEPD  = expInfo.paid;
         eval FDTINC = totInfo.incurred;
         eval FDTPD  = totInfo.paid;
         eval FDCLEX = CMCLEX;
         eval FDAJNM = GetStaffName(FDCLEX);
         eval FDSUPV = CMSUPR;
         eval FDSVNM = GetStaffName(FDSUPV);
         eval FDMNGR = CMUMAN;
         eval FDMNNM = GetStaffName(FDMNGR);
       //** begin add ***  05/09/11  ********************************************
       endif;
       //** end add *****  05/09/11  ********************************************
       eval FDUWTR = EMUWTR;
       eval FDUWNM = GetStaffName(FDUWTR);
       eval FDAGT# = GetEmpAgt(FCCO#:FCFND:FCEMP#:FCDIV:FCFYR:0:xxAgNm:ahRRN);
       eval FDAGNM = xxAgNm;
       eval FDPROD = GetEmpProd(ahRRN:xxPrNm);
       eval FDPRNM = xxPrNm;

       eval FDPRBL = inWMFHL.FDCUBL;
       eval FDPRPR = inWMFHL.FDCUPR;
       eval FDPRPY = inWMFHL.FDCUPY;
       eval FDPRTY = inWMFHL.FDCUTY;
       eval FDPRDT = inWMFHL.FDCUDT;

       select;
       when EFTYPE = 'CLF';
        eval FDCUBL = FCCFCB;
       when EFTYPE = 'GEN';
        eval FDCUBL = FCCOFB;
       endsl;
       eval FDCUPR = GetPremium(xxAsOf);
       eval FDCUPY = GetPayroll(xxAsOf);
       eval FDCUTY = GetIMR(xxAsOf);
       eval FDCUDT = DateTo8(xxAsOf);

       eval FDCPCT = EFCOPN;

       eval FDASOF = DateTo8(xxAsOf);
       eval FDIVDT = DateTo8(xxPoDt);
       eval FDPGM  = q1Pgm;
       eval FDUSER = q1User;
       eval FDDT   = sysDt8;
       eval FDTIME = TimeTo6(SystemTime);
      /end-free
     C*
     C                   write     xfd0p
     C*
     C                   endsr
     C*





     P*====================================================================
     P* Get claimant name
     P*====================================================================
     PGetClmntName     b
     DGetClmntName     pi                  like(FDCNAM)

     D##RtCd           s              1
     D*
     DChkClaimAuth     pr                  extPgm('WA067R')
     D inCo#                          3    const
     D inFnd                          3    const
     D inFyr                          3    const
     D inCase                         7    const
     D inUser                        10    const
     D inRtCd                         1

     c*** begin add ***  05/09/11  *********************************************
     c                   if        fccase = 0
     c                   return    *blanks
     c                   endif
     c*** end add *****  05/09/11  *********************************************
      /free
       // Call claims security API to check claim authority
       callp ChkClaimAuth(%editc(CMCO#:'X'):%editc(CMFND:'X'):
                          %editc(CMFYR:'X'):%editc(CMCASE:'X'):
                          q1User:##RtCd);
       select;
       when ##RtCd = '2';
        return '*** NO AUTHORITY ***';
       other;
        return %trim(CDLSN) + ', ' + %trim(CDFSN);
       endsl;
      /end-free

     PGetClmntName     e





     P*====================================================================
     P* Get employer producer
     P*====================================================================
     PGetEmpProd       b
     DGetEmpProd       pi                  like(FDPROD)
     D inRRN                               like(rrn)
     D outName                             like(FDPRNM)

     DxxFein           s              9  0
     DxxMod            s              3  0

     C                   clear                   AHCO#
     C                   clear                   AHFND
     C                   clear                   AHAGT#
     C                   clear                   AHPROD
     C     inRRN         chain     wmahp
      /free
       eval xxFein = GetAgtFein(AHCO#:AHFND:AHAGT#:xxMod);
       eval outName = GetProdNm(xxFein:xxMod:AHPROD:rrn:AHCO#);
       return AHPROD;
      /end-free

     PGetEmpProd       e





     P*====================================================================
     P* Get I/M/R status
     P*====================================================================
     PGetIMR           b
     DGetIMR           pi                  like(FDCUTY)
     D asOfDt                          d


     c*** begin add ***  05/09/11  *********************************************
     c                   if        fccase = 0
     c                   return    *blanks
     c                   endif
     c*** end add *****  05/09/11  *********************************************
      /free

       eval keyWMTHL.THCO#  = CMCO#;
       eval keyWMTHL.THFND  = CMFND;
       eval keyWMTHL.THFYR  = CMFYR ;
       eval keyWMTHL.THCASE = CMCASE;
       eval keyWMTHL.THTRDT = DateTo7(asOfDt);
       eval keyWMTHL.THTIME = *hival;

       eval key1WMTHL.THCO#  = CMCO#;
       eval key1WMTHL.THFND  = CMFND;
       eval key1WMTHL.THFYR  = CMFYR;
       eval key1WMTHL.THCASE = CMCASE;

       //Chain to file loads the fields to inWMTHL;
       clear inWMTHL;
       setgt %kds(keyWMTHL) pth00;
       readpe %kds(key1WMTHL) pth00 inWMTHL;
       return inWMTHL.THTO;

      /end-free

     PGetIMR           e





     P*====================================================================
     P* Get payroll
     P*====================================================================
     PGetPayroll       b
     DGetPayroll       pi                  like(FDCUPY)
     D asOfDt                          d

     Dpayroll          s                   like(EPAAPR) inz

      /free

       eval keyWMEPL.EPCO#  = FCCO#;
       eval keyWMEPL.EPFND  = FCFND;
       eval keyWMEPL.EPEMP# = FCEMP#;
       eval keyWMEPL.EPDIV  = FCDIV;
       eval keyWMEPL.EPFYR  = FCFYR ;

       //Chain to file loads the fields to inWMEPL;
       clear inWMEPL;
       setll %kds(keyWMEPL) pep00;
       dou %eof;
        reade %kds(keyWMEPL) pep00 inWMEPL;
        if not %eof;
         if ELFAF = 'A';
          payroll += inWMEPL.EPAAPR;
         else;
          payroll += inWMEPL.EPEAPR;
         endif;
        endif;
       enddo;
       return payroll;

      /end-free

     PGetPayroll       e





     P*====================================================================
     P* Get premium
     P*====================================================================
     PGetPremium       b
     DGetPremium       pi                  like(FDCUPR)
     D asOfDt                          d


      /free

       eval keyWDEHL3.EHCO#  = FCCO#;
       eval keyWDEHL3.EHFND  = FCFND;
       eval keyWDEHL3.EHEMP# = FCEMP#;
       eval keyWDEHL3.EHDIV  = FCDIV;
       eval keyWDEHL3.EHFYR  = FCFYR ;
       eval keyWDEHL3.EHEFDT = DateTo7(asOfDt);

       eval key1WDEHL3.EHCO#  = FCCO#;
       eval key1WDEHL3.EHFND  = FCFND;
       eval key1WDEHL3.EHEMP# = FCEMP#;
       eval key1WDEHL3.EHDIV  = FCDIV;
       eval key1WDEHL3.EHFYR  = FCFYR;

       //Chain to file loads the fields to inWDEHL3;
       clear inWDEHL3;
       setgt %kds(keyWDEHL3) peh03;
       readpe %kds(key1WDEHL3) peh03 inWDEHL3;
       return inWDEHL3.EHWAPR;
       return inWDEHL3.EHIAPR;

      /end-free

     PGetPremium       e





     P*====================================================================
     P* Get prior history date
     P*====================================================================
     PGetPriorRcd      b
     DGetPriorRcd      pi
     D asOfDt                          d

      /free
       eval keyWMFHL.FDCO#  = FCCO#;
       eval keyWMFHL.FDFND  = FCFND;
       eval keyWMFHL.FDEMP# = FCEMP#;
       eval keyWMFHL.FDDIV  = FCDIV;
       eval keyWMFHL.FDFYR  = FCFYR;
       eval keyWMFHL.FDCASE = FCCASE;
       eval keyWMFHL.FDCUDT = DateTo7(asOfDt);

       eval key1WMFHL.FDCO#  = FCCO#;
       eval key1WMFHL.FDFND  = FCFND;
       eval key1WMFHL.FDEMP# = FCEMP#;
       eval key1WMFHL.FDDIV  = FCDIV;
       eval key1WMFHL.FDFYR  = FCFYR;
       eval key1WMFHL.FDCASE = FCCASE;

       //Chain to file loads the fields to inWMFHL;
       clear inWMFHL;
       setll %kds(keyWMFHL) pfh00;
       readpe %kds(key1WMFHL) pfh00 inWMFHL;
      /end-free

     PGetPriorRcd      e





     P*====================================================================
     P* Get staff name from staff code
     P*====================================================================
     PGetStaffName     b
     DGetStaffName     pi                  like(UPUSNM)
     D inStaffCode                         like(UPSTAF)

     C                   clear                   UPUSNM
     C     inStaffCode   chain     wmupl1
     C                   return    UPUSNM

     PGetStaffName     e





     P*====================================================================
     P* Load reserve info
     P*====================================================================
     PLoadRsvInfo      b
     DLoadRsvInfo      pi

     DxxKey            s                   like(RTKEY)
     Di                s              3  0

     C     keyWTRTL1     klist
     C                   kfld                    FCCO#
     C                   kfld                    xxKey

     C                   clear                   compInfo
     C                   clear                   medInfo
     C                   clear                   expInfo
     C                   clear                   totInfo
     C*
     C     1             do        #ofReserves   i
     C                   eval      xxKey = reserveType(i)
     C                   clear                   RTTYPE
     C     keyWTRTL1     chain     wtrtl1
      /free
       select;
       when RTTYPE = 'C';
        eval compInfo.type = 'C';
        eval compInfo.paid += reservePd(i);
        eval compInfo.incurred += reserveRmn(i) + reservePd(i);
       when RTTYPE = 'M';
        eval medInfo.type = 'C';
        eval medInfo.paid += reservePd(i);
        eval medInfo.incurred += reserveRmn(i) + reservePd(i);
       when RTTYPE = 'E';
        eval expInfo.type = 'E';
        eval expInfo.paid += reservePd(i);
        eval expInfo.incurred += reserveRmn(i) + reservePd(i);
       endsl;
      /end-free
     C                   enddo

      /free
        eval totInfo.type = '*';
        eval totInfo.paid = %xfoot(reservePd);
        eval totInfo.incurred = %xfoot(reserveRmn) + %xfoot(reservePd);
      /end-free

     PLoadRsvInfo      e
